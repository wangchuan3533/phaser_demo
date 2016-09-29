/*
 * libwebsockets-test-server for libev - libwebsockets test implementation
 *
 * Copyright (C) 2010-2015 Andy Green <andy@warmcat.com>
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation:
 *  version 2.1 of the License.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *  MA  02110-1301  USA
 */

#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>
#include <signal.h>
#include <string.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <assert.h>
#include <syslog.h>
#include <sys/time.h>
#include <unistd.h>

#include <uv.h>
#include <lws_config.h>
#include <libwebsockets.h>

int close_testing;
int max_poll_elements;
int debug_level = 7;
struct lws_context *context;
struct lws_plat_file_ops fops_plat;

/* http server gets files from this path */
#define LOCAL_RESOURCE_PATH "resource"
const char *resource_path = LOCAL_RESOURCE_PATH;
char crl_path[1024] = "";
const char *http_response = "HTTP/1.1 200 OK\r\nServer: phaser_demo\r\nContent-Length: 5\r\n\r\nhello";
char broadcast_message[128];
typedef struct session_data {
    char nickname[64];
    struct lws *wsi;
    unsigned char recv_buf[1024];
    unsigned int recv_len;
    unsigned char send_buf[1024];
    unsigned int send_len;
} session_data_t;

int callback_http(struct lws *wsi, enum lws_callback_reasons reason, void *user, void *in, size_t len)
{
    session_data_t *session = (session_data_t *)user;
    int n;
    
    switch (reason) {
    case LWS_CALLBACK_HTTP:
        lwsl_notice("http request\n");
        n = lws_write(wsi, (unsigned char *)http_response, strlen(http_response), LWS_WRITE_HTTP);
        lws_callback_on_writable(wsi);
        break;
    case LWS_CALLBACK_FILTER_PROTOCOL_CONNECTION:
        lwsl_notice("protocol filter\n");
        lws_hdr_copy(wsi, session->nickname, sizeof(session->nickname), WSI_TOKEN_GET_URI);
        lwsl_notice("nickname is %s", session->nickname);
        break;
    case LWS_CALLBACK_ESTABLISHED:
        session->wsi = wsi;
        session->recv_len = 0;
        lwsl_notice("client connected\n");
        break;
    case LWS_CALLBACK_RECEIVE:
        lwsl_notice("received %s\n", (const char *)in);
        memcpy(session->recv_buf, in, len);
        session->recv_len = len;
        session->recv_buf[len] = '\0';
        snprintf(broadcast_message, sizeof(broadcast_message), "%s:%s", session->nickname, (char *)session->recv_buf);
        lws_callback_on_writable_all_protocol(context, lws_get_protocol(wsi));
        lws_callback_on_writable(wsi);
        break;
    case LWS_CALLBACK_SERVER_WRITEABLE:
        lwsl_notice("writable\n");
        n = lws_write(wsi, (unsigned char *)broadcast_message, strlen(broadcast_message), LWS_WRITE_TEXT);
        //n = lws_write(wsi, session->recv_buf, session->recv_len, LWS_WRITE_TEXT);
        if (n < 0) {
            lwsl_err("error writing %d\n", n);
            return 1;
        } else if (n < (int)session->recv_len) {
            lwsl_err("partial write %d %d\n", session->recv_len, n);
            return -1;
        }
        break;
    default:
        lwsl_notice("callback_http unprocessed reason: %d\n", reason);
        break;
    }
    return 0;
}


static struct lws_protocols protocols[] = {
    {
        "",                                        /* name */
        callback_http,                             /* callback */
        sizeof(session_data_t),     /* per_session_data_size */
        0,                                         /* max frame size / rx buffer */
    },
    { NULL, NULL, 0, 0 } /* terminator */
};

void signal_cb(uv_signal_t *watcher, int signum)
{
    lwsl_err("Signal %d caught, exiting...\n", watcher->signum);
    switch (watcher->signum) {
    case SIGTERM:
    case SIGINT:
        break;
    default:
        signal(SIGABRT, SIG_DFL);
        abort();
        break;
    }
    lws_libuv_stop(context);
}

static struct option options[] = {
    { "help",    no_argument,        NULL, 'h' },
    { "debug",    required_argument,    NULL, 'd' },
    { "port",    required_argument,    NULL, 'p' },
    { "ssl",    no_argument,        NULL, 's' },
    { "allow-non-ssl",    no_argument,    NULL, 'a' },
    { "interface",  required_argument,    NULL, 'i' },
    { "closetest",  no_argument,        NULL, 'c' },
    { "libev",  no_argument,        NULL, 'e' },
    { "foreign",  no_argument,        NULL, 'f' },
    { "daemonize",     no_argument,        NULL, 'D' },
    { "resource_path", required_argument,    NULL, 'r' },
    { NULL, 0, 0, 0 }
};

/* ----- this code is only needed for foreign / external libuv tests -----*/
struct counter
{
    int cur, lim;
    int stop_loop;
};

static void timer_cb(uv_timer_t *t)
{
    struct counter *c = (struct counter *)t->data;

    lwsl_debug("  timer %p cb, count %d, loop has %d handles\n",
            t, c->cur, t->loop->active_handles);

    if (c->cur++ == c->lim) {
        lwsl_debug("stop loop from timer\n");
        uv_timer_stop(t);
        if (c->stop_loop)
            uv_stop(t->loop);
    }
}

static void timer_close_cb(uv_handle_t *h)
{
    lwsl_debug("timer close cb %p, loop has %d handles\n",
            h, h->loop->active_handles);
}

void outer_signal_cb(uv_signal_t *s, int signum)
{
    lwsl_notice("Foreign loop got signal %d\n", signum);
    uv_signal_stop(s);
    uv_stop(s->loop);
}

static void lws_uv_close_cb(uv_handle_t *handle)
{
    //lwsl_err("%s\n", __func__);
}

static void lws_uv_walk_cb(uv_handle_t *handle, void *arg)
{
    uv_close(handle, lws_uv_close_cb);
}


int main(int argc, char **argv)
{
    struct lws_context_creation_info info;
    char interface_name[128] = "";
    uv_loop_t loop;
    uv_signal_t signal_outer;
    uv_timer_t timer_outer;
    struct counter ctr;
    int foreign_libuv_loop = 0;
    const char *iface = NULL;
    char cert_path[1024];
    char key_path[1024];
    int use_ssl = 0;
    int opts = 0;
    int n = 0;
    int syslog_options = LOG_PID | LOG_PERROR;
    int daemonize = 0;

    /*
     * take care to zero down the info struct, he contains random garbaage
     * from the stack otherwise
     */
    memset(&info, 0, sizeof info);
    info.port = 7681;

    while (n >= 0) {
        n = getopt_long(argc, argv, "feci:hsap:d:Dr:", options, NULL);
        if (n < 0)
            continue;
        switch (n) {
        case 'f':
            foreign_libuv_loop = 1;
            break;
        case 'e':
            opts |= LWS_SERVER_OPTION_LIBEV;
            break;
        case 'D':
            daemonize = 1;
            #ifndef _WIN32
            syslog_options &= ~LOG_PERROR;
            #endif
            break;
        case 'd':
            debug_level = atoi(optarg);
            break;
        case 's':
            use_ssl = 1;
            break;
        case 'a':
            opts |= LWS_SERVER_OPTION_ALLOW_NON_SSL_ON_SSL_PORT;
            break;
        case 'p':
            info.port = atoi(optarg);
            break;
        case 'i':
            strncpy(interface_name, optarg, sizeof interface_name);
            interface_name[(sizeof interface_name) - 1] = '\0';
            iface = interface_name;
            break;
        case 'c':
            close_testing = 1;
            fprintf(stderr, " Close testing mode -- closes on "
                       "client after 50 dumb increments"
                       "and suppresses lws_mirror spam\n");
            break;
        case 'r':
            resource_path = optarg;
            printf("Setting resource path to \"%s\"\n", resource_path);
            break;
        case 'h':
            fprintf(stderr, "Usage: test-server "
                    "[--port=<p>] [--ssl] "
                    "[-d <log bitfield>] "
                    "[--resource_path <path>]\n");
            exit(1);
        }
    }

    /*
     * normally lock path would be /var/lock/lwsts or similar, to
     * simplify getting started without having to take care about
     * permissions or running as root, set to /tmp/.lwsts-lock
     */
    if (daemonize && lws_daemonize("/tmp/.lwsts-lock")) {
        fprintf(stderr, "Failed to daemonize\n");
        return 1;
    }

    /* we will only try to log things according to our debug_level */
    setlogmask(LOG_UPTO (LOG_DEBUG));
    openlog("lwsts", syslog_options, LOG_DAEMON);

    /* tell the library what debug level to emit and to send it to syslog */
    lws_set_log_level(debug_level, lwsl_emit_syslog);

    lwsl_notice("libwebsockets test server libuv - license LGPL2.1+SLE\n");
    lwsl_notice("(C) Copyright 2010-2016 Andy Green <andy@warmcat.com>\n");

    lwsl_info("Using resource path \"%s\"\n", resource_path);

    info.iface = iface;
    info.protocols = protocols;
    info.extensions = NULL;

    info.ssl_cert_filepath = NULL;
    info.ssl_private_key_filepath = NULL;

    if (use_ssl) {
        if (strlen(resource_path) > sizeof(cert_path) - 32) {
            lwsl_err("resource path too long\n");
            return -1;
        }
        sprintf(cert_path, "%s/libwebsockets-test-server.pem",
            resource_path);
        if (strlen(resource_path) > sizeof(key_path) - 32) {
            lwsl_err("resource path too long\n");
            return -1;
        }
        sprintf(key_path, "%s/libwebsockets-test-server.key.pem",
            resource_path);

        info.ssl_cert_filepath = cert_path;
        info.ssl_private_key_filepath = key_path;
        opts |= LWS_SERVER_OPTION_DO_SSL_GLOBAL_INIT;
    }
    info.gid = -1;
    info.uid = -1;
    info.max_http_header_pool = 1;
    info.timeout_secs = 5;
    info.options = opts | LWS_SERVER_OPTION_LIBUV;

    /* create the foreign loop */
    uv_loop_init(&loop);
    uv_signal_init(&loop, &signal_outer);
    uv_signal_start(&signal_outer, outer_signal_cb, SIGINT);
    
    uv_timer_init(&loop, &timer_outer);
    timer_outer.data = &ctr;
    ctr.cur = 0;
    ctr.lim = ctr.cur + 5;
    ctr.stop_loop = 1;
    uv_timer_start(&timer_outer, timer_cb, 0, 1000);
    lwsl_notice("running loop without libwebsockets for %d s\n", ctr.lim);

    uv_run(&loop, UV_RUN_DEFAULT);

    context = lws_create_context(&info);
    if (context == NULL) {
        lwsl_err("libwebsocket init failed\n");
        return -1;
    }
    lws_uv_sigint_cfg(context, 1, signal_cb);
    lws_uv_initloop(context, &loop, 0);

    /*
     * prepare inner timer on loop, to run along with lws.
     * Will exit after 5s while lws keeps running
     */
    struct counter ctr_inner = { 0, 3, 0 };
    int e;
    uv_timer_t timer_inner;
    uv_timer_init(&loop, &timer_inner);
    timer_inner.data = &ctr_inner;
    uv_timer_start(&timer_inner, timer_cb, 200, 1000);

    /* make this timer long-lived, should keep
     * firing after lws exits */
    ctr.cur = 0;
    ctr.lim = ctr.cur + 1000;
    uv_timer_start(&timer_outer, timer_cb, 0, 1000);

    uv_run(&loop, UV_RUN_DEFAULT);

    /* we are here either because signal stopped us,
     * or outer timer expired */

    /* close short timer */
    uv_timer_stop(&timer_inner);
    uv_close((uv_handle_t*)&timer_inner, timer_close_cb);

    lwsl_notice("Destroying lws context\n");

    /* detach lws */
    lws_context_destroy(context);

    lwsl_notice("Please wait while the outer libuv test continues for 10s\n");

    ctr.lim = ctr.cur + 10;

    /* try and run outer timer for 10 more seconds,
     * (or sigint outer handler) after lws has left the loop */
    uv_run(&loop, UV_RUN_DEFAULT);

    /* Clean up the foreign loop now */

    /* PHASE 1: stop and close things we created
     *          outside of lws */

    uv_timer_stop(&timer_outer);
    uv_close((uv_handle_t*)&timer_outer, timer_close_cb);
    uv_signal_stop(&signal_outer);

    e = 100;
    while (e--)
        uv_run(&loop, UV_RUN_NOWAIT);

    /* PHASE 2: close anything remaining */

    uv_walk(&loop, lws_uv_walk_cb, NULL);

    e = 100;
    while (e--)
        uv_run(&loop, UV_RUN_NOWAIT);

    /* PHASE 3: close the UV loop itself */

    e = uv_loop_close(&loop);
    lwsl_notice("uv loop close rc %s\n", e ? uv_strerror(e) : "ok");
    lwsl_notice("libwebsockets-test-server exited cleanly\n");

    return 0;
}
