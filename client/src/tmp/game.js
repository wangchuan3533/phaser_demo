var pomelo = window.pomelo;
var host = "127.0.0.1";
var port = "3011";
var uid = 0;
var game;
var nickname;
var playerEntityId;

var entities = {};
var dots = {};
var updates = [];
var cursors;
var spaceKey;

var players;
var stars;
var score = 0;
var scoreText;
var size = 40;
var latency;
var actionId = 0;

var EntityType = {
  ENTITY: 0,
  PLAYER: 1,
  GHOST: 2,
  RUNE: 3,
  ICE: 4,
  BLACKHOLE: 5,
  HURRICANE: 6
};

var ActionType = {
  STAY: 0,
  LEFT: 1,
  RIGHT: 2,
  UP: 3,
  DOWN: 4,
  SPEED_UP: 5
};

var EntityStatus = {
  NORMAL: 0x0000,
  STRONG: 0x0001,
  SLOW:   0x0002,
  SHIELD: 0x0004,
  
  checkStatus: function(status1, status2) {
    return status1 & status2;
  }
};

var serverTime = 0;
var serverTick = 0;

var clientTime = 0;
var clientTick = 0;
var clientInterval = 0;

setInterval(function() {
  console.log("latency is " + latency);
}, 2000);

var map = {
  init: function(data) {
    this.width = data.width;
    this.height = data.height;
    this.data = data.data;
  },
  intIsWall: function(x, y) {
    var index, tile;
    if (x < 0 || x >= this.width || y < 0 || y >= this.height) {
      return true;
    }
    index = y * this.width + x;
    tile = this.data[index];
    return tile === '|' || tile === '_';
  },
  isWall: function(x, y) {
    var ceil, floor;
    if (x === parseInt(x) && y === parseInt(y)) {
      return this.intIsWall(x, y);
    } else if (x === parseInt(x)) {
      floor = Math.floor(y);
      ceil = Math.ceil(y);
      return this.intIsWall(x, floor) || this.intIsWall(x, ceil);
    } else if (y === parseInt(y)) {
      floor = Math.floor(x);
      ceil = Math.ceil(x);
      return this.intIsWall(floor, y) || this.intIsWall(ceil, y);
    } else {
      return true;
    }
  },
};

function loadMap(cb) {
  $.ajax({url: '/map/64.json'}).done(function(data) {
    map.init(data);
    cb(null);
  });
}


function connect() {
  pomelo.init({
    host: host,
    port: port,
    log: true
  }, function() {
    pomelo.request("connector.entryHandler.entry", {nickname}, function(data) {
      console.log(JSON.stringify(data));
      uid = data.uid;
      serverTime = data.serverTime;
      serverTick = data.tick;
      
      for (var i = 0, len = data.entities.length; i < len; i++) {
        var entity = data.entities[i];
        var entityId = entity.id;
        if (entity.uid == uid) {
          playerEntityId = entityId;
        }
        entities[entityId] = createEntity(entity.type, entity.x, entity.y);
        entities[entityId].entityId = entityId;
        entities[entityId].uid = entity.uid;
      }
      
      for (var i = 0, len = data.dots.length; i < len; i++) {
        var dot = data.dots[i];
        dots[dot.x] = dots[dot.x] || {};
        dots[dot.x][dot.y] = createEntity(EntityType.STAR, dot.x, dot.y);
      }
    });
  });
}

function notifyAction(action) {
  console.log("action: " + action);
  var sendtime = Date.now();
  pomelo.request("logic.entryHandler.control", {action, serverTime, actionId: actionId++, tick: serverTick}, function (response) {
    var recvtime = Date.now();
    latency = recvtime - sendtime;
  });
}


function preload() {
    game.load.image('sky', 'assets/sky.png');
    game.load.image('star', 'assets/star.png');
    game.load.image('diamond', 'assets/diamond.png');
    game.load.image('ice', 'assets/ice.png');
    game.load.image('blackhole', 'assets/blackhole.png');
    game.load.image('hurricane', 'assets/hurricane.png');
    game.load.spritesheet('dude', 'assets/dude.png', 32, 48);
    game.load.spritesheet('baddie', 'assets/baddie.png', 32, 32);
    
    require('boot');
    connect();
}

function createEntity(type, x, y) {
  if (type == EntityType.PLAYER) {
    return createPlayer(x, y);
  } else if (type == EntityType.GHOST) {
    return createGhost(x, y);
  } else if (type == EntityType.RUNE) {
    return createRune(x, y);
  } else if (type == EntityType.STAR) {
    return createStar(x, y);
  } else if (type == EntityType.ICE) {
    return createIce(x, y);
  } else if (type == EntityType.BLACKHOLE) {
    return createBlackhole(x, y);
  } else if (type == EntityType.HURRICANE) {
    return createHurricane(x, y);
  }
}

function createPlayer(x, y) {
  
  // The player and its settings
  var player = players.create(x * size, y * size, 'dude');
  game.physics.arcade.enable(player);
  player.animations.add('left', [0, 1, 2, 3], 10, true);
  player.animations.add('right', [5, 6, 7, 8], 10, true);
  player.origScale = 0.5;
  player.scale.setTo(player.origScale, player.origScale);
  
  return player;
}

function createGhost(x, y) {
  
  // The ghost and its settings
  var ghost = players.create(x * size, y * size, 'baddie');
  ghost.animations.add('left', [0, 1], 10, true);
  ghost.animations.add('right', [2, 3], 10, true);
  game.physics.arcade.enable(ghost);
  ghost.origScale = 0.5;
  ghost.scale.setTo(ghost.origScale, ghost.origScale);
  
  return ghost;
}

function createRune(x, y) {
  
  // The ghost and its settings
  var rune = players.create(x * size, y * size, 'diamond');
  game.physics.arcade.enable(rune);
  rune.origScale = 0.5;
  rune.scale.setTo(rune.origScale, rune.origScale);
  
  return rune;
}

function createIce(x, y) {
  
  // The ghost and its settings
  var ice = players.create(x * size, y * size, 'ice');
  game.physics.arcade.enable(ice);
  
  ice.origScale = 0.3;
  ice.scale.setTo(ice.origScale, ice.origScale);
  
  return ice;
}

function createBlackhole(x, y) {
  
  // The ghost and its settings
  var blackhole = players.create(x * size, y * size, 'blackhole');
  game.physics.arcade.enable(blackhole);
  blackhole.origScale = 0.2;
  blackhole.scale.setTo(blackhole.origScale, blackhole.origScale);
  
  return blackhole;
}
function createHurricane(x, y) {
  
  // The ghost and its settings
  var hurricane = players.create(x * size, y * size, 'hurricane');
  game.physics.arcade.enable(hurricane);
  hurricane.origScale = 0.2;
  hurricane.scale.setTo(hurricane.origScale, hurricane.origScale);
  
  return hurricane;
}

function createStar(x, y) {
  var star = stars.create((x + 0.25) * size, (y + 0.25) * size, 'star');
  star.origScale = 0.5;
  star.scale.setTo(star.origScale, star.origScale);
  
  return star;
}

function createWalls() {
  var graphics = game.add.graphics(0, 0);
  graphics.lineStyle(1, 0xffd900, 1);
  graphics.beginFill(0xFFFF0B, 1);
  
  for (var x = 0; x < map.width; x++) {
    for (var y = 0; y < map.height; y++) {
      if (map.isWall(x, y)) {
        graphics.drawRect(x * size + 2, y * size + 2, size - 4, size - 4);
        game.add.text(x * size + 5, y * size + 10, x + ',' + y, { fontSize: '10px', fill: 'red' });
      }
    }
  }
  graphics.endFill();
}

function create() {

    game.physics.startSystem(Phaser.Physics.ARCADE);
  
    createWalls();
    stars = game.add.group();
    stars.enableBody = true;
    players = game.add.group();
    scoreText = game.add.text(22 * size, 2 * size, 'LEADERBOARD', { fontSize: '15px', fill: 'red' });
    cursors = game.input.keyboard.createCursorKeys();
    spaceKey = game.input.keyboard.addKey(Phaser.Keyboard.SPACEBAR);
    
    pomelo.on('update', function (positions) {
      updates.push(positions);
    });
    
    pomelo.on('kill', function(msg) {
      if (msg.to == playerEntityId) {
        alert('you are killed by ' + msg.from);
      }
    });
    
    pomelo.on('scores', function (scores) {
      //console.log(JSON.stringify(scores));
      if (scores.scores) renderScores(scores.scores);
    });
}

function update() {
    clientTick++;
    
    var now = Date.now();
    
    clientInterval = now - clientTime;
    clientTime = now;

    //for (var i = 0, len = players.children.length; i < len; i++) {
    //  var player = players.children[i];
    //  game.physics.arcade.overlap(player, stars, collectStar, null, this);
    //}
    
    if (cursors.left.isDown) {
        notifyAction(ActionType.LEFT);
    } else if (cursors.right.isDown) {
        notifyAction(ActionType.RIGHT);
    } else if (cursors.up.isDown) {
        notifyAction(ActionType.UP);
    } else if (cursors.down.isDown) {
        notifyAction(ActionType.DOWN);
    } else if (spaceKey.isDown) {
        notifyAction(ActionType.SPEED_UP);
    }
    
    if (updates.length > 1) {
      // added
      var from = updates[0];
      var to = updates[1];
      
      serverTime = from.serverTime;
      serverTick = from.tick;
      //console.log(`clientTick = ${clientTick} serverTick = ${serverTick}`);
      
      if (from.added) {
        for (var i = 0, len = from.added.length; i < len; i++) {
          var entity = from.added[i];
          if (entity.uid == uid) {
            continue;
          }
          
          var entityId = entity.id;
          entities[entityId] = createEntity(entity.type, entity.x, entity.y);
          entities[entityId].entityId = entityId;
        }
      }
      
      // deleted
      if (from.deleted) {
        for (var i = 0, len = from.deleted.length; i < len; i++) {
          var entityId = from.deleted[i];
          var sprite = entities[entityId];
          if (sprite) {
            sprite.destroy();
          }
        }
      }
      
      // moved
      if (from.moved) {
        for (var i = 0, len = from.moved.length; i < len; i++) {
          var entity = from.moved[i];
          var entityId = entity.id;
          var sprite = entities[entityId];
          if (sprite) {
            sprite.x = entity.x * size;
            sprite.y = entity.y * size;
            // sprite.body.velocity.x = entity.dx * size;
            // sprite.body.velocity.y = entity.dy * size;
            
            if (entity.dx > 0 && (entity.type == EntityType.PLAYER || entity.type == EntityType.GHOST)) {
              sprite.animations.play('right');
            } else if (entity.dx < 0 && (entity.type == EntityType.PLAYER || entity.type == EntityType.GHOST)) {
              sprite.animations.play('left');
            } else if (entity.type == EntityType.PLAYER || entity.type == EntityType.GHOST){
              sprite.animations.stop();
              sprite.frame = 4;
            }
            // strong
            if (EntityStatus.checkStatus(entity.status, EntityStatus.STRONG)) {
              sprite.scale.setTo(sprite.origScale * 2, sprite.origScale * 2);
            }
            // slow
            if (EntityStatus.checkStatus(entity.status, EntityStatus.SLOW)) {
              sprite.scale.setTo(0.5 * sprite.origScale, 0.5 * sprite.origScale);
            }
            // shield
            if (EntityStatus.checkStatus(entity.status, EntityStatus.SHIELD)) {
              sprite.scale.setTo(4 * sprite.origScale, 4 * sprite.origScale);
            }
            
            // reset
            if (entity.status == 0) {
              sprite.scale.setTo(sprite.origScale, sprite.origScale);
            }
          }
        }
      }
      
      if (from.dotsAdded) {
        for (var i = 0, len = from.dotsAdded.length; i < len; i++) {
          var dot = from.dotsAdded[i];
          console.log(dot);
          console.log('added');
          dots[dot.x] = dots[dot.x] || {};
          if (!dots[dot.x][dot.y]) {
            dots[dot.x][dot.y] = createEntity(EntityType.STAR, dot.x, dot.y);
          }
        }
      }
      
      if (from.dotsDeleted) {
        for (var i = 0, len = from.dotsDeleted.length; i < len; i++) {
          var dot = from.dotsDeleted[i];
          var star = dots[dot.x] && dots[dot.x][dot.y];
          console.log(dot);
          console.log('deleted');
          if (star) {
            star.destroy();
            delete dots[dot.x][dot.y];
          }
        }
      }
      
      if (to.moved) {
        for (var i = 0, len = to.moved.length; i < len; i++) {
          var entity = to.moved[i];
          var entityId = entity.id;
          var sprite = entities[entityId];
          if (sprite) {
            sprite.next_x = entity.x * size;
            sprite.next_y = entity.y * size;
            sprite.leftTime = to.serverTime - from.serverTime;
          }
        }
      }
      
      updates.shift();
    } else {
      for (var i = 0, len = players.children.length; i < len; i++) {
        var player = players.children[i];
        if (player.next_x && player.leftTime > 0) {
          var dx = (player.next_x - player.x) * clientInterval / player.leftTime;
          var dy = (player.next_y - player.y) * clientInterval / player.leftTime;
          player.x += dx;
          player.y += dy;
          player.leftTime -= clientInterval;
        }
      }
    }
}

function collectStar (player, star) {
    
    star.kill();

    if (player.uid == uid) {
      //score += 10;
      //scoreText.text = 'score: ' + score;
      //notifyAction('eat', star.entityId);
    }
}

function renderScores(scores) {
  var text = 'LEADERBOARD\n';
  for (var i = 0; i < scores.length; i++) {
    text += scores[i].nickname + ': ' + scores[i].score + '\n'
  }
  scoreText.text = text;
}

$(function() {
  $('#login-modal').modal('show');
  $('#login').on('click', function () {
    nickname = $('#name').val();
    $('#login-modal').modal('hide');
    loadMap(function(err) {
      if (err) {
        return alert(err);
      }
      game = new Phaser.Game(parseInt(map.width * size), parseInt(map.height * size), Phaser.AUTO, '', { preload: preload, create: create, update: update });
      
    });
  });
})
