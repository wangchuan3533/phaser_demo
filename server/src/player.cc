#include "common.h"
#include "player.h"

void player::toEntity(demo::protocol::Entity *entity)
{
    entity->set_id(_player_id);
    entity->set_index(_index);
    entity->set_offset(_offset);
    entity->set_last_action_id(_last_action_id);
}
