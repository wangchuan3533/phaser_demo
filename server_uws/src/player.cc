#include "common.h"
#include "player.h"

void player::toEntity(Entity *entity)
{
    entity->set_id(_player_id);
    entity->set_index(_index);
    entity->set_offset(_offset);
}
