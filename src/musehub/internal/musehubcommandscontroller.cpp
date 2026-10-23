/*
* Audacity: A Digital Audio Editor
*/
#include "musehubcommandscontroller.h"

#include "au3-musehub/MuseHubService.h"

#include "../musehubcommands.h"

using namespace au::musehub;
using namespace muse;
using namespace muse::rcommand;

void MuseHubCommandsController::init()
{
    commandDispatcher()->onRequest(this, GET_EFFECTS_COMMAND, [this]() { return getEffects(); });
}

void MuseHubCommandsController::deinit()
{
    commandDispatcher()->unreg(this);
}

Ret MuseHubCommandsController::getEffects()
{
    if (audacity::musehub::IsMuseHubRegistered()) {
        return platformInteractive()->openUrl("musehub://plugins?compatible=audacity");
    }

    interactive()->open("audacity://projectscene/geteffects");
    return make_ok();
}
