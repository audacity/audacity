/*
* Audacity: A Digital Audio Editor
*/
#include "musehubcommandsstate.h"

using namespace au::musehub;
using namespace muse;
using namespace muse::rcommand;

std::string MuseHubCommandsState::moduleName() const
{
    return "musehub";
}

void MuseHubCommandsState::init()
{
}

void MuseHubCommandsState::deinit()
{
}

CommandState MuseHubCommandsState::commandState(const Command&) const
{
    return CommandState(true, false);
}

async::Channel<Command, CommandState> MuseHubCommandsState::commandStateChanged() const
{
    return {};
}
