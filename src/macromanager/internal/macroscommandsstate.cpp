/*
* Audacity: A Digital Audio Editor
*/
#include "macroscommandsstate.h"

using namespace au::macromanager;
using namespace muse::rcommand;

std::string MacrosCommandsState::moduleName() const
{
    return "macromanager";
}

void MacrosCommandsState::init()
{
}

void MacrosCommandsState::deinit()
{
}

CommandState MacrosCommandsState::commandState(const Command&) const
{
    return CommandState(true, false);
}

muse::async::Channel<Command, CommandState> MacrosCommandsState::commandStateChanged() const
{
    return m_commandStateChanged;
}
