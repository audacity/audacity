/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxWidgets

   Dan Horgan

******************************************************************//**

\file CommandDirectory.cpp
\brief A dictionary of supported scripting commands, including
functions to look up a command by name.

*//*******************************************************************/

#include "CommandDirectory.h"

std::unique_ptr<CommandDirectory> CommandDirectory::mInstance;

CommandDirectory::CommandDirectory()
{
    // Create the command map.
    // First we have commands which return information
    //AddCommand(std::make_unique<MessageCommandType>());

//   AddCommand(std::make_unique<BatchEvalCommandType>());

    // Legacy adapter commands that previously was needed to
    // access menu items.
    //AddCommand(std::make_unique<ExecMenuCommandType>());

    // Not needed.  Sets selected/solo/mute on multiple tracks.
    //AddCommand(std::make_unique<SetProjectInfoCommandType>());

//   Moved to AudacityCommand
//   AddCommand(std::make_unique<OpenProjectCommandType>());
//   AddCommand(std::make_unique<SaveProjectCommandType>());
//   AddCommand(std::make_unique<ImportCommandType>());
//   AddCommand(std::make_unique<ExportCommandType>());
//   AddCommand(std::make_unique<HelpCommandType>());
//   AddCommand(std::make_unique<GetInfoCommandType>("GetAll"));
//   AddCommand(std::make_unique<GetInfoCommandType>("GetCommands"));
//   AddCommand(std::make_unique<GetInfoCommandType>("GetMenus"));
//   AddCommand(std::make_unique<GetInfoCommandType>("GetMenusPlus"));
//   AddCommand(std::make_unique<GetInfoCommandType>("GetBoxes"));
//   AddCommand(std::make_unique<GetInfoCommandType>("GetClips"));

//   AddCommand(std::make_unique<GetTrackInfoCommandType>());
//   AddCommand(std::make_unique<GetProjectInfoCommandType>());
//   AddCommand(std::make_unique<CompareAudioCommandType>());
//   AddCommand(std::make_unique<GetPreferenceCommandType>());
//   AddCommand(std::make_unique<SetPreferenceCommandType>());
//   AddCommand(std::make_unique<ScreenshotCommandType>());
//   AddCommand(std::make_unique<SelectCommandType>());
//   AddCommand(std::make_unique<SetTrackInfoCommandType>());
}

CommandDirectory::~CommandDirectory()
{
}

OldStyleCommandType* CommandDirectory::LookUp(const wxString& cmdName) const
{
    auto iter = sCmdMap().find(cmdName);
    if (iter == sCmdMap().end()) {
        return nullptr;
    }
    return iter->second.get();
}

CommandMap& CommandDirectory::sCmdMap()
{
    static CommandMap theMap;
    return theMap;
}

void CommandDirectory::AddCommand(std::unique_ptr<OldStyleCommandType> type)
{
    wxASSERT(type != NULL);
    // Internal string is shown but only in assertion message
    auto cmdName = type->GetSymbol().Internal();
    wxASSERT_MSG(sCmdMap().find(cmdName) == sCmdMap().end(),
                 wxT("A command named ") + cmdName
                 + wxT(" already exists."));

    sCmdMap()[cmdName] = std::move(type);
}

CommandDirectory* CommandDirectory::Get()
{
    if (!mInstance) {
        mInstance.reset(safenew CommandDirectory());
    }
    return mInstance.get();
}
