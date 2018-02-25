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

#include "../Audacity.h"
#include "CommandDirectory.h"
#include "CommandMisc.h"

#include "HelpCommand.h"
#include "MessageCommand.h"
#include "BatchEvalCommand.h"

std::unique_ptr<CommandDirectory> CommandDirectory::mInstance;

CommandDirectory::CommandDirectory()
{
   // Create the command map.
   // First we have commands which return information
   //AddCommand(make_movable<MessageCommandType>());
   AddCommand(make_movable<BatchEvalCommandType>());


   // Legacy adapter commands that previously was needed to 
   // access menu items.
   //AddCommand(make_movable<ExecMenuCommandType>());

   // Not needed.  Sets selected/solo/mute on multiple tracks.
   //AddCommand(make_movable<SetProjectInfoCommandType>());

//   Moved to AudacityCommand
//   AddCommand(make_movable<OpenProjectCommandType>());
//   AddCommand(make_movable<SaveProjectCommandType>());
//   AddCommand(make_movable<ImportCommandType>());
//   AddCommand(make_movable<ExportCommandType>());
//   AddCommand(make_movable<HelpCommandType>());
//   AddCommand(make_movable<GetInfoCommandType>("GetAll"));
//   AddCommand(make_movable<GetInfoCommandType>("GetCommands"));
//   AddCommand(make_movable<GetInfoCommandType>("GetMenus"));
//   AddCommand(make_movable<GetInfoCommandType>("GetMenusPlus"));
//   AddCommand(make_movable<GetInfoCommandType>("GetBoxes"));
//   AddCommand(make_movable<GetInfoCommandType>("GetClips"));

//   AddCommand(make_movable<GetTrackInfoCommandType>());
//   AddCommand(make_movable<GetProjectInfoCommandType>());
//   AddCommand(make_movable<CompareAudioCommandType>());
//   AddCommand(make_movable<GetPreferenceCommandType>());
//   AddCommand(make_movable<SetPreferenceCommandType>());
//   AddCommand(make_movable<ScreenshotCommandType>());
//   AddCommand(make_movable<SelectCommandType>());
//   AddCommand(make_movable<SetTrackInfoCommandType>());

}

CommandDirectory::~CommandDirectory()
{
}

OldStyleCommandType *CommandDirectory::LookUp(const wxString &cmdName) const
{
   CommandMap::const_iterator iter = mCmdMap.find(cmdName);
   if (iter == mCmdMap.end())
   {
      return NULL;
   }
   return iter->second.get();
}

void CommandDirectory::AddCommand(movable_ptr<OldStyleCommandType> &&type)
{
   wxASSERT(type != NULL);
   wxString cmdName = type->GetName();
   wxASSERT_MSG(mCmdMap.find(cmdName) == mCmdMap.end()
         , wxT("A command named ") + cmdName
         + wxT(" already exists."));

   mCmdMap[cmdName] = std::move(type);
}

CommandDirectory *CommandDirectory::Get()
{
   if (!mInstance)
      mInstance.reset(safenew CommandDirectory());
   return mInstance.get();
}
