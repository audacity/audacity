/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxWidgets

   Dan Horgan

******************************************************************//**

\file CommandDirectory.cpp
\brief Contains definitions for the CommandDirectory class

*//*******************************************************************/

#include "../Audacity.h"
#include "CommandDirectory.h"
#include "CommandMisc.h"

#include "ScreenshotCommand.h"
#include "BatchEvalCommand.h"
#include "ExecMenuCommand.h"
#include "GetAllMenuCommands.h"
#include "MessageCommand.h"
#include "GetTrackInfoCommand.h"
#include "GetProjectInfoCommand.h"
#include "HelpCommand.h"
#include "SelectCommand.h"
#include "CompareAudioCommand.h"
#include "SetTrackInfoCommand.h"
#include "SetProjectInfoCommand.h"
#include "PreferenceCommands.h"
#include "ImportExportCommands.h"
#include "OpenSaveCommands.h"

std::unique_ptr<CommandDirectory> CommandDirectory::mInstance;

CommandDirectory::CommandDirectory()
{
   // Create the command map.
   // Adding an entry here is the easiest way to register a Command class.
   AddCommand(make_movable<ScreenshotCommandType>());
   AddCommand(make_movable<BatchEvalCommandType>());
   AddCommand(make_movable<ExecMenuCommandType>());
   AddCommand(make_movable<GetAllMenuCommandsType>());
   AddCommand(make_movable<MessageCommandType>());
   AddCommand(make_movable<GetTrackInfoCommandType>());
   AddCommand(make_movable<GetProjectInfoCommandType>());

   AddCommand(make_movable<HelpCommandType>());
   AddCommand(make_movable<SelectCommandType>());
   AddCommand(make_movable<CompareAudioCommandType>());
   AddCommand(make_movable<SetTrackInfoCommandType>());
   AddCommand(make_movable<SetProjectInfoCommandType>());

   AddCommand(make_movable<SetPreferenceCommandType>());
   AddCommand(make_movable<GetPreferenceCommandType>());
   AddCommand(make_movable<ImportCommandType>());
   AddCommand(make_movable<ExportCommandType>());
   AddCommand(make_movable<OpenProjectCommandType>());
   AddCommand(make_movable<SaveProjectCommandType>());
}

CommandDirectory::~CommandDirectory()
{
}

CommandType *CommandDirectory::LookUp(const wxString &cmdName) const
{
   CommandMap::const_iterator iter = mCmdMap.find(cmdName);
   if (iter == mCmdMap.end())
   {
      return NULL;
   }
   return iter->second.get();
}

void CommandDirectory::AddCommand(movable_ptr<CommandType> &&type)
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
