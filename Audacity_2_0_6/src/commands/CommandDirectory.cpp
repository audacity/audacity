/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxWidgets

   Dan Horgan

******************************************************************//**

\file CommandDirectory.cpp
\brief Contains definitions for the CommandDirectory class

*//*******************************************************************/

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

CommandDirectory *CommandDirectory::mInstance = NULL;

CommandDirectory::CommandDirectory()
{
   wxASSERT(mInstance == NULL);
   mInstance = this;

   // Create the command map.
   // Adding an entry here is the easiest way to register a Command class.
   AddCommand(new ScreenshotCommandType());
   AddCommand(new BatchEvalCommandType());
   AddCommand(new ExecMenuCommandType());
   AddCommand(new GetAllMenuCommandsType());
   AddCommand(new MessageCommandType());
   AddCommand(new GetTrackInfoCommandType());
   AddCommand(new GetProjectInfoCommandType());

   AddCommand(new HelpCommandType());
   AddCommand(new SelectCommandType());
   AddCommand(new CompareAudioCommandType());
   AddCommand(new SetTrackInfoCommandType());
   AddCommand(new SetProjectInfoCommandType());

   AddCommand(new SetPreferenceCommandType());
   AddCommand(new GetPreferenceCommandType());
   AddCommand(new ImportCommandType());
   AddCommand(new ExportCommandType());
   AddCommand(new OpenProjectCommandType());
   AddCommand(new SaveProjectCommandType());
}

CommandDirectory::~CommandDirectory()
{
   // Delete the factories
   CommandMap::iterator iter;
   for (iter = mCmdMap.begin(); iter != mCmdMap.end(); ++iter)
   {
      delete iter->second;
   }
}

CommandType *CommandDirectory::LookUp(const wxString &cmdName) const
{
   CommandMap::const_iterator iter = mCmdMap.find(cmdName);
   if (iter == mCmdMap.end())
   {
      return NULL;
   }
   return iter->second;
}

void CommandDirectory::AddCommand(CommandType *type)
{
   wxASSERT(type != NULL);
   wxString cmdName = type->GetName();
   wxASSERT_MSG(mCmdMap.find(cmdName) == mCmdMap.end()
         , wxT("A command named ") + cmdName
         + wxT(" already exists."));

   mCmdMap[cmdName] = type;
}

CommandDirectory *CommandDirectory::Get()
{
   if (mInstance == NULL)
   {
      return new CommandDirectory();
   }
   return mInstance;
}

void CommandDirectory::Destroy()
{
   if (mInstance != NULL)
   {
      delete mInstance;
   }
}
