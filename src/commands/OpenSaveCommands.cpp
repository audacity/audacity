/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   File License: wxWidgets

   Stephen Parry
   James Crook

******************************************************************//**

\file OpenSaveCommands.cpp
\brief Contains definitions for the OpenProjectCommand and SaveProjectCommand classes

*//*******************************************************************/


#include "OpenSaveCommands.h"

#include "CommandDispatch.h"
#include "CommandManager.h"
#include "../CommonCommandFlags.h"
#include "LoadCommands.h"
#include "AudacityLogger.h"
#include "Project.h"
#include "../ProjectFileIO.h"
#include "../ProjectFileManager.h"
#include "../ProjectManager.h"
#include "../export/Export.h"
#include "SettingsVisitor.h"
#include "ShuttleGui.h"
#include "CommandContext.h"


const ComponentInterfaceSymbol OpenProjectCommand::Symbol
{ XO("Open Project2") };

namespace{ BuiltinCommandsModule::Registration< OpenProjectCommand > reg; }

template<bool Const>
bool OpenProjectCommand::VisitSettings( SettingsVisitorBase<Const> & S ){
   S.Define( mFileName, wxT("Filename"), wxString{"test.aup3"} );
   S.OptionalN(bHasAddToHistory).Define( mbAddToHistory, wxT("AddToHistory"),  false );
   return true;
}

bool OpenProjectCommand::VisitSettings( SettingsVisitor & S )
   { return VisitSettings<false>(S); }

bool OpenProjectCommand::VisitSettings( ConstSettingsVisitor & S )
   { return VisitSettings<true>(S); }

void OpenProjectCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieTextBox(XXO("File Name:"),mFileName);
      S.TieCheckBox(XXO("Add to History"), mbAddToHistory );
   }
   S.EndMultiColumn();
}

bool OpenProjectCommand::Apply(const CommandContext & context){

   auto &projectFileIO = ProjectFileIO::Get(context.project);

   auto oldFileName = projectFileIO.GetFileName();
   if(mFileName.empty())
   {
      // This path queries the user for files to open
      auto project = &context.project;
      ProjectManager::OpenFiles(project);
   }
   else
   {
      ProjectManager::ProjectChooser chooser{ &context.project, true };
      if(ProjectFileManager::OpenFile(
         std::ref(chooser), mFileName, mbAddToHistory))
         chooser.Commit();
   }
   const auto &newFileName = projectFileIO.GetFileName();

   // Because Open does not return a success or failure, we have to guess
   // at this point, based on whether the project file name has
   // changed and what to...
   return !newFileName.empty() && newFileName != oldFileName;
}

const ComponentInterfaceSymbol SaveProjectCommand::Symbol
{ XO("Save Project2") };

namespace{ BuiltinCommandsModule::Registration< SaveProjectCommand > reg2; }

template<bool Const>
bool SaveProjectCommand::VisitSettings( SettingsVisitorBase<Const> & S ){
   S.Define( mFileName, wxT("Filename"), wxString{"name.aup3"} );
   S.Define( mbAddToHistory, wxT("AddToHistory"),  false );
   return true;
}

bool SaveProjectCommand::VisitSettings( SettingsVisitor & S )
   { return VisitSettings<false>(S); }

bool SaveProjectCommand::VisitSettings( ConstSettingsVisitor & S )
   { return VisitSettings<true>(S); }

void SaveProjectCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieTextBox(XXO("File Name:"),mFileName);
      S.TieCheckBox(XXO("Add to History"), mbAddToHistory );
   }
   S.EndMultiColumn();
}

bool SaveProjectCommand::Apply(const CommandContext &context)
{
   auto &projectFileManager = ProjectFileManager::Get( context.project );
   if ( mFileName.empty() )
      return projectFileManager.SaveAs();
   else
      return projectFileManager.SaveAs(mFileName, mbAddToHistory);
}

const ComponentInterfaceSymbol SaveCopyCommand::Symbol
{ XO("Save Copy") };

namespace{ BuiltinCommandsModule::Registration< SaveCopyCommand > reg3; }

template<bool Const>
bool SaveCopyCommand::VisitSettings( SettingsVisitorBase<Const> & S ){
   S.Define( mFileName, wxT("Filename"), wxString{"name.aup3"} );
   return true;
}

bool SaveCopyCommand::VisitSettings( SettingsVisitor & S )
   { return VisitSettings<false>(S); }

bool SaveCopyCommand::VisitSettings( ConstSettingsVisitor & S )
   { return VisitSettings<true>(S); }

void SaveCopyCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieTextBox(XXO("File Name:"),mFileName);
   }
   S.EndMultiColumn();
}

bool SaveCopyCommand::Apply(const CommandContext &context)
{
   auto &projectFileManager = ProjectFileManager::Get( context.project );
   return projectFileManager.SaveCopy(mFileName);
}

const ComponentInterfaceSymbol SaveLogCommand::Symbol
{ XO("Save Log") };

namespace{ BuiltinCommandsModule::Registration< SaveLogCommand > reg4; }

template<bool Const>
bool SaveLogCommand::VisitSettings(SettingsVisitorBase<Const> & S)
{
   S.Define( mFileName, wxT("Filename"), wxString{"log.txt"} );
   return true;
}

bool SaveLogCommand::VisitSettings( SettingsVisitor & S )
   { return VisitSettings<false>(S); }

bool SaveLogCommand::VisitSettings( ConstSettingsVisitor & S )
   { return VisitSettings<true>(S); }

void SaveLogCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieTextBox(XXO("File Name:"),mFileName);
   }
   S.EndMultiColumn();
}

bool SaveLogCommand::Apply(const CommandContext &context)
{
   auto logger = AudacityLogger::Get();
   return logger->SaveLog(mFileName);
}

const ComponentInterfaceSymbol ClearLogCommand::Symbol
{ XO("Clear Log") };

namespace{ BuiltinCommandsModule::Registration< ClearLogCommand > reg5; }

template<bool Const>
bool ClearLogCommand::VisitSettings(SettingsVisitorBase<Const> & S)
{
   return true;
}

bool ClearLogCommand::VisitSettings( SettingsVisitor & S )
   { return VisitSettings<false>(S); }

bool ClearLogCommand::VisitSettings( ConstSettingsVisitor & S )
   { return VisitSettings<true>(S); }

bool ClearLogCommand::PromptUser(wxWindow *parent)
{
   return true;
}

bool ClearLogCommand::Apply(const CommandContext &context)
{
   auto logger = AudacityLogger::Get();
   return logger->ClearLog();
}

namespace {
using namespace MenuTable;

// Register menu items

AttachedItem sAttachment{
   wxT("Optional/Extra/Part2/Scriptables2"),
   Items( wxT(""),
      // Note that the PLUGIN_SYMBOL must have a space between words,
      // whereas the short-form used here must not.
      // (So if you did write "Compare Audio" for the PLUGIN_SYMBOL name, then
      // you would have to use "CompareAudio" here.)
      Command( wxT("OpenProject2"), XXO("Open Project..."),
         CommandDispatch::OnAudacityCommand, AudioIONotBusyFlag() ),
      Command( wxT("SaveProject2"), XXO("Save Project..."),
         CommandDispatch::OnAudacityCommand, AudioIONotBusyFlag() )
   )
};
}
