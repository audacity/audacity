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

#include "../Audacity.h"
#include "OpenSaveCommands.h"

#include "LoadCommands.h"
#include "../Project.h"
#include "../ProjectFileManager.h"
#include "../ProjectManager.h"
#include "../export/Export.h"
#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "CommandContext.h"


const ComponentInterfaceSymbol OpenProjectCommand::Symbol
{ XO("Open Project2") };

namespace{ BuiltinCommandsModule::Registration< OpenProjectCommand > reg; }

bool OpenProjectCommand::DefineParams( ShuttleParams & S ){
   S.Define( mFileName, wxT("Filename"),  "test.aup" );
   S.OptionalN(bHasAddToHistory).Define( mbAddToHistory, wxT("AddToHistory"),  false );
   return true;
}

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

   auto oldFileName = context.project.GetFileName();
   if(mFileName.empty())
   {
      auto project = &context.project;
      ProjectManager::OpenFiles(project);
   }
   else
   {
      ProjectFileManager::Get( context.project )
         .OpenFile(mFileName, mbAddToHistory);
   }
   const auto &newFileName = context.project.GetFileName();

   // Because Open does not return a success or failure, we have to guess
   // at this point, based on whether the project file name has
   // changed and what to...
   return !newFileName.empty() && newFileName != oldFileName;
}

const ComponentInterfaceSymbol SaveProjectCommand::Symbol
{ XO("Save Project2") };

namespace{ BuiltinCommandsModule::Registration< SaveProjectCommand > reg2; }

bool SaveProjectCommand::DefineParams( ShuttleParams & S ){
   S.Define( mFileName, wxT("Filename"),  "name.aup" );
   S.Define( mbAddToHistory, wxT("AddToHistory"),  false );
   S.Define( mbCompress, wxT("Compress"),  false );
   return true;
}

void SaveProjectCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieTextBox(XXO("File Name:"),mFileName);
      S.TieCheckBox(XXO("Add to History"), mbAddToHistory );
      S.TieCheckBox(XXO("Compress"), mbCompress );
   }
   S.EndMultiColumn();
}

bool SaveProjectCommand::Apply(const CommandContext &context)
{
   auto &projectFileManager = ProjectFileManager::Get( context.project );
   if ( mFileName.empty() )
      return projectFileManager.SaveAs(mbCompress);
   else
      return projectFileManager.SaveAs(
         mFileName, mbCompress, mbAddToHistory);
}
