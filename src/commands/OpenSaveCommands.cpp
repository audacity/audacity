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

#include "../Menus.h"
#include "../Project.h"
#include "../export/Export.h"
#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "CommandContext.h"


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
      S.TieTextBox(_("File Name:"),mFileName);
      S.TieCheckBox(_("Add to History"), mbAddToHistory );
   }
   S.EndMultiColumn();
}

bool OpenProjectCommand::Apply(const CommandContext & context){

   auto oldFileName = context.GetProject()->GetFileName();
   if(mFileName.empty())
   {
      auto project = context.GetProject();
      AudacityProject::OpenFiles(project);
   }
   else
   {
      context.GetProject()->OpenFile(mFileName, mbAddToHistory);
   }
   const auto &newFileName = context.GetProject()->GetFileName();

   // Because Open does not return a success or failure, we have to guess
   // at this point, based on whether the project file name has
   // changed and what to...
   return !newFileName.empty() && newFileName != oldFileName;
}

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
      S.TieTextBox(_("File Name:"),mFileName);
      S.TieCheckBox(_("Add to History:"), mbAddToHistory );
      S.TieCheckBox(_("Compress:"), mbCompress );
   }
   S.EndMultiColumn();
}

bool SaveProjectCommand::Apply(const CommandContext &context)
{
   if(mFileName.empty())
      return context.GetProject()->SaveAs(mbCompress);
   else
      return context.GetProject()->SaveAs(mFileName,mbCompress,mbAddToHistory);
}
