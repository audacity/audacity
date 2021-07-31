/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   File License: wxWidgets

   Dan Horgan
   James Crook

******************************************************************//**

\file ImportExportCommands.cpp
\brief Contains definitions for the ImportCommand and ExportCommand classes

*//*******************************************************************/


#include "ImportExportCommands.h"

#include "LoadCommands.h"
#include "../ProjectFileManager.h"
#include "../ViewInfo.h"
#include "../export/Export.h"
#include "../SelectUtilities.h"
#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "../Track.h"
#include "wxFileNameWrapper.h"
#include "CommandContext.h"

const ComponentInterfaceSymbol ImportCommand::Symbol
{ XO("Import2") };

namespace{ BuiltinCommandsModule::Registration< ImportCommand > reg; }

bool ImportCommand::DefineParams( ShuttleParams & S ){
   S.Define( mFileName, wxT("Filename"),  "" );
   return true;
}

void ImportCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieTextBox(XXO("File Name:"),mFileName);
   }
   S.EndMultiColumn();
}

bool ImportCommand::Apply(const CommandContext & context)
{
   bool wasEmpty = TrackList::Get( context.project ).Any().empty();
   bool success = ProjectFileManager::Get( context.project )
      .Import(mFileName, false);

   if (success && wasEmpty)
   {
      SelectUtilities::SelectAllIfNone( context.project );
   }

   return success;
}



bool ExportCommand::DefineParams( ShuttleParams & S ){
   wxFileName fn = FileNames::FindDefaultPath(FileNames::Operation::Export);
   fn.SetName("exported.wav");
   S.Define(mFileName, wxT("Filename"), fn.GetFullPath());
   S.Define( mnChannels, wxT("NumChannels"),  1 );
   return true;
}

const ComponentInterfaceSymbol ExportCommand::Symbol
{ XO("Export2") };

namespace{ BuiltinCommandsModule::Registration< ExportCommand > reg2; }

void ExportCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieTextBox(XXO("File Name:"),mFileName);
      S.TieTextBox(XXO("Number of Channels:"),mnChannels);
   }
   S.EndMultiColumn();
}

bool ExportCommand::Apply(const CommandContext & context)
{
   double t0, t1;
   auto &selectedRegion = ViewInfo::Get( context.project ).selectedRegion;
   t0 = selectedRegion.t0();
   t1 = selectedRegion.t1();

   // Find the extension and check it's valid
   int splitAt = mFileName.Find(wxUniChar('.'), true);
   if (splitAt < 0)
   {
      context.Error(wxT("Export filename must have an extension!"));
      return false;
   }
   wxString extension = mFileName.Mid(splitAt+1).MakeUpper();

   Exporter exporter{ context.project };

   bool exportSuccess = exporter.Process(std::max(0, mnChannels),
                                         extension, mFileName,
                                         true, t0, t1);

   if (exportSuccess)
   {
      context.Status(wxString::Format(wxT("Exported to %s format: %s"),
                              extension, mFileName));
      return true;
   }

   context.Error(wxString::Format(wxT("Could not export to %s format!"), extension));
   return false;
}

