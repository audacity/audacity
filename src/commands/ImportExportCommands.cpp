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

#include "../Audacity.h"
#include "ImportExportCommands.h"

#include "../Project.h"
#include "../Track.h"
#include "../export/Export.h"
#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "CommandContext.h"

bool ImportCommand::DefineParams( ShuttleParams & S ){
   S.Define( mFileName, wxT("Filename"),  "" );
   return true;
}

void ImportCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieTextBox(_("File Name:"),mFileName);
   }
   S.EndMultiColumn();
}

bool ImportCommand::Apply(const CommandContext & context){
   return context.GetProject()->Import(mFileName);
}



bool ExportCommand::DefineParams( ShuttleParams & S ){
   S.Define( mFileName, wxT("Filename"),  "exported.wav" );
   S.Define( mnChannels, wxT("NumChannels"),  1 );
   return true;
}

void ExportCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieTextBox(_("File Name:"),mFileName);
      S.TieTextBox(_("Number of Channels:"),mnChannels);
   }
   S.EndMultiColumn();
}

bool ExportCommand::Apply(const CommandContext & context)
{
   double t0, t1;
   t0 = context.GetProject()->mViewInfo.selectedRegion.t0();
   t1 = context.GetProject()->mViewInfo.selectedRegion.t1();

   // Find the extension and check it's valid
   int splitAt = mFileName.Find(wxUniChar('.'), true);
   if (splitAt < 0)
   {
      context.Error(wxT("Export filename must have an extension!"));
      return false;
   }
   wxString extension = mFileName.Mid(splitAt+1).MakeUpper();

   Exporter exporter;

   bool exportSuccess = exporter.Process(context.GetProject(),
                                         std::max(0, mnChannels),
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

