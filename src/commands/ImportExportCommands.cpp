/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   File License: wxWidgets

   Dan Horgan

******************************************************************//**

\file ImportExportCommands.cpp
\brief Contains definitions for the ImportCommand and ExportCommand classes

*//*******************************************************************/

#include "ImportExportCommands.h"
#include "../Project.h"
#include "../export/Export.h"

// Import

wxString ImportCommandType::BuildName()
{
   return wxT("Import");
}

void ImportCommandType::BuildSignature(CommandSignature &signature)
{
   Validator *filenameValidator(new Validator());
   signature.AddParameter(wxT("Filename"), wxT(""), filenameValidator);
}

Command *ImportCommandType::Create(CommandOutputTarget *target)
{
   return new ImportCommand(*this, target);
}

bool ImportCommand::Apply(CommandExecutionContext context)
{
   wxString filename = GetString(wxT("Filename"));
   return context.GetProject()->Import(filename);
}

ImportCommand::~ImportCommand()
{ }

// Export

wxString ExportCommandType::BuildName()
{
   return wxT("Export");
}

void ExportCommandType::BuildSignature(CommandSignature &signature)
{
   OptionValidator *modeValidator(new OptionValidator());
   modeValidator->AddOption(wxT("All"));
   modeValidator->AddOption(wxT("Selection"));
   signature.AddParameter(wxT("Mode"), wxT("All"), modeValidator);

   Validator *filenameValidator(new Validator());
   signature.AddParameter(wxT("Filename"), wxT("exported.wav"), filenameValidator);

   IntValidator *channelsValidator(new IntValidator());
   signature.AddParameter(wxT("Channels"), 1, channelsValidator);
}

Command *ExportCommandType::Create(CommandOutputTarget *target)
{
   return new ExportCommand(*this, target);
}

bool ExportCommand::Apply(CommandExecutionContext context)
{
   wxString mode = GetString(wxT("Mode"));
   wxString filename = GetString(wxT("Filename"));
   long numChannels = GetLong(wxT("Channels"));

   bool selection = mode.IsSameAs(wxT("Selection"));

   double t0, t1;
   if (selection)
   {
      t0 = context.GetProject()->mViewInfo.selectedRegion.t0();
      t1 = context.GetProject()->mViewInfo.selectedRegion.t1();
   }
   else
   {
      t0 = 0.0;
      t1 = context.GetProject()->GetTracks()->GetEndTime();
   }

   // Find the extension and check it's valid
   int splitAt = filename.Find(wxT("."));
   if (splitAt < 0)
   {
      Error(wxT("Export filename must have an extension!"));
      return false;
   }
   wxString extension = filename.Mid(splitAt+1).MakeUpper();

   Exporter exporter;

   bool exportSuccess = exporter.Process(context.GetProject(), numChannels,
                                         extension.c_str(), filename,
                                         selection, t0, t1);

   if (exportSuccess)
   {
      Status(wxString::Format(wxT("Exported to %s format: %s"),
                              extension.c_str(), filename.c_str()));
      return true;
   }

   Error(wxString::Format(wxT("Could not export to %s format!"), extension.c_str()));
   return false;
}

ExportCommand::~ExportCommand()
{ }
