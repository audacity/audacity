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
#include "../Track.h"
#include "../export/Export.h"

// Import

wxString ImportCommandType::BuildName()
{
   return wxT("Import");
}

void ImportCommandType::BuildSignature(CommandSignature &signature)
{
   auto filenameValidator = make_movable<DefaultValidator>();
   signature.AddParameter(wxT("Filename"), wxT(""), std::move(filenameValidator));
}

CommandHolder ImportCommandType::Create(std::unique_ptr<CommandOutputTarget> &&target)
{
   return std::make_shared<ImportCommand>(*this, std::move(target));
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
   auto modeValidator = make_movable<OptionValidator>();
   modeValidator->AddOption(wxT("All"));
   modeValidator->AddOption(wxT("Selection"));
   signature.AddParameter(wxT("Mode"), wxT("All"), std::move(modeValidator));

   auto filenameValidator = make_movable<DefaultValidator>();
   signature.AddParameter(wxT("Filename"), wxT("exported.wav"), std::move(filenameValidator));

   auto channelsValidator = make_movable<IntValidator>();
   signature.AddParameter(wxT("Channels"), 1, std::move(channelsValidator));
}

CommandHolder ExportCommandType::Create(std::unique_ptr<CommandOutputTarget> &&target)
{
   return std::make_shared<ExportCommand>(*this, std::move(target));
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

   bool exportSuccess = exporter.Process(context.GetProject(),
                                         std::max(0L, numChannels),
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
