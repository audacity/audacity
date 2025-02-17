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

#include "CommandDispatch.h"
#include "MenuRegistry.h"
#include "../CommonCommandFlags.h"
#include "LoadCommands.h"
#include "../ProjectFileManager.h"
#include "ViewInfo.h"
#include "Export.h"
#include "../SelectUtilities.h"
#include "SettingsVisitor.h"
#include "ShuttleGui.h"
#include "Track.h"
#include "wxFileNameWrapper.h"
#include "CommandContext.h"
#include "ExportUtils.h"
#include "ProjectRate.h"
#include "ExportPluginRegistry.h"
#include "ExportProgressUI.h"

const ComponentInterfaceSymbol ImportCommand::Symbol
{ XO("Import2") };

namespace {
BuiltinCommandsModule::Registration< ImportCommand > reg;
}

template<bool Const>
bool ImportCommand::VisitSettings(SettingsVisitorBase<Const>& S)
{
    S.Define(mFileName, wxT("Filename"), wxString {});
    return true;
}

bool ImportCommand::VisitSettings(SettingsVisitor& S)
{ return VisitSettings<false>(S); }

bool ImportCommand::VisitSettings(ConstSettingsVisitor& S)
{ return VisitSettings<true>(S); }

void ImportCommand::PopulateOrExchange(ShuttleGui& S)
{
    S.AddSpace(0, 5);

    S.StartMultiColumn(2, wxALIGN_CENTER);
    {
        S.TieTextBox(XXO("File Name:"), mFileName);
    }
    S.EndMultiColumn();
}

bool ImportCommand::Apply(const CommandContext& context)
{
    bool wasEmpty = TrackList::Get(context.project).empty();
    const bool success
        =ProjectFileManager::Get(context.project).Import(mFileName, false);

    if (success && wasEmpty) {
        SelectUtilities::SelectAllIfNone(context.project);
    }

    return success;
}

template<bool Const>
bool ExportCommand::VisitSettings(SettingsVisitorBase<Const>& S)
{
    wxFileName fn = FileNames::FindDefaultPath(FileNames::Operation::Export);
    fn.SetName("exported.wav");
    S.Define(mFileName, wxT("Filename"), fn.GetFullPath());
    S.Define(mnChannels, wxT("NumChannels"),  1);
    return true;
}

bool ExportCommand::VisitSettings(SettingsVisitor& S)
{ return VisitSettings<false>(S); }

bool ExportCommand::VisitSettings(ConstSettingsVisitor& S)
{ return VisitSettings<true>(S); }

const ComponentInterfaceSymbol ExportCommand::Symbol
{ XO("Export2") };

namespace {
BuiltinCommandsModule::Registration< ExportCommand > reg2;
}

void ExportCommand::PopulateOrExchange(ShuttleGui& S)
{
    S.AddSpace(0, 5);

    S.StartMultiColumn(2, wxALIGN_CENTER);
    {
        S.TieTextBox(XXO("File Name:"), mFileName);
        S.TieTextBox(XXO("Number of Channels:"), mnChannels);
    }
    S.EndMultiColumn();
}

bool ExportCommand::Apply(const CommandContext& context)
{
    double t0, t1;
    auto& selectedRegion = ViewInfo::Get(context.project).selectedRegion;
    t0 = selectedRegion.t0();
    t1 = selectedRegion.t1();

    // Find the extension and check it's valid
    int splitAt = mFileName.Find(wxUniChar('.'), true);
    if (splitAt < 0) {
        context.Error(wxT("Export filename must have an extension!"));
        return false;
    }
    wxString extension = mFileName.Mid(splitAt + 1).MakeUpper();

    auto [plugin, formatIndex] = ExportPluginRegistry::Get().FindFormat(extension);

    if (plugin != nullptr) {
        auto editor = plugin->CreateOptionsEditor(formatIndex, nullptr);
        editor->Load(*gPrefs);

        auto builder = ExportTaskBuilder{}
        .SetParameters(ExportUtils::ParametersFromEditor(*editor))
        .SetNumChannels(std::max(0, mnChannels))
        .SetSampleRate(ProjectRate::Get(context.project).GetRate())
        .SetPlugin(plugin)
        .SetFileName(mFileName)
        .SetRange(t0, t1, true);

        auto result = ExportResult::Error;
        ExportProgressUI::ExceptionWrappedCall([&]
        {
            result = ExportProgressUI::Show(builder.Build(context.project));
        });
        if (result == ExportResult::Success || result == ExportResult::Stopped) {
            context.Status(wxString::Format(wxT("Exported to %s format: %s"),
                                            extension, mFileName));
            return true;
        }
    }

    context.Error(wxString::Format(wxT("Could not export to %s format!"), extension));
    return false;
}

namespace {
using namespace MenuRegistry;

// Register menu items

AttachedItem sAttachment{
    Items(wxT(""),
          // Note that the PLUGIN_SYMBOL must have a space between words,
          // whereas the short-form used here must not.
          // (So if you did write "Compare Audio" for the PLUGIN_SYMBOL name, then
          // you would have to use "CompareAudio" here.)
          Command(wxT("Import2"), XXO("Import..."),
                  CommandDispatch::OnAudacityCommand, AudioIONotBusyFlag()),
          Command(wxT("Export2"), XXO("Export..."),
                  CommandDispatch::OnAudacityCommand, AudioIONotBusyFlag())
          ),
    wxT("Optional/Extra/Part2/Scriptables2")
};
}
