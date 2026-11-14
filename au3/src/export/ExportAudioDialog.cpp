/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportAudioDialog.cpp

  Dominic Mazzoni

  Vitaly Sverchinsky split from ExportMultiple.cpp and ExportAudioDialog.cpp

**********************************************************************/

#include "ExportAudioDialog.h"

#include <numeric>

#include <wx/frame.h>

#include "Export.h"
#include "ExportUtils.h"
#include "WaveTrack.h"
#include "LabelTrack.h"
#include "Mix.h"
#include "Prefs.h"
#include "ViewInfo.h"
#include "Project.h"
#include "SelectionState.h"

#include <wx/log.h>
#include <wx/checkbox.h>
#include <wx/textctrl.h>
#include <wx/button.h>
#include <wx/radiobut.h>
#include <wx/stattext.h>
#include <wx/event.h>

#include "ShuttleGui.h"
#include "AudacityMessageBox.h"
#include "ProjectWindows.h"
#include "Theme.h"
#include "HelpSystem.h"
#include "TagsEditor.h"
#include "ExportFilePanel.h"
#include "ExportProgressUI.h"
#include "ImportExport.h"
#include "RealtimeEffectList.h"
#include "WindowAccessible.h"

#if wxUSE_ACCESSIBILITY
#include "WindowAccessible.h"
#endif

namespace {
const bool hookRegistered = [] {
    ExportUtils::RegisterExportHook(
        [](AudacityProject& project, const FileExtension& format,
           AudiocomTrace /*ignore*/, bool selectedOnly) {
        ExportAudioDialog dialog { &GetProjectFrame(project), project,
                                   project.GetProjectName(), format,
                                   selectedOnly
                                   ? ExportAudioDialog::ExportMode::SelectedOnly
                                   : ExportAudioDialog::ExportMode::Auto
        };

        dialog.ShowModal();
        return ExportUtils::ExportHookResult::Handled;
    });
    return true;
}();

ChoiceSetting ExportAudioExportRange { L"/ExportAudioDialog/ExportRange",
                                       {
                                           { "project", XO("Entire &Project") },
                                           { "split", XO("M&ultiple Files") },
                                           { "selection", XO("Curren&t Selection") }
                                       },
                                       0, //project
};

ChoiceSetting ExportAudioSplitMode { L"/ExportAudioDialog/SplitMode",
                                     {
                                         { "tracks", XO("Tracks") },
                                         { "labels", XO("Labels") }
                                     },
                                     0
};

ChoiceSetting ExportAudioSplitNamePolicy { L"/ExportAudioDialog/SplitNamePolicy",
                                           {
                                               { "name", XO("Using Label/Track Name") },//->"label_track"
                                               { "num_and_name", XO("Numbering before Label/Track Name") },//->"number_before"
                                               { "num_and_prefix", XO("Numbering after File name prefix") }//->"number_after"
                                           },
                                           0
};

BoolSetting ExportAudioIncludeAudioBeforeFirstLabel { L"/ExportAudioDialog/IncludeAudioBeforeFirstLabel", false };

BoolSetting ExportAudioOverwriteExisting { L"/ExportAudioDialog/OverwriteExisting", false };

BoolSetting ExportAudioSkipSilenceAtBeginning { L"/ExportAudioDialog/SkipSilenceAtBeginning", false };

StringSetting ExportAudioDefaultFormat{ L"/ExportAudioDialog/Format", L"WAV" };

StringSetting ExportAudioDefaultPath{ L"ExportAudioDialog/DefaultPath", L"" };

enum {
    ExportFilePanelID = 10000,//to avoid IDs collision with ExportFilePanel items

    ExportRangeProjectID,
    ExportRangeSelectionID,
    ExportRangeSplitID,

    TrimBlankSpaceBeforeFirstClipID,

    ExportModeTracksID,
    ExportModeLabelsID,

    IncludeAudioBeforeFirstLabelID,

    ExportSplitNamePolicyTrackNameID,
    ExportSplitNamePolicyNumberingBeforeNameID,
    ExportSplitNamePolicyNumberingAfterPrefixID,

    FileNamePrefixID,

    OverwriteExistingFilesID,

    EditMetadataID,
};
}

BEGIN_EVENT_TABLE(ExportAudioDialog, wxDialogWrapper)
EVT_COMMAND(ExportFilePanelID, AUDACITY_EXPORT_FORMAT_CHANGE_EVENT, ExportAudioDialog::OnFormatChange)

EVT_RADIOBUTTON(ExportRangeProjectID, ExportAudioDialog::OnExportRangeChange)
EVT_RADIOBUTTON(ExportRangeSelectionID, ExportAudioDialog::OnExportRangeChange)
EVT_RADIOBUTTON(ExportRangeSplitID, ExportAudioDialog::OnExportRangeChange)

EVT_RADIOBUTTON(ExportModeTracksID, ExportAudioDialog::OnSplitModeChange)
EVT_RADIOBUTTON(ExportModeLabelsID, ExportAudioDialog::OnSplitModeChange)

EVT_RADIOBUTTON(ExportSplitNamePolicyTrackNameID, ExportAudioDialog::OnSplitNamePolicyChange)
EVT_RADIOBUTTON(ExportSplitNamePolicyNumberingBeforeNameID, ExportAudioDialog::OnSplitNamePolicyChange)
EVT_RADIOBUTTON(ExportSplitNamePolicyNumberingAfterPrefixID, ExportAudioDialog::OnSplitNamePolicyChange)

EVT_CHECKBOX(TrimBlankSpaceBeforeFirstClipID, ExportAudioDialog::OnTrimBlankSpaceBeforeFirstClip)
EVT_CHECKBOX(IncludeAudioBeforeFirstLabelID, ExportAudioDialog::OnIncludeAudioBeforeFirstLabelChange)

EVT_TEXT(FileNamePrefixID, ExportAudioDialog::OnFileNamePrefixChange)

EVT_BUTTON(EditMetadataID, ExportAudioDialog::OnEditMetadata)
EVT_BUTTON(wxID_HELP, ExportAudioDialog::OnHelp)

EVT_BUTTON(wxID_OK, ExportAudioDialog::OnExport)
END_EVENT_TABLE()

ExportAudioDialog::ExportAudioDialog(wxWindow* parent,
                                     AudacityProject& project,
                                     const wxString& defaultName,
                                     const wxString& defaultFormat,
                                     ExportMode mode)
    : wxDialogWrapper(parent, wxID_ANY, XO("Export Audio"))
    , mProject(project)
{
    assert(!ExportUtils::FindExportWaveTracks(TrackList::Get(project), false).empty());

    ShuttleGui S(this, eIsCreatingFromPrefs);
    PopulateOrExchange(S);

    SetMinSize({ GetBestSize().GetWidth(), -1 });

    wxFileName filename;
    auto exportPath = ExportAudioDefaultPath.Read();
    if (exportPath.empty()) {
        exportPath = FileNames::FindDefaultPath(FileNames::Operation::Export);
    }
    filename.SetPath(exportPath);

    //extension will be set in `ChangeFormat`
    filename.SetEmptyExt();
    if (defaultName.empty()) {
        //i18n-hint: default exported file name when exporting from unsaved project
        filename.SetName(_("untitled"));
    } else {
        filename.SetName(defaultName);
    }

    auto sampleRate = ImportExport::Get(project).GetPreferredExportRate();
    if (sampleRate == ImportExport::InvalidRate) {
        auto& tracks = TrackList::Get(project);
        for (const auto track : tracks.Any<WaveTrack>()) {
            sampleRate = std::max(sampleRate, track->GetRate());
        }
    }

    wxString format = defaultFormat;
    if (format.empty()) {
        ExportAudioDefaultFormat.Read(&format);
    }

    mExportOptionsPanel->Init(filename, sampleRate, format);

    auto& tracks = TrackList::Get(mProject);
    const auto labelTracks = tracks.Any<LabelTrack>();
    const auto hasLabels = !labelTracks.empty()
                           && (*labelTracks.begin())->GetNumLabels() > 0;
    const auto hasMultipleWaveTracks = tracks.Any<WaveTrack>().size() > 1;
    const auto hasSelectedAudio = ExportUtils::HasSelectedAudio(mProject);

    mRangeSelection->Enable(hasSelectedAudio);
    mRangeSplit->Enable(hasLabels || hasMultipleWaveTracks);

    mSplitByLabels->Enable(hasLabels);
    mSplitByTracks->Enable(hasMultipleWaveTracks);

    if (mRangeSelection->IsEnabled() && mode == ExportMode::SelectedOnly) {
        mRangeSelection->SetValue(true);
    } else {
        if (!mRangeSelection->IsEnabled() && ExportAudioExportRange.Read() == "selection") {
            mRangeProject->SetValue(true);
        }

        if (!mRangeSplit->IsEnabled() && ExportAudioExportRange.Read() == "split") {
            mRangeProject->SetValue(true);
        }

        if (!hasLabels && hasMultipleWaveTracks) {
            mSplitByTracks->SetValue(true);
        }
        if (!hasMultipleWaveTracks && hasLabels) {
            mSplitByLabels->SetValue(true);
        }
    }

    if (mRangeSelection->IsEnabled() && !hasLabels && !hasMultipleWaveTracks) {
        mRangeSelection->MoveAfterInTabOrder(mRangeProject);
    }

    if (ExportAudioExportRange.Read() != "split" || (!hasLabels && !hasMultipleWaveTracks)) {
        mSplitsPanel->Hide();
    }

    mExportOptionsPanel->SetCustomMappingEnabled(
        !mRangeSplit->GetValue()
        &&//Custom channel export isn't available when master channel has effects assigned
        RealtimeEffectList::Get(project).GetStatesCount() == 0
        );

    mIncludeAudioBeforeFirstLabel->Enable(mSplitByLabels->GetValue());

    if (ExportAudioSplitNamePolicy.Read() != "num_and_prefix") {
        mSplitFileNamePrefix->Disable();
    }

    Layout();
    Fit();
    Center();
}

ExportAudioDialog::~ExportAudioDialog() = default;

// Fix for issue #4960, which only affects Windows
bool ExportAudioDialog::Show(bool show)
{
    bool ret = wxDialogWrapper::Show(show);

#if defined(__WXMSW__)
    if (show) {
        mExportOptionsPanel->SetInitialFocus();
    }
#endif

    return ret;
}

void ExportAudioDialog::PopulateOrExchange(ShuttleGui& S)
{
    S.SetBorder(5);
    S.StartVerticalLay();
    {
        if (S.GetMode() == eIsCreating) {
            mExportOptionsPanel = safenew ExportFilePanel(mProject, false, this, ExportFilePanelID);
            S.Id(ExportFilePanelID).AddWindow(mExportOptionsPanel, wxEXPAND);
        }

        S.StartPanel();
        {
            S.SetBorder(5);
            S.StartTwoColumn();
            {
                S.StartHorizontalLay(wxSHRINK | wxALIGN_TOP);
                {
                    if (auto prompt = S.AddPrompt(XO("Export Range:"))) {
                        prompt->SetMinSize({ 145, -1 });
                    }
                }
                S.EndHorizontalLay();

                S.StartVerticalLay();
                {
                    S.StartRadioButtonGroup(ExportAudioExportRange);
                    {
                        mRangeProject = S.Id(ExportRangeProjectID)
                                        .Name(XO("Export entire project"))
                                        .TieRadioButton();
                        mRangeSplit = S.Id(ExportRangeSplitID)
                                      .Name(XO("Export multiple files"))
                                      .TieRadioButton();
                        mRangeSelection = S.Id(ExportRangeSelectionID)
                                          .Name(XO("Export current selection"))
                                          .TieRadioButton();
#if wxUSE_ACCESSIBILITY
                        safenew WindowAccessible(mRangeProject);
                        safenew WindowAccessible(mRangeSelection);
                        safenew WindowAccessible(mRangeSplit);
#endif
                    }
                    S.EndRadioButtonGroup();
                }
                S.EndVerticalLay();
            }
            S.EndTwoColumn();

            S.AddSpace(10);

            S.StartTwoColumn();
            {
                S.AddSpace(155, 1);
                mSkipSilenceAtBeginning = S
                                          .Id(TrimBlankSpaceBeforeFirstClipID)
                                          .TieCheckBox(XO("Trim blank space before first clip"), ExportAudioSkipSilenceAtBeginning);
            }
            S.EndTwoColumn();
        }
        S.EndPanel();

        S.SetBorder(5);
        mSplitsPanel = S.StartPanel();
        {
            S.StartMultiColumn(2);
            {
                S.StartStatic(XO("Split files based on:"));
                {
                    S.StartVerticalLay();
                    {
                        S.StartRadioButtonGroup(ExportAudioSplitMode);
                        {
                            mSplitByTracks = S.Id(ExportModeTracksID).TieRadioButton();
                            mSplitByLabels = S.Id(ExportModeLabelsID).TieRadioButton();
                        }
                        S.EndRadioButtonGroup();

                        S.StartHorizontalLay(wxALIGN_TOP);
                        {
                            S.AddSpace(10, 1);
                            mIncludeAudioBeforeFirstLabel = S
                                                            .Id(IncludeAudioBeforeFirstLabelID)
                                                            .TieCheckBox(XO(
                                                                             "Include audio before first label"),
                                                                         ExportAudioIncludeAudioBeforeFirstLabel);
                        }
                        S.EndHorizontalLay();
                    }
                    S.EndVerticalLay();
                }
                S.EndStatic();

                S.StartStatic(XO("Name files:"));
                {
                    S.StartVerticalLay();
                    {
                        S.StartRadioButtonGroup(ExportAudioSplitNamePolicy);
                        {
                            mSplitUseName = S.Id(ExportSplitNamePolicyTrackNameID).TieRadioButton();
                            mSplitUseNumAndName = S.Id(ExportSplitNamePolicyNumberingBeforeNameID).TieRadioButton();
                            mSplitUseNumAndPrefix = S.Id(ExportSplitNamePolicyNumberingAfterPrefixID).TieRadioButton();
                        }
                        S.EndRadioButtonGroup();
                        S.StartHorizontalLay(wxALIGN_TOP);
                        {
                            S.AddSpace(10, 1);
                            mSplitFileNamePrefix = S
                                                   .Id(FileNamePrefixID)
                                                   .AddTextBox(XO("File name prefix:"), {}, 0);
                        }
                        S.EndHorizontalLay();
                    }
                    S.EndVerticalLay();
                }
                S.EndStatic();
            }
            S.EndMultiColumn();

            mOverwriteExisting = S
                                 .Id(OverwriteExistingFilesID)
                                 .TieCheckBox(XO("Overwrite existing files"), ExportAudioOverwriteExisting);
        }
        S.EndPanel();

        S.AddSpace(1, 10);

        S.SetBorder(5);

        S.SetBorder(5);
        S.StartHorizontalLay(wxEXPAND);
        {
            mEditMetadata = S.Id(EditMetadataID).AddButton(XO("Edit &Metadata..."), wxLEFT | wxBOTTOM);
            S.AddSpace(1, 1, wxEXPAND);
            S.Id(wxID_CANCEL).AddButton(XO("&Cancel"), wxBOTTOM);
            S.Id(wxID_OK).AddButton(XO("&Export"), wxRIGHT | wxBOTTOM, true);
        }
        S.EndHorizontalLay();
    }
    S.EndVerticalLay();
}

void ExportAudioDialog::OnExportRangeChange(wxCommandEvent& event)
{
    const auto enableSplits = event.GetId() == ExportRangeSplitID;
    mExportOptionsPanel->SetCustomMappingEnabled(!enableSplits);
    if (mSplitsPanel->IsShown() != enableSplits) {
        mExportSettingsDirty = true;
        mSplitsPanel->Show(enableSplits);

        Layout();
        Fit();
    }
}

void ExportAudioDialog::OnSplitModeChange(wxCommandEvent& event)
{
    mIncludeAudioBeforeFirstLabel->Enable(event.GetId() == ExportModeLabelsID);
    mExportSettingsDirty = true;
}

void ExportAudioDialog::OnSplitNamePolicyChange(wxCommandEvent& event)
{
    mSplitFileNamePrefix->Enable(event.GetId() == ExportSplitNamePolicyNumberingAfterPrefixID);
    mExportSettingsDirty = true;
}

void ExportAudioDialog::OnTrimBlankSpaceBeforeFirstClip(wxCommandEvent&)
{
    mExportSettingsDirty = true;
}

void ExportAudioDialog::OnIncludeAudioBeforeFirstLabelChange(wxCommandEvent& event)
{
    mExportSettingsDirty = true;
}

void ExportAudioDialog::OnFileNamePrefixChange(wxCommandEvent&)
{
    mExportSettingsDirty = true;
}

void ExportAudioDialog::OnEditMetadata(wxCommandEvent& event)
{
    if (mRangeSplit->GetValue()) {
        UpdateExportSettings();

        std::vector<Tags*> tags;
        std::vector<wxString> names;
        tags.reserve(mExportSettings.size());
        names.reserve(mExportSettings.size());
        for (auto& spec : mExportSettings) {
            tags.push_back(&spec.tags);
            names.push_back(spec.filename.GetFullName());
        }
        TagsEditorDialog dialog(this, XO("Edit Metadata Tags"), tags, names, true, true);
        dialog.ShowModal();
    } else {
        TagsEditorDialog::EditProjectMetadata(mProject,
                                              XO("Edit Metadata Tags"), XO("Exported Tags"));
    }
}

void ExportAudioDialog::OnHelp(wxCommandEvent& event)
{
    HelpSystem::ShowHelp(wxGetTopLevelParent(this), L"File_Export_Dialog", true);
}

void ExportAudioDialog::OnExport(wxCommandEvent& event)
{
    auto selectedPlugin = mExportOptionsPanel->GetPlugin();
    if (selectedPlugin == nullptr) {
        return;
    }
    auto selectedFormat = mExportOptionsPanel->GetFormat();
    auto parameters = mExportOptionsPanel->GetParameters();
    if (!parameters.has_value()) {
        return;
    }

    const auto path = mExportOptionsPanel->GetPath();

    if (!wxDirExists(path)) {
        wxFileName::Mkdir(path, wxS_DIR_DEFAULT, wxPATH_MKDIR_FULL);
    }

    if (!wxDirExists(path)) {
        AudacityMessageBox(XO("Unable to create destination folder"),
                           XO("Export Audio"),
                           wxOK | wxCENTER,
                           this);
        return;
    }

    auto result = ExportResult::Error;

    if (mRangeSplit->GetValue()) {
        FilePaths exportedFiles;

        UpdateExportSettings();

        if (mSplitByLabels->GetValue()) {
            result = DoExportSplitByLabels(*selectedPlugin, selectedFormat, *parameters, exportedFiles);
        } else if (mSplitByTracks->GetValue()) {
            result = DoExportSplitByTracks(*selectedPlugin, selectedFormat, *parameters, exportedFiles);
        }

        auto msg = (result == ExportResult::Success
                    ? XO("Successfully exported the following %lld file(s).")
                    : result == ExportResult::Error
                    ? XO("Something went wrong after exporting the following %lld file(s).")
                    : result == ExportResult::Cancelled
                    ? XO("Export canceled after exporting the following %lld file(s).")
                    : result == ExportResult::Stopped
                    ? XO("Export stopped after exporting the following %lld file(s).")
                    : XO("Something went really wrong after exporting the following %lld file(s).")
                    ).Format((long long)exportedFiles.size());

        wxString fileList;
        for (auto& path : exportedFiles) {
            fileList += path + '\n';
        }

        // TODO: give some warning dialog first, when only some files exported
        // successfully.

        HelpSystem::ShowInfoDialog(this,
                                   XO("Export Audio"),
                                   msg,
                                   fileList,
                                   450, 400);
    } else {
        wxFileName filename(path, mExportOptionsPanel->GetFullName());

        if (filename.FileExists()) {
            auto result = AudacityMessageBox(
                XO("A file named \"%s\" already exists. Replace?")
                .Format(filename.GetFullPath()),
                XO("Export Audio"),
                wxYES_NO | wxICON_EXCLAMATION);
            if (result != wxYES) {
                return;
            }
        }

        ExportTaskBuilder builder;
        builder.SetFileName(filename)
        .SetPlugin(selectedPlugin, selectedFormat)
        .SetParameters(*parameters)
        .SetSampleRate(mExportOptionsPanel->GetSampleRate());

        const auto& viewInfo = ViewInfo::Get(mProject);

        const auto selectedOnly = mRangeSelection->GetValue();

        auto t0 = selectedOnly
                  ? std::max(.0, viewInfo.selectedRegion.t0())
                  : .0;

        auto t1 = selectedOnly
                  ? std::min(TrackList::Get(mProject).GetEndTime(), viewInfo.selectedRegion.t1())
                  : TrackList::Get(mProject).GetEndTime();

        auto exportedTracks = ExportUtils::FindExportWaveTracks(TrackList::Get(mProject), selectedOnly);
        if (exportedTracks.empty()) {
            ShowExportErrorDialog(
                selectedOnly ? XO("All selected audio is muted.") : XO("All audio is muted."), //":576"
                XO("Warning"),
                false);
            return;
        }

        if (mSkipSilenceAtBeginning->GetValue()) {
            t0 = std::max(t0, exportedTracks.min(&Track::GetStartTime));
        }

        builder.SetRange(t0, t1, selectedOnly);

        std::unique_ptr<MixerOptions::Downmix> tempMixerSpec;
        const auto channels = mExportOptionsPanel->GetChannels();
        if (channels == 0) {
            //Figure out the final channel mapping: mixer dialog shows
            //all tracks regardless of their mute/solo state, but
            //muted channels should not be present in exported file -
            //apply channel mask to exclude them
            auto tracks = TrackList::Get(mProject).Any<WaveTrack>();
            std::vector<bool> channelMask(
                tracks.sum([](const auto track) { return track->NChannels(); }),
                false);
            unsigned trackIndex = 0;
            for (const auto track : tracks) {
                if (track->GetSolo()) {
                    channelMask.assign(channelMask.size(), false);
                    for (unsigned i = 0; i < track->NChannels(); ++i) {
                        channelMask[trackIndex++] = true;
                    }
                    break;
                }
                if (!track->GetMute() && (!selectedOnly || track->GetSelected())) {
                    for (unsigned i = 0; i < track->NChannels(); ++i) {
                        channelMask[trackIndex++] = true;
                    }
                } else {
                    trackIndex += track->NChannels();
                }
            }

            tempMixerSpec = std::make_unique<MixerOptions::Downmix>(*mExportOptionsPanel->GetMixerSpec(), channelMask);
            builder.SetMixerSpec(tempMixerSpec.get());
        } else {
            builder.SetNumChannels(channels);
        }

        ExportProgressUI::ExceptionWrappedCall([&]
        {
            result = ExportProgressUI::Show(builder.Build(mProject));
        });
    }

    if (result == ExportResult::Success || result == ExportResult::Stopped) {
        ImportExport::Get(mProject).SetPreferredExportRate(mExportOptionsPanel->GetSampleRate());

        ExportAudioDefaultFormat.Write(selectedPlugin->GetFormatInfo(selectedFormat).format);
        ExportAudioDefaultPath.Write(mExportOptionsPanel->GetPath());

        ShuttleGui S(this, eIsSavingToPrefs);
        PopulateOrExchange(S);
        event.Skip();
    }
}

void ExportAudioDialog::OnFormatChange(wxCommandEvent& event)
{
    Layout();
    Fit();

    auto enableMeta = false;
    if (auto plugin = mExportOptionsPanel->GetPlugin()) {
        enableMeta = plugin->GetFormatInfo(mExportOptionsPanel->GetFormat()).canMetaData;
    }
    mEditMetadata->Enable(enableMeta);
    mExportSettingsDirty = true;
}

void ExportAudioDialog::UpdateExportSettings()
{
    if (!mExportSettingsDirty) {
        return;
    }

    const auto selectedPlugin = mExportOptionsPanel->GetPlugin();
    if (selectedPlugin == nullptr) {
        return;
    }

    const auto selectedFormat = mExportOptionsPanel->GetFormat();

    if (mRangeSplit->GetValue()) {
        const auto byName = mSplitUseName->GetValue() || mSplitUseNumAndName->GetValue();
        const auto addNumber = mSplitUseNumAndName->GetValue();
        const auto prefix = mSplitFileNamePrefix->GetValue();

        if (mSplitByLabels->GetValue()) {
            UpdateLabelExportSettings(*selectedPlugin, selectedFormat, byName, addNumber, prefix);
        } else if (mSplitByTracks->GetValue()) {
            UpdateTrackExportSettings(*selectedPlugin, selectedFormat, byName, addNumber, prefix);
        }

        mExportSettingsDirty = false;
    }
}

void ExportAudioDialog::UpdateLabelExportSettings(const ExportPlugin& plugin, int formatIndex, bool byName, bool addNumber,
                                                  const wxString& prefix)
{
    const auto& tracks = TrackList::Get(mProject);
    const auto& labels = (*tracks.Any<const LabelTrack>().begin());
    const auto numLabels = labels->GetNumLabels();

    auto numFiles = numLabels;
    auto fileIndex = 0;       // counter for files done
    std::vector<ExportSetting> exportSettings; // dynamic array for settings.
    exportSettings.reserve(numFiles); // Allocate some guessed space to use.

    // Account for exporting before first label
    if (mIncludeAudioBeforeFirstLabel->GetValue()) {
        fileIndex = -1;
        numFiles++;
    }

    const auto formatInfo = plugin.GetFormatInfo(formatIndex);

    FilePaths otherNames; // keep track of file names we will use, so we
    // don't duplicate them
    ExportSetting setting;  // the current batch of settings
    setting.filename.SetPath(mExportOptionsPanel->GetPath());
    setting.channels = mExportOptionsPanel->GetChannels();
    setting.filename.SetFullName(mExportOptionsPanel->GetFullName());

    wxString name;   // used to hold file name whilst we mess with it
    wxString title;  // un-messed-with title of file for tagging with

    const LabelStruct* info = NULL;
    /* Examine all labels a first time, sort out all data but don't do any
     * exporting yet (so this run is quick but interactive) */
    while (fileIndex < numLabels) {
        // Get file name and starting time
        if (fileIndex < 0) {
            // create wxFileName for output file
            name = setting.filename.GetName();
            setting.t0 = 0.0;
        } else {
            info = labels->GetLabel(fileIndex);
            name = (info->title);
            setting.t0 = info->selectedRegion.t0();
        }

        // Figure out the ending time
        if (info && !info->selectedRegion.isPoint()) {
            setting.t1 = info->selectedRegion.t1();
        } else if (fileIndex < numLabels - 1) {
            // Use start of next label as end
            const LabelStruct* info1 = labels->GetLabel(fileIndex + 1);
            setting.t1 = info1->selectedRegion.t0();
        } else {
            setting.t1 = tracks.GetEndTime();
        }

        if (name.empty()) {
            name = _("untitled");
        }

        // store title of label to use in tags
        title = name;

        // Numbering files...
        if (!byName) {
            name.Printf(wxT("%s-%02d"), prefix, fileIndex + 1);
        } else if (addNumber) {
            // Following discussion with GA, always have 2 digits
            // for easy file-name sorting (on Windows)
            name.Prepend(wxString::Format(wxT("%02d-"), fileIndex + 1));
        }
        Internat::SanitiseFilename(name, wxT("_"));

        setting.filename.SetName(name);
        {
            // FIXME: TRAP_ERR User could have given an illegal filename prefix.
            // in that case we should tell them, not fail silently.
            wxASSERT(setting.filename.IsOk());  // burp if file name is broke

            // Make sure the (final) file name is unique within the set of exports
            FileNames::MakeNameUnique(otherNames, setting.filename);

            /* do the metadata for this file */
            // copy project metadata to start with
            if (exportSettings.empty()) {
                setting.tags = Tags::Get(mProject);
                setting.tags.LoadDefaults();
            } else {
                setting.tags = exportSettings.back().tags;
            }
            // over-ride with values
            setting.tags.SetTag(TAG_TITLE, title);
            setting.tags.SetTag(TAG_TRACK, fileIndex + 1);
        }

        /* add the settings to the array of settings to be used for export */
        exportSettings.push_back(setting);

        fileIndex++; // next label, count up one
    }
    std::swap(mExportSettings, exportSettings);
}

void ExportAudioDialog::UpdateTrackExportSettings(const ExportPlugin& plugin, int formatIndex, bool byName, bool addNumber,
                                                  const wxString& prefix)
{
    auto& tracks = TrackList::Get(mProject);

    bool anySolo = !((tracks.Any<const WaveTrack>() + &WaveTrack::GetSolo).empty());

    auto waveTracks = tracks.Any<WaveTrack>()
                      - (anySolo ? &WaveTrack::GetNotSolo : &WaveTrack::GetMute);

    const auto numWaveTracks = waveTracks.size();

    auto fileIndex = 0;    // track counter
    FilePaths otherNames;
    auto formatInfo = plugin.GetFormatInfo(formatIndex);
    std::vector<ExportSetting> exportSettings; // dynamic array we will use to store the
    // settings needed to do the exports with in
    exportSettings.reserve(numWaveTracks);  // Allocate some guessed space to use.
    ExportSetting setting;  // the current batch of settings
    setting.filename.SetPath(mExportOptionsPanel->GetPath());
    setting.filename.SetExt(wxFileName { mExportOptionsPanel->GetFullName() }.GetExt());

    wxString name;   // used to hold file name whilst we mess with it
    wxString title;  // un-messed-with title of file for tagging with

    const auto skipSilenceAtBeginning = mSkipSilenceAtBeginning->GetValue();

    /* Examine all tracks in turn, collecting export information */
    for (auto tr : waveTracks) {
        // Get the times for the track
        setting.t0 = skipSilenceAtBeginning ? tr->GetStartTime() : 0;
        setting.t1 = tr->GetEndTime();

        setting.channels = mExportOptionsPanel->GetChannels();
        // Get name and title
        title = tr->GetName();
        if (title.empty()) {
            title = _("untitled");
        }

        if (byName) {
            name = title;
            if (addNumber) {
                name.Prepend(
                    wxString::Format(wxT("%02d-"), fileIndex + 1));
            }
        } else {
            name = (wxString::Format(wxT("%s-%02d"), prefix, fileIndex + 1));
        }

        Internat::SanitiseFilename(name, wxT("_"));
        // store sanitised and user checked name in object
        setting.filename.SetName(name);

        // FIXME: TRAP_ERR User could have given an illegal track name.
        // in that case we should tell them, not fail silently.
        wxASSERT(setting.filename.IsOk());   // burp if file name is broke

        // Make sure the (final) file name is unique within the set of exports
        FileNames::MakeNameUnique(otherNames, setting.filename);

        /* do the metadata for this file */
        // copy project metadata to start with

        if (exportSettings.empty()) {
            setting.tags = Tags::Get(mProject);
            setting.tags.LoadDefaults();
        } else {
            setting.tags = exportSettings.back().tags;
        }

        // over-ride with values
        setting.tags.SetTag(TAG_TITLE, title);
        setting.tags.SetTag(TAG_TRACK, fileIndex + 1);

        exportSettings.push_back(setting);

        fileIndex++; // next track, count up one
    }
    std::swap(mExportSettings, exportSettings);
}

ExportResult ExportAudioDialog::DoExportSplitByLabels(const ExportPlugin& plugin,
                                                      int formatIndex,
                                                      const ExportProcessor::Parameters& parameters,
                                                      FilePaths& exporterFiles)
{
    auto ok = ExportResult::Success;  // did it work?
    /* Go round again and do the exporting (so this run is slow but
     * non-interactive) */
    for (auto& activeSetting : mExportSettings) {
        /* get the settings to use for the export from the array */
        // Bug 1440 fix.
        if (activeSetting.filename.GetName().empty()) {
            continue;
        }

        // Export it
        ok = DoExport(plugin, formatIndex, parameters, activeSetting.filename, activeSetting.channels,
                      activeSetting.t0, activeSetting.t1, false, activeSetting.tags, exporterFiles);

        if (ok == ExportResult::Stopped) {
            AudacityMessageDialog dlgMessage(
                nullptr,
                XO("Continue to export remaining files?"),
                XO("Export"),
                wxYES_NO | wxNO_DEFAULT | wxICON_WARNING);
            if (dlgMessage.ShowModal() != wxID_YES) {
                // User decided not to continue - bail out!
                break;
            }
        } else if (ok != ExportResult::Success) {
            break;
        }
    }

    return ok;
}

ExportResult ExportAudioDialog::DoExportSplitByTracks(const ExportPlugin& plugin,
                                                      int formatIndex,
                                                      const ExportProcessor::Parameters& parameters,
                                                      FilePaths& exporterFiles)
{
    auto& tracks = TrackList::Get(mProject);

    bool anySolo
        =!((tracks.Any<const WaveTrack>() + &WaveTrack::GetSolo).empty());

    auto waveTracks = tracks.Any<WaveTrack>()
                      - (anySolo ? &WaveTrack::GetNotSolo : &WaveTrack::GetMute);

    auto& selectionState = SelectionState::Get(mProject);

    /* Remember which tracks were selected, and set them to deselected */
    SelectionStateChanger changer{ selectionState, tracks };
    for (auto tr : tracks.Selected<WaveTrack>()) {
        tr->SetSelected(false);
    }

    auto ok = ExportResult::Success;

    int count = 0;
    for (auto tr : waveTracks) {
        wxLogDebug("Get setting %i", count);
        /* get the settings to use for the export from the array */
        auto& activeSetting = mExportSettings[count];
        if (activeSetting.filename.GetName().empty()) {
            count++;
            continue;
        }

        /* Select the track */
        SelectionStateChanger changer2{ selectionState, tracks };
        tr->SetSelected(true);

        // Export the data. "channels" are per track.
        ok = DoExport(plugin, formatIndex, parameters, activeSetting.filename, activeSetting.channels,
                      activeSetting.t0, activeSetting.t1, true, activeSetting.tags, exporterFiles);

        if (ok == ExportResult::Stopped) {
            AudacityMessageDialog dlgMessage(
                nullptr,
                XO("Continue to export remaining files?"),
                XO("Export"),
                wxYES_NO | wxNO_DEFAULT | wxICON_WARNING);
            if (dlgMessage.ShowModal() != wxID_YES) {
                // User decided not to continue - bail out!
                break;
            }
        } else if (ok != ExportResult::Success) {
            break;
        }
        // increment export counter
        count++;
    }

    return ok;
}

ExportResult ExportAudioDialog::DoExport(const ExportPlugin& plugin,
                                         int formatIndex,
                                         const ExportProcessor::Parameters& parameters,
                                         const wxFileName& filename,
                                         int channels,
                                         double t0, double t1, bool selectedOnly,
                                         const Tags& tags,
                                         FilePaths& exportedFiles)
{
    wxFileName name;

    wxLogDebug(wxT("Doing multiple Export: File name \"%s\""), (filename.GetFullName()));
    wxLogDebug(wxT("Channels: %i, Start: %lf, End: %lf "), channels, t0, t1);
    if (selectedOnly) {
        wxLogDebug(wxT("Selected Region Only"));
    } else {
        wxLogDebug(wxT("Whole Project"));
    }

    wxFileName backup;
    if (mOverwriteExisting->GetValue()) {
        name = filename;
        backup.Assign(name);

        int suffix = 0;
        do {
            backup.SetName(name.GetName()
                           + wxString::Format(wxT("%d"), suffix));
            ++suffix;
        }while (backup.FileExists());
        ::wxRenameFile(filename.GetFullPath(), backup.GetFullPath());
    } else {
        name = filename;
        int i = 2;
        wxString base(name.GetName());
        while (name.FileExists()) {
            name.SetName(wxString::Format(wxT("%s-%d"), base, i++));
        }
    }

    bool success{ false };
    const wxString fullPath{ name.GetFullPath() };

    auto cleanup = finally([&] {
        if (backup.IsOk()) {
            if (success) {
                // Remove backup
                ::wxRemoveFile(backup.GetFullPath());
            } else {
                // Restore original
                ::wxRemoveFile(fullPath);
                ::wxRenameFile(backup.GetFullPath(), fullPath);
            }
        } else {
            if (!success) {
                // Remove any new, and only partially written, file.
                ::wxRemoveFile(fullPath);
            }
        }
    });

    auto result = ExportResult::Error;
    ExportProgressUI::ExceptionWrappedCall([&]
    {
        result = ExportProgressUI::Show(ExportTaskBuilder {}.SetPlugin(&plugin, formatIndex)
                                        .SetParameters(parameters)
                                        .SetRange(t0, t1, selectedOnly)
                                        .SetTags(&tags)
                                        .SetNumChannels(channels)
                                        .SetFileName(fullPath)
                                        .SetSampleRate(mExportOptionsPanel->GetSampleRate())
                                        .Build(mProject));
    });

    success = result == ExportResult::Success || result == ExportResult::Stopped;

    if (success) {
        exportedFiles.push_back(fullPath);
    }

    return result;
}
