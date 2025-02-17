/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportAudioDialog.h

  Vitaly Sverchinsky

**********************************************************************/

#pragma once

#include "wxPanelWrapper.h"
#include "ExportTypes.h"
#include <wx/filename.h>

#include "ExportPlugin.h"
#include "Tags.h"

class ExportFilePanel;
class AudacityProject;
class ShuttleGui;

class Exporter;
class ExportPlugin;
class ExportTaskBuilder;

class ExportOptionsHandler;
class ExportOptionsHandlerEvent;

class wxTextCtrl;
class wxButton;
class wxChoice;
class wxRadioButton;
class wxCheckBox;

namespace MixerOptions {
class Downmix;
}

class ExportAudioDialog final : public wxDialogWrapper
{
    ///\brief A private class used to store the information needed to do an export.
    struct ExportSetting
    {
        double t0; /**< Start time for the export */
        double t1; /**< End time for the export */
        wxFileName filename; /**< The file to export to */
        unsigned channels; /**< Number of channels for ExportMultipleByTrack */
        Tags tags; /**< The set of metadata to use for the export */
    };

public:
    enum class ExportMode
    {
        Auto,
        SelectedOnly
    };

    ExportAudioDialog(wxWindow* parent, AudacityProject& project, const wxString& defaultName, const wxString& defaultFormat,
                      ExportMode mode = ExportMode::Auto);
    ~ExportAudioDialog() override;

    bool Show(bool show = true) override;
private:

    void PopulateOrExchange(ShuttleGui& S);

    void OnExportRangeChange(wxCommandEvent& event);
    void OnSplitModeChange(wxCommandEvent& event);
    void OnSplitNamePolicyChange(wxCommandEvent& event);

    void OnTrimBlankSpaceBeforeFirstClip(wxCommandEvent&);

    void OnIncludeAudioBeforeFirstLabelChange(wxCommandEvent&);

    void OnFileNamePrefixChange(wxCommandEvent&);

    void OnEditMetadata(wxCommandEvent& event);

    void OnHelp(wxCommandEvent& event);

    void OnExport(wxCommandEvent& event);

    void OnFormatChange(wxCommandEvent& event);

    void UpdateExportSettings();
    void UpdateLabelExportSettings(const ExportPlugin& plugin, int formatIndex, bool byName, bool addNumber, const wxString& prefix);
    void UpdateTrackExportSettings(const ExportPlugin& plugin, int formatIndex, bool byName, bool addNumber, const wxString& prefix);

    ExportResult DoExportSplitByLabels(const ExportPlugin& plugin, int formatIndex, const ExportProcessor::Parameters& parameters,
                                       FilePaths& exporterFiles);

    ExportResult DoExportSplitByTracks(const ExportPlugin& plugin, int formatIndex, const ExportProcessor::Parameters& parameters,
                                       FilePaths& exporterFiles);

    ExportResult DoExport(const ExportPlugin& plugin, int formatIndex, const ExportProcessor::Parameters& parameters,
                          const wxFileName& filename, int channels, double t0, double t1, bool selectedOnly, const Tags& tags,
                          FilePaths& exportedFiles);

    AudacityProject& mProject;

    ExportFilePanel* mExportOptionsPanel{};

    wxWindow* mSplitsPanel{};
    wxCheckBox* mIncludeAudioBeforeFirstLabel{};
    wxTextCtrl* mSplitFileNamePrefix{};
    wxButton* mEditMetadata{};
    wxRadioButton* mRangeProject;
    wxRadioButton* mRangeSelection{};
    wxRadioButton* mRangeSplit{};
    wxRadioButton* mSplitByLabels{};
    wxRadioButton* mSplitByTracks{};
    wxRadioButton* mExportRangeSelection{};
    wxRadioButton* mSplitModeTracks{};
    wxRadioButton* mSplitModeLabels{};
    wxRadioButton* mSplitUseName{};
    wxRadioButton* mSplitUseNumAndName{};
    wxRadioButton* mSplitUseNumAndPrefix{};
    wxCheckBox* mOverwriteExisting{};
    wxCheckBox* mSkipSilenceAtBeginning{};

    std::vector<ExportSetting> mExportSettings;
    bool mExportSettingsDirty{ true };

    DECLARE_EVENT_TABLE()
};
