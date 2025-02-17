#pragma once

#include "wxPanelWrapper.h"
#include "ExportPlugin.h"

class AudacityProject;
class ShuttleGui;
class ExportFilePanel;

class TimerRecordExportDialog final : public wxDialogWrapper
{
    enum
    {
        ExportFilePanelID = wxID_HIGHEST,
        EditMetadataID
    };
public:
    TimerRecordExportDialog(AudacityProject& project, wxWindow* parent = nullptr, wxWindowID = wxID_ANY);

    void Bind(wxFileName& filename, wxString& format, int& sampleRate, int& channels, ExportProcessor::Parameters& paramters);

private:
    void PopulateOrExchange(ShuttleGui& S);

    void OnOK(wxCommandEvent&);
    void OnEditMetadata(wxCommandEvent&);
    void OnFormatChanged(wxCommandEvent&);

    AudacityProject& mProject;

    wxButton* mEditMetadata{};
    ExportFilePanel* mExportFilePanel{ nullptr };

    wxFileName* mFileName{};
    wxString* mFormat{};
    int* mSampleRate{};
    int* mChannels{};
    ExportProcessor::Parameters* mParameters{};

    DECLARE_EVENT_TABLE()
};
