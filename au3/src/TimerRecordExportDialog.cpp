#include "TimerRecordExportDialog.h"

#include <wx/button.h>

#include "ShuttleGui.h"
#include "export/ExportFilePanel.h"
#include "Export.h"
#include "TagsEditor.h"

BEGIN_EVENT_TABLE(TimerRecordExportDialog, wxDialogWrapper)
EVT_BUTTON(wxID_OK, TimerRecordExportDialog::OnOK)
EVT_BUTTON(EditMetadataID, TimerRecordExportDialog::OnEditMetadata)
EVT_COMMAND(ExportFilePanelID, AUDACITY_EXPORT_FORMAT_CHANGE_EVENT, TimerRecordExportDialog::OnFormatChanged)
END_EVENT_TABLE()

TimerRecordExportDialog::TimerRecordExportDialog(AudacityProject& project,
                                                 wxWindow* parent,
                                                 wxWindowID winid)
    : wxDialogWrapper(parent, winid, XO("Export Audio"))
    , mProject(project)
{
    SetMinSize({ 450, -1 });
    ShuttleGui S(this, eIsCreating);
    PopulateOrExchange(S);
}

void TimerRecordExportDialog::Bind(wxFileName& filename,
                                   wxString& format,
                                   int& sampleRate,
                                   int& channels,
                                   ExportProcessor::Parameters& parameters)
{
    mFileName = &filename;
    mFormat = &format;
    mSampleRate = &sampleRate;
    mChannels = &channels;
    mParameters = &parameters;
    mExportFilePanel->Init(filename, sampleRate, format, channels, parameters);
}

void TimerRecordExportDialog::PopulateOrExchange(ShuttleGui& S)
{
    S.SetBorder(5);
    S.StartVerticalLay();
    {
        if (S.GetMode() == eIsCreating) {
            mExportFilePanel = safenew ExportFilePanel(mProject, true, this, ExportFilePanelID);
            S.AddWindow(mExportFilePanel, wxEXPAND);
        }

        S.StartHorizontalLay(wxEXPAND);
        {
            mEditMetadata = S.Id(EditMetadataID).AddButton(XO("Edit &Metadata..."), wxLEFT | wxBOTTOM);
            S.AddSpace(1, 1, wxEXPAND);
            S.Id(wxID_CANCEL).AddButton(XO("&Cancel"), wxBOTTOM);
            S.Id(wxID_OK).AddButton(XO("O&K"), wxRIGHT | wxBOTTOM, true);
        }
        S.EndHorizontalLay();
    }
    S.EndVerticalLay();
}

void TimerRecordExportDialog::OnOK(wxCommandEvent& event)
{
    if (const auto parameters = mExportFilePanel->GetParameters()) {
        *mParameters = std::move(*parameters);
    } else {
        return;
    }

    *mFileName = wxFileName(mExportFilePanel->GetPath(), mExportFilePanel->GetFullName());
    *mFormat = mExportFilePanel->GetPlugin()->GetFormatInfo(mExportFilePanel->GetFormat()).format;
    *mSampleRate = mExportFilePanel->GetSampleRate();
    *mChannels = mExportFilePanel->GetChannels();

    event.Skip();
}

void TimerRecordExportDialog::OnEditMetadata(wxCommandEvent&)
{
    TagsEditorDialog::EditProjectMetadata(mProject,
                                          XO("Edit Metadata Tags"),
                                          XO("Exported Tags"));
}

void TimerRecordExportDialog::OnFormatChanged(wxCommandEvent&)
{
    Fit();
    Layout();

    auto enableMeta = false;
    if (auto plugin = mExportFilePanel->GetPlugin()) {
        enableMeta = plugin->GetFormatInfo(mExportFilePanel->GetFormat()).canMetaData;
    }
    mEditMetadata->Enable(enableMeta);
}
