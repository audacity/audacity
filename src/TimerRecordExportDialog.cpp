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
                                     Exporter& exporter,
                                     wxWindow* parent,
                                     wxWindowID winid)
   : wxDialogWrapper(parent, winid, XO("Export Audio"))
   , mProject(project)
   , mExporter(exporter)
{
   SetMinSize({450, -1});
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);

   mExportFilePanel->Init(
      exporter.GetAutoExportFileName(),
      exporter.GetAutoExportSampleRate(),
      exporter.GetAutoExportParameters(),
      exporter.GetPlugins()[exporter.GetAutoExportFormat()]->GetFormatInfo(exporter.GetAutoExportSubFormat()).format);

   Layout();
   Fit();
}

void TimerRecordExportDialog::PopulateOrExchange(ShuttleGui& S)
{
   S.SetBorder(5);
   S.StartVerticalLay();
   {
      if(S.GetMode() == eIsCreating)
      {
         mExportFilePanel = safenew ExportFilePanel(mProject, true, this, ExportFilePanelID);
         S.AddWindow(mExportFilePanel, wxEXPAND);
      }

      S.StartHorizontalLay(wxEXPAND);
      {
         mEditMetadata = S.Id(EditMetadataID).AddButton(XO("Edit &Metadata..."), wxLEFT | wxBOTTOM);
         S.AddSpace(1, 1, wxEXPAND);
         S.Id(wxID_CANCEL).AddButton(XO("&Cancel"), wxBOTTOM);
         S.Id(wxID_OK).AddButton(XO("&OK"), wxRIGHT | wxBOTTOM, true);
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();
}

void TimerRecordExportDialog::OnOK(wxCommandEvent& event)
{
   mExporter.Configure(
      wxFileName(mExportFilePanel->GetPath(), mExportFilePanel->GetFullName()),
      mExportFilePanel->GetPluginIndex(),
      mExportFilePanel->GetFormat(),
      mExportFilePanel->GetSampleRate(),
      mExportFilePanel->GetParameters());
   event.Skip();
}

void TimerRecordExportDialog::OnEditMetadata(wxCommandEvent&)
{
   TagsEditorDialog::DoEditMetadata( mProject,
      XO("Edit Metadata Tags"),
      XO("Exported Tags"),
      true);
}

void TimerRecordExportDialog::OnFormatChanged(wxCommandEvent&)
{
   Fit();
   Layout();

   auto enableMeta = false;
   if(auto plugin = mExportFilePanel->GetPlugin())
      enableMeta = plugin->GetFormatInfo(mExportFilePanel->GetFormat()).canMetaData;
   mEditMetadata->Enable(enableMeta);
}
