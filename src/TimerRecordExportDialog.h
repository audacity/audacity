#pragma once

#include "wxPanelWrapper.h"

class AudacityProject;
class Exporter;
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
   TimerRecordExportDialog(AudacityProject& project,
                     Exporter& exporter,
                     wxWindow* parent = nullptr,
                     wxWindowID = wxID_ANY);

private:
   void PopulateOrExchange(ShuttleGui& S);

   void OnOK(wxCommandEvent&);
   void OnEditMetadata(wxCommandEvent&);
   void OnFormatChanged(wxCommandEvent&);

   AudacityProject& mProject;
   Exporter& mExporter;

   wxButton* mEditMetadata{};
   ExportFilePanel* mExportFilePanel{nullptr};

   DECLARE_EVENT_TABLE()
};
