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

namespace MixerOptions
{
class Downmix;
}

class Tags;

class ExportAudioDialog final : public wxDialogWrapper
{
public:
   ExportAudioDialog(wxWindow* parent,
                     AudacityProject& project,
                     const wxString& defaultName,
                     const wxString& defaultFormat);
   ~ExportAudioDialog() override;
private:
   
   void PopulateOrExchange(ShuttleGui& S);
   
   void OnExportRangeChange(wxCommandEvent& event);
   void OnSplitModeChange(wxCommandEvent& event);
   void OnSplitNamePolicyChange(wxCommandEvent& event);
   
   void OnEditMetadata(wxCommandEvent& event);
   
   void OnHelp(wxCommandEvent& event);
   
   void OnExport(wxCommandEvent& event);

   void OnFormatChange(wxCommandEvent& event);
   
   ExportResult DoExportSplitByLabels(const ExportPlugin& plugin,
                                      int formatIndex,
                                      const ExportProcessor::Parameters& parameters,
                                      bool byName,
                                      bool addNumber,
                                      const wxString& prefix,
                                      FilePaths& exporterFiles);
   
   ExportResult DoExportSplitByTracks(const ExportPlugin& plugin,
                                      int formatIndex,
                                      const ExportProcessor::Parameters& parameters,
                                      bool byName,
                                      bool addNumber,
                                      const wxString& prefix,
                                      FilePaths& exporterFiles);
   
   ExportResult DoExport(const ExportPlugin& plugin,
                         int formatIndex,
                         const ExportProcessor::Parameters& parameters,
                         const wxFileName& filename,
                         int channels,
                         double t0, double t1, bool selectedOnly,
                         const Tags& tags,
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
   
   DECLARE_EVENT_TABLE()
};

