#pragma once

#include <memory>

#include <wx/event.h>

#include "Observer.h"
#include "ExportPlugin.h"

#include "wxPanelWrapper.h"

class ExportOptionsHandlerEvent;
class ShuttleGui;
class AudacityProject;

class Exporter;
class ExportOptionsHandler;

class wxWindow;
class wxChoice;
class wxTextCtrl;
class wxRadioButton;

wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API,
   AUDACITY_EXPORT_FORMAT_CHANGE_EVENT, wxCommandEvent);

class ExportFilePanel final : public wxPanelWrapper
{
   constexpr static auto MaxExportChannels = 32u;
public:

   ExportFilePanel(AudacityProject& project,
                      bool monoStereoMode = false,
                      wxWindow* parent = nullptr,
                      wxWindowID winid = wxID_ANY);
   ~ExportFilePanel() override; 

   void PopulateOrExchange(ShuttleGui& S);

   void Init(const wxFileName& filename,
             const wxString& format = wxEmptyString,
             int sampleRate = 44100,
             int channels = 1,
             const ExportProcessor::Parameters& parameters = {},
             const MixerOptions::Downmix* mixerSpec = nullptr);
   
   wxString GetPath() const;
   wxString GetFullName() const;

   const ExportPlugin* GetPlugin() const;
   int GetPluginIndex() const;
   int GetFormat() const;
   int GetSampleRate() const;
   ExportProcessor::Parameters GetParameters() const;
   int GetChannels() const;

   MixerOptions::Downmix* GetMixerSpec() const;

private:

   void OnFormatChange(wxCommandEvent& event);
   void OnSampleRateChange(wxCommandEvent& event);
   
   void OnFolderBrowse(wxCommandEvent& event);

   void OnChannelsChange(wxCommandEvent& event);
   void OnChannelsConfigure(wxCommandEvent& event);

   void ChangeFormat(int index);
   
   void OnOptionsHandlerEvent(const ExportOptionsHandlerEvent& e);
   
   void UpdateSampleRateList();
   void UpdateFileNameExt(const wxString& ext);

   AudacityProject& mProject;

   bool mMonoStereoMode{false};

   wxChoice* mFormat{};
   wxChoice* mRates{};
   wxWindow* mAudioOptionsPanel{};

   wxTextCtrl* mFolder{};
   wxTextCtrl* mFullName{};

   wxRadioButton* mMono{};
   wxRadioButton* mStereo{};
   wxRadioButton* mCustomMapping{};

   int mSampleRate{};
   const ExportPlugin* mSelectedPlugin{};
   int mSelectedPluginIndex{};
   int mSelectedFormatIndex{};

   std::unique_ptr<Exporter> mExporter;
   std::unique_ptr<ExportOptionsHandler> mOptionsHandler;
   std::unique_ptr<MixerOptions::Downmix> mMixerSpec;

   Observer::Subscription mOptionsChangeSubscription;

   DECLARE_EVENT_TABLE()
};
