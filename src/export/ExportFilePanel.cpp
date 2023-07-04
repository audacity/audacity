#include "ExportFilePanel.h"

#include <numeric>
#include <wx/stattext.h>
#include <wx/button.h>
#include <wx/choice.h>
#include <wx/textctrl.h>
#include <wx/radiobut.h>
#include <wx/wupdlock.h>

#include "Export.h"
#include "ExportMixerDialog.h"
#include "ProjectRate.h"
#include "Mix.h"
#include "WaveTrack.h"

#include "ShuttleGui.h"
#include "ExportOptionsHandler.h"

wxDEFINE_EVENT(AUDACITY_EXPORT_FORMAT_CHANGE_EVENT, wxCommandEvent);

namespace
{

const ExportOptionsEditor::SampleRateList
DefaultRates {
   8000,
   11025,
   16000,
   22050,
   32000,
   44100,
   48000,
   88200,
   96000,
   176400,
   192000,
   352800,
   384000
};

enum
{
   FolderBrowseID = wxID_HIGHEST,

   FormatID,
   
   AudioMixModeMonoID,
   AudioMixModeStereoID,
   AudioMixModeCustomID,
   
   AudioChannelsConfigureID,
   
   SampleRateID,
};

class CustomSampleRateDialog final : public wxDialogWrapper
{
   enum {
      CustomSampleRateID = wxID_HIGHEST
   };
public:
   CustomSampleRateDialog(wxWindow* parent, int defaultSampleRate = 44100)
      : wxDialogWrapper(parent, wxID_ANY, XO("Custom Sample Rate"), wxDefaultPosition, {-1, 160})
      , mSampleRate(defaultSampleRate)
   {
      ShuttleGui S(this, eIsCreating);
      S.SetBorder(5);
      S.StartHorizontalLay(wxEXPAND);
      {
         S.StartMultiColumn(2, wxALIGN_CENTER_VERTICAL);
         {
            S.Id(CustomSampleRateID).AddNumericTextBox(XO("New Sample Rate (Hz)"), wxString::Format("%d", mSampleRate), 0);
         }
         S.EndMultiColumn();
      }
      S.EndHorizontalLay();
      
      S.AddStandardButtons();
   }
   
   int GetSampleRate() const noexcept
   {
      return mSampleRate;
   }
   
private:
   
   void OnSampleRateChange(wxCommandEvent& event)
   {
      long rate;
      if(event.GetString().ToLong(&rate))
         mSampleRate = static_cast<int>(rate);
   }
   
   int mSampleRate;
   
   DECLARE_EVENT_TABLE();
};



}

BEGIN_EVENT_TABLE(CustomSampleRateDialog, wxDialogWrapper)
   EVT_TEXT(CustomSampleRateID, CustomSampleRateDialog::OnSampleRateChange)
END_EVENT_TABLE()

BEGIN_EVENT_TABLE(ExportFilePanel, wxPanelWrapper)
   EVT_BUTTON(FolderBrowseID, ExportFilePanel::OnFolderBrowse)

   EVT_CHOICE(FormatID, ExportFilePanel::OnFormatChange)

   EVT_RADIOBUTTON(AudioMixModeMonoID, ExportFilePanel::OnChannelsChange)
   EVT_RADIOBUTTON(AudioMixModeStereoID, ExportFilePanel::OnChannelsChange)
   EVT_RADIOBUTTON(AudioMixModeCustomID, ExportFilePanel::OnChannelsChange)

   EVT_BUTTON(AudioChannelsConfigureID, ExportFilePanel::OnChannelsConfigure)

   EVT_CHOICE(SampleRateID, ExportFilePanel::OnSampleRateChange)
END_EVENT_TABLE()


ExportFilePanel::ExportFilePanel(AudacityProject& project,
                                       bool monoStereoMode,
                                       wxWindow* parent,
                                       wxWindowID winid)
   : wxPanelWrapper(parent, winid)
   , mMonoStereoMode(monoStereoMode)
   , mProject(project)
   , mExporter(std::make_unique<Exporter>(project))
{
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
}

ExportFilePanel::~ExportFilePanel() = default;

void ExportFilePanel::PopulateOrExchange(ShuttleGui& S)
{
   TranslatableStrings formats;
   if(S.GetMode() == eIsCreating)
   {
      for(auto& plugin : mExporter->GetPlugins())
      {
         for(int formatIndex = 0; formatIndex < plugin->GetFormatCount(); ++formatIndex)
         {
            auto formatInfo = plugin->GetFormatInfo(formatIndex);
            formats.push_back(formatInfo.description);
         }
      }
   }

   S.SetBorder(5);
   S.StartMultiColumn(3, wxEXPAND);
   {
      S.SetStretchyCol(1);
      
      mFullName = S.AddTextBox(XO("File Name:"), {}, 0);
      S.AddSpace(1);
      
      mFolder = S.AddTextBox(XO("Folder:"), {}, 0);
      S.Id(FolderBrowseID).AddButton(XO("Browse..."));
      
      mFormat = S.Id(FormatID).AddChoice(XO("Format:"), formats);
      S.AddSpace(1);
   }
   S.EndMultiColumn();
   
   S.SetBorder(5);
   S.StartStatic(XO("Audio options"));
   {
      S.StartTwoColumn();
      {
         if(auto prompt = S.AddPrompt(XO("Channels")))
            prompt->SetMinSize({140, -1});

         S.StartHorizontalLay(wxALIGN_LEFT);
         {
            S.SetBorder(2);

            const int channels = 2;

            mMono = S.Id(AudioMixModeMonoID).AddRadioButton(XO("Mono"), 1, channels);
            mStereo = S.Id(AudioMixModeStereoID).AddRadioButtonToGroup(XO("Stereo"), 2, channels);
            if(!mMonoStereoMode)
            {
               mCustomMapping = S.Id(AudioMixModeCustomID).AddRadioButtonToGroup(XO("Custom mapping"), 0, true);
               S.Id(AudioChannelsConfigureID).AddButton(XO("Configure"));
            }
         }
         S.EndHorizontalLay();

         S.SetBorder(5);

         if(auto prompt = S.AddPrompt(XO("Sample Rate")))
            prompt->SetMinSize({140, -1});

         S.StartHorizontalLay(wxALIGN_LEFT);
         {
            mRates = S.Id(SampleRateID).AddChoice({}, {});
         }
         S.EndHorizontalLay();
      }
      S.EndTwoColumn();
      
      mAudioOptionsPanel = S.StartPanel();
      {
         
      }
      S.EndPanel();
   }
   S.EndStatic();
}

void ExportFilePanel::Init(const wxFileName& filename,
                           const wxString& format,
                           int sampleRate,
                           int channels,
                           const ExportProcessor::Parameters& parameters,
                           const MixerOptions::Downmix* mixerSpec)
{
   mFolder->SetValue(filename.GetPath());
   mFullName->SetValue(filename.GetFullName());
   mSampleRate = sampleRate == 0
      ? ProjectRate::Get(mProject).GetRate()
      : sampleRate;

   auto selectedFormatIndex = 0;
   if(!format.empty())
   {
      auto counter = 0;
      auto formatFound = false;
      for(auto& p : mExporter->GetPlugins())
      {
         for(unsigned i = 0; i < p->GetFormatCount(); ++i)
         {
            if(format == p->GetFormatInfo(i).format)
            {
               selectedFormatIndex = counter;
               formatFound = true;
               break;
            }
            ++counter;
         }
         if(formatFound)
            break;
      }
   }

   if(mixerSpec != nullptr)
      *mMixerSpec = *mixerSpec;

   mFormat->SetSelection(selectedFormatIndex);
   ChangeFormat(selectedFormatIndex);
   if(!parameters.empty())
      mOptionsHandler->SetParameters(parameters);

   switch(channels)
   {
   case 0 : {
      if(!mMonoStereoMode)
         mCustomMapping->SetValue(true);
   } break;
   case 1 : mMono->SetValue(true); break;
   case 2 : mStereo->SetValue(true); break;
   default: break;
   }
}

wxString ExportFilePanel::GetPath() const
{
   return mFolder->GetValue();
}

wxString ExportFilePanel::GetFullName() const
{
   return mFullName->GetValue();
}

const ExportPlugin* ExportFilePanel::GetPlugin() const
{
   return mSelectedPlugin;
}

int ExportFilePanel::GetPluginIndex() const
{
   return mSelectedPluginIndex;
}

int ExportFilePanel::GetFormat() const
{
   return mSelectedFormatIndex;
}


int ExportFilePanel::GetSampleRate() const
{
   return mSampleRate;
}

ExportProcessor::Parameters ExportFilePanel::GetParameters() const
{
   mOptionsHandler->TransferDataFromEditor();
   return mOptionsHandler->GetParameters();
}

int ExportFilePanel::GetChannels() const
{
   if(mCustomMapping != nullptr && mCustomMapping->GetValue())
      return 0;
   return mMono->GetValue() ? 1 : 2;
}

MixerOptions::Downmix* ExportFilePanel::GetMixerSpec() const
{
   return mMixerSpec.get();
}

void ExportFilePanel::OnFormatChange(wxCommandEvent &event)
{
   ChangeFormat(event.GetInt());
   event.Skip();
}

void ExportFilePanel::OnSampleRateChange(wxCommandEvent &event)
{
   const auto clientData = event.GetClientData();
   if(clientData == nullptr)
   {
      CustomSampleRateDialog dialog(this, mSampleRate);
      if(dialog.ShowModal() == wxID_OK &&
         dialog.GetSampleRate() > 0)
      {
         mSampleRate = dialog.GetSampleRate();
      }
      UpdateSampleRateList();
   }
   else
      mSampleRate = *reinterpret_cast<const int*>(&clientData);
}

void ExportFilePanel::OnFolderBrowse(wxCommandEvent &event)
{
   wxDirDialogWrapper dirDialog(this,
                                XO("Choose a location to save the exported files"),
                                mFolder->GetValue());
   if(dirDialog.ShowModal() == wxID_OK)
      mFolder->SetValue(dirDialog.GetPath());
}

void ExportFilePanel::OnChannelsChange(wxCommandEvent& event)
{
   if(event.GetId() == AudioMixModeCustomID)
   {
      if(!mMixerSpec)
      {
         auto waveTracks = TrackList::Get(mProject).Leaders<const WaveTrack>();
         mMixerSpec = std::make_unique<MixerOptions::Downmix>(
            std::accumulate(
               waveTracks.begin(),
               waveTracks.end(),
               0, [](int a, const auto& track) { return a + track->NChannels(); }),
            MaxExportChannels);
      }
   }
}

void ExportFilePanel::OnChannelsConfigure(wxCommandEvent &event)
{
   //Configure for all tracks, but some channels may turn out to be silent
   //if exported region does not contain audio samples
   auto waveTracks = TrackList::Get(mProject).Leaders<const WaveTrack>();
   
   auto mixerSpec = mMixerSpec
      ? std::make_unique<MixerOptions::Downmix>(*mMixerSpec)
      : std::make_unique<MixerOptions::Downmix>(
            std::accumulate(
               waveTracks.begin(),
               waveTracks.end(),
               0, [](int a, const auto& track) { return a + track->NChannels(); }),
            MaxExportChannels);
   
   ExportMixerDialog md(waveTracks,
                        mixerSpec.get(),
                        nullptr,
                        1,
                        XO("Advanced Mixing Options"));
   if(md.ShowModal() == wxID_OK)
   {
      mMixerSpec.swap(mixerSpec);
      mCustomMapping->SetValue(true);
   }
}


void ExportFilePanel::ChangeFormat(int index)
{
   mSelectedPlugin = nullptr;

   wxWindowUpdateLocker wndupdlck(mAudioOptionsPanel);

   auto pluginCounter = 0;
   auto formatCounter = 0;
   for(const auto& plugin : mExporter->GetPlugins())
   {
      for(int formatIndex = 0; formatIndex < plugin->GetFormatCount(); ++formatIndex)
      {
         if(formatCounter == index)
         {
            mOptionsChangeSubscription.Reset();

            mSelectedPlugin = plugin.get();
            mSelectedPluginIndex = pluginCounter;
            mSelectedFormatIndex = formatIndex;
            
            auto formatInfo = plugin->GetFormatInfo(formatIndex);
            if(!formatInfo.extensions[0].empty())
            {
               wxFileName filename;
               filename.SetFullName(mFullName->GetValue());
               filename.SetExt(formatInfo.extensions[0]);
               mFullName->SetValue(filename.GetFullName());
            }
            mAudioOptionsPanel->SetSizer(nullptr);
            mAudioOptionsPanel->DestroyChildren();
            
            ShuttleGui S(mAudioOptionsPanel, eIsCreating, true, {1, 1});
            mOptionsHandler = std::make_unique<ExportOptionsHandler>(S, *plugin, formatIndex);
            mOptionsChangeSubscription = mOptionsHandler->Subscribe(*this, &ExportFilePanel::OnOptionsHandlerEvent);
            
            if(formatInfo.maxChannels < 2 && mStereo->GetValue())
               mMono->SetValue(true);
            mStereo->Enable(formatInfo.maxChannels > 1);
            
            const auto mixerMaxChannels = std::clamp(formatInfo.maxChannels,
                                                    // JKC: This is an attempt to fix a 'watching brief' issue, where the slider is
                                                    // sometimes not slidable.  My suspicion is that a mixer may incorrectly
                                                    // state the number of channels - so we assume there are always at least two.
                                                    // The downside is that if someone is exporting to a mono device, the dialog
                                                    // will allow them to output to two channels. Hmm.  We may need to revisit this.
                                                    // STF (April 2016): AMR (narrowband) and MP3 may export 1 channel.
                                                    1u,
                                                    MaxExportChannels);
            if(!mMixerSpec || mMixerSpec->GetMaxNumChannels() != mixerMaxChannels)
            {
               auto waveTracks = TrackList::Get(mProject).Leaders<const WaveTrack>();
               mMixerSpec = std::make_unique<MixerOptions::Downmix>(
                  std::accumulate(
                     waveTracks.begin(),
                     waveTracks.end(),
                     0, [](int sum, const auto& track) { return sum + track->NChannels(); }),
                  mixerMaxChannels);
            }
            
            UpdateSampleRateList();
            
            mAudioOptionsPanel->Layout();
            
            wxPostEvent(GetParent(), wxCommandEvent { AUDACITY_EXPORT_FORMAT_CHANGE_EVENT, GetId() });
            
            return;
         }
         ++formatCounter;
      }
   }
}

void ExportFilePanel::OnOptionsHandlerEvent(const ExportOptionsHandlerEvent &e)
{
   switch(e.type)
   {
   case ExportOptionsHandlerEvent::SampleRateListChange:
      UpdateSampleRateList();
      break;
   case ExportOptionsHandlerEvent::FileSuffixChange:
      UpdateFileNameExt(e.tag);
      break;
   }
   
}

void ExportFilePanel::UpdateFileNameExt(const wxString& ext)
{
   if(!ext.empty())
   {
      wxFileName filename;
      filename.SetFullName(mFullName->GetValue());
      filename.SetExt(ext.BeforeFirst(' ').Lower());
      mFullName->SetValue(filename.GetFullName());
   }
}

void ExportFilePanel::UpdateSampleRateList()
{
   auto availableRates = mOptionsHandler->GetSampleRateList();
   
   const auto* rates = availableRates.empty() ? &DefaultRates : &availableRates;
   
   mRates->Clear();
   
   void* clientData;
   const auto projectRate = static_cast<int>(ProjectRate::Get(mProject).GetRate());
   int customRate = mSampleRate;
   int selectedItemIndex = 0;
   int preferredItemIndex = 0;
   for(auto rate : *rates)
   {
      *reinterpret_cast<int*>(&clientData) = rate;
      const auto itemIndex =
         mRates->Append(
            XO("%d Hz").Format(rate).Translation(),
            clientData);
      if(rate == mSampleRate)
      {
         customRate = 0;
         selectedItemIndex = itemIndex;
      }
      if(rate <= projectRate)
         preferredItemIndex = itemIndex;
   }
   if(rates == &DefaultRates)
   {
      if(customRate != 0)
      {
         *reinterpret_cast<int*>(&clientData) = customRate;
         selectedItemIndex =
            mRates->Append(
               XO("%d Hz (custom)").Format(customRate).Translation(),
               clientData);
      }
      mRates->Append(_("Other..."));
   }
   else if(customRate != 0)//sample rate not in the list
   {
      mSampleRate = (*rates)[preferredItemIndex];
      selectedItemIndex = preferredItemIndex;
   }
   mRates->SetSelection(selectedItemIndex);
}
