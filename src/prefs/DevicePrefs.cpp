/**********************************************************************

  Audacity: A Digital Audio Editor

  DevicePrefs.cpp

  Joshua Haberman
  Dominic Mazzoni
  James Crook

*******************************************************************//**

\class DevicePrefs
\brief A PrefsPanel used to select recording and playback devices and
other settings.

  Presents interface for user to select the recording device and
  playback device, from the list of choices that PortAudio
  makes available.

  Also lets user decide how many channels to record.

*//********************************************************************/


#include "DevicePrefs.h"
#include "AudioIOBase.h"

#include "RecordingPrefs.h"

#include <wx/defs.h>

#include <wx/choice.h>
#include <wx/combobox.h>
#include <wx/log.h>
#include <wx/textctrl.h>

#include "portaudio.h"

#include "Prefs.h"
#include "ShuttleGui.h"
#include "DeviceManager.h"
#include "ProjectRate.h"

enum {
   HostID = 10000,
   PlayID,
   RecordID,
   ChannelsID
};

BEGIN_EVENT_TABLE(DevicePrefs, PrefsPanel)
   EVT_CHOICE(HostID, DevicePrefs::OnHost)
   EVT_CHOICE(RecordID, DevicePrefs::OnDevice)
END_EVENT_TABLE()

DevicePrefs::DevicePrefs(wxWindow * parent, wxWindowID winid, AudacityProject* project)
:  PrefsPanel(parent, winid, XO("Devices"))
, mProject(project)
{
   Populate();
}

DevicePrefs::~DevicePrefs()
{
}


ComponentInterfaceSymbol DevicePrefs::GetSymbol() const
{
   return DEVICE_PREFS_PLUGIN_SYMBOL;
}

TranslatableString DevicePrefs::GetDescription() const
{
   return XO("Preferences for Device");
}

ManualPageID DevicePrefs::HelpPageName()
{
   return "Devices_Preferences";
}

void DevicePrefs::Populate()
{
   // First any pre-processing for constructing the GUI.
   GetNamesAndLabels();

   // Get current setting for devices
   mPlayDevice = AudioIOPlaybackDevice.Read();
   mRecordDevice = AudioIORecordingDevice.Read();
   mRecordSource = AudioIORecordingSource.Read();
   mRecordChannels = AudioIORecordChannels.Read();

   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------

   wxCommandEvent e;
   OnHost(e);
}


/*
 * Get names of device hosts.
 */
void DevicePrefs::GetNamesAndLabels()
{
   // Gather list of hosts.  Only added hosts that have devices attached.
   // FIXME: TRAP_ERR PaErrorCode not handled in DevicePrefs GetNamesAndLabels()
   // With an error code won't add hosts, but won't report a problem either.
   int nDevices = Pa_GetDeviceCount();
   for (int i = 0; i < nDevices; i++) {
      const PaDeviceInfo *info = Pa_GetDeviceInfo(i);
      if ((info!=NULL)&&(info->maxOutputChannels > 0 || info->maxInputChannels > 0)) {
         wxString name = wxSafeConvertMB2WX(Pa_GetHostApiInfo(info->hostApi)->name);
         if (!make_iterator_range(mHostNames)
            .contains( Verbatim( name ) )) {
            mHostNames.push_back( Verbatim( name ) );
            mHostLabels.push_back(name);
         }
      }
   }
}

void DevicePrefs::PopulateOrExchange(ShuttleGui & S)
{
   ChoiceSetting HostSetting{
      AudioIOHost,
      { ByColumns, mHostNames, mHostLabels }
   };
   S.SetBorder(2);
   S.StartScroller();

   /* i18n-hint Software interface to audio devices */
   S.StartStatic(XC("Interface", "device"));
   {
      S.StartMultiColumn(2);
      {
         S.Id(HostID);
         mHost = S.TieChoice( XXO("&Host:"), HostSetting);

         S.AddPrompt(XXO("Using:"));
         S.AddFixedText( Verbatim(wxSafeConvertMB2WX(Pa_GetVersionText() ) ) );
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   S.StartStatic(XO("Playback"));
   {
      S.StartMultiColumn(2);
      {
         S.Id(PlayID);
         mPlay = S.AddChoice(XXO("&Device:"),
                             {} );
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   // i18n-hint: modifier as in "Recording preferences", not progressive verb
   S.StartStatic(XC("Recording", "preference"));
   {
      S.StartMultiColumn(2);
      {
         S.Id(RecordID);
         mRecord = S.AddChoice(XXO("De&vice:"),
                               {} );

         S.Id(ChannelsID);
         mChannels = S.AddChoice(XXO("Cha&nnels:"),
                                 {} );
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   // These previously lived in recording preferences.
   // However they are liable to become device specific.
   // Buffering also affects playback, not just recording, so is a device characteristic.
   S.StartStatic( XO("Latency"));
   {
      S.StartThreeColumn();
      {
         wxTextCtrl *w;
         // only show the following controls if we use Portaudio v19, because
         // for Portaudio v18 we always use default buffer sizes
         w = S
            .NameSuffix(XO("milliseconds"))
            .TieNumericTextBox(XXO("&Buffer length:"),
                                 AudioIOLatencyDuration,
                                 25);
         S.AddUnits(XO("milliseconds"));

         w = S
            .NameSuffix(XO("milliseconds"))
            .TieNumericTextBox(XXO("&Latency compensation:"),
               AudioIOLatencyCorrection, 25);
         S.AddUnits(XO("milliseconds"));
      }
      S.EndThreeColumn();
   }
   S.EndStatic();

   if (mProject != nullptr)
   {
      S.StartStatic(XO("Quality"));
      {
         S.StartThreeColumn();

         auto cmbProjectRate = S.AddCombo(XO("Project rate:"), {}, {});

         for (auto i = 0; i < AudioIOBase::NumStandardRates; ++i)
            cmbProjectRate->Append(
               wxString::Format("%d", AudioIOBase::StandardRates[i]));

         cmbProjectRate->SetValue(wxString::Format(
            "%d", static_cast<int>(ProjectRate::Get(*mProject).GetRate())));

         cmbProjectRate->Bind(
            wxEVT_COMBOBOX,
            [this, cmbProjectRate](auto)
            {
               long value;
               if (cmbProjectRate->GetValue().ToLong(&value))
                  ProjectRate::Get(*mProject).SetRate(value);
            });

         S.AddUnits(XO("Hz"));

         S.EndThreeColumn();
      }
      S.EndStatic();
   }

   S.EndScroller();

}

void DevicePrefs::OnHost(wxCommandEvent & e)
{
   // Bail if we have no hosts
   if (mHostNames.size() < 1)
      return;

   // Find the index for the host API selected
   int index = -1;
   auto apiName = mHostLabels[mHost->GetCurrentSelection()];
   int nHosts = Pa_GetHostApiCount();
   for (int i = 0; i < nHosts; ++i) {
      wxString name = wxSafeConvertMB2WX(Pa_GetHostApiInfo(i)->name);
      if (name == apiName) {
         index = i;
         break;
      }
   }
   // We should always find the host!
   if (index < 0) {
      wxLogDebug(wxT("DevicePrefs::OnHost(): API index not found"));
      return;
   }

   int nDevices = Pa_GetDeviceCount();

   // FIXME: TRAP_ERR PaErrorCode not handled.  nDevices can be negative number.
   if (nDevices == 0) {
      mHost->Clear();
      mHost->Append(_("No audio interfaces"), (void *) NULL);
      mHost->SetSelection(0);
   }

   const std::vector<DeviceSourceMap> &inMaps  = DeviceManager::Instance()->GetInputDeviceMaps();
   const std::vector<DeviceSourceMap> &outMaps = DeviceManager::Instance()->GetOutputDeviceMaps();

   wxArrayString playnames;
   wxArrayString recordnames;
   size_t i;
   int devindex;  /* temp variable to hold the numeric ID of each device in turn */
   wxString device;
   wxString recDevice;

   recDevice = mRecordDevice;
   if (!this->mRecordSource.empty())
      recDevice += wxT(": ") + mRecordSource;

   mRecord->Clear();
   for (i = 0; i < inMaps.size(); i++) {
      if (index == inMaps[i].hostIndex) {
         device   = MakeDeviceSourceString(&inMaps[i]);
         devindex = mRecord->Append(device);
         // We need to const cast here because SetClientData is a wx function
         // It is okay because the original variable is non-const.
         mRecord->SetClientData(devindex, const_cast<DeviceSourceMap *>(&inMaps[i]));
         if (device == recDevice) {  /* if this is the default device, select it */
            mRecord->SetSelection(devindex);
         }
      }
   }

   mPlay->Clear();
   for (i = 0; i < outMaps.size(); i++) {
      if (index == outMaps[i].hostIndex) {
         device   = MakeDeviceSourceString(&outMaps[i]);
         devindex = mPlay->Append(device);
         mPlay->SetClientData(devindex, const_cast<DeviceSourceMap *>(&outMaps[i]));
         if (device == mPlayDevice) {  /* if this is the default device, select it */
            mPlay->SetSelection(devindex);
         }
      }
   }

   /* deal with not having any devices at all */
   if (mPlay->GetCount() == 0) {
      playnames.push_back(_("No devices found"));
      mPlay->Append(playnames[0], (void *) NULL);
      mPlay->SetSelection(0);
   }
   if (mRecord->GetCount() == 0) {
      recordnames.push_back(_("No devices found"));
      mRecord->Append(recordnames[0], (void *) NULL);
      mRecord->SetSelection(0);
   }

   /* what if we have no device selected? we should choose the default on
    * this API, as defined by PortAudio. We then fall back to using 0 only if
    * that fails */
   if (mPlay->GetCount() && mPlay->GetSelection() == wxNOT_FOUND) {
      DeviceSourceMap *defaultMap = DeviceManager::Instance()->GetDefaultOutputDevice(index);
      if (defaultMap)
         mPlay->SetStringSelection(MakeDeviceSourceString(defaultMap));

      if (mPlay->GetSelection() == wxNOT_FOUND) {
         mPlay->SetSelection(0);
      }
   }

   if (mRecord->GetCount() && mRecord->GetSelection() == wxNOT_FOUND) {
      DeviceSourceMap *defaultMap = DeviceManager::Instance()->GetDefaultInputDevice(index);
      if (defaultMap)
         mRecord->SetStringSelection(MakeDeviceSourceString(defaultMap));

      if (mPlay->GetSelection() == wxNOT_FOUND) {
         mPlay->SetSelection(0);
      }
   }

   ShuttleGui::SetMinSize(mPlay, mPlay->GetStrings());
   ShuttleGui::SetMinSize(mRecord, mRecord->GetStrings());
   OnDevice(e);
}

void DevicePrefs::OnDevice(wxCommandEvent & WXUNUSED(event))
{
   int ndx = mRecord->GetCurrentSelection();
   if (ndx == wxNOT_FOUND) {
      ndx = 0;
   }

   int sel = mChannels->GetSelection();
   int cnt = 0;

   DeviceSourceMap *inMap = (DeviceSourceMap *) mRecord->GetClientData(ndx);
   if (inMap != NULL) {
      cnt = inMap->numChannels;
   }

   if (sel != wxNOT_FOUND) {
      mRecordChannels = sel + 1;
   }

   mChannels->Clear();

   // Mimic old behavior
   if (cnt <= 0) {
      cnt = 16;
   }

   // Place an artificial limit on the number of channels to prevent an
   // outrageous number.  I don't know if this is really necessary, but
   // it doesn't hurt.
   if (cnt > 256) {
      cnt = 256;
   }

   wxArrayStringEx channelnames;

   // Channel counts, mono, stereo etc...
   for (int i = 0; i < cnt; i++) {
      wxString name;

      if (i == 0) {
         name = _("1 (Mono)");
      }
      else if (i == 1) {
         name = _("2 (Stereo)");
      }
      else {
         name = wxString::Format(wxT("%d"), i + 1);
      }

      channelnames.push_back(name);
      int index = mChannels->Append(name);
      if (i == mRecordChannels - 1) {
         mChannels->SetSelection(index);
      }
   }

   if (mChannels->GetCount() && mChannels->GetCurrentSelection() == wxNOT_FOUND) {
      mChannels->SetSelection(0);
   }

   ShuttleGui::SetMinSize(mChannels, channelnames);
   Layout();
}

bool DevicePrefs::Commit()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);
   DeviceSourceMap *map = NULL;

   if (mPlay->GetCount() > 0) {
      map = (DeviceSourceMap *) mPlay->GetClientData(
            mPlay->GetSelection());
   }
   if (map)
      AudioIOPlaybackDevice.Write(map->deviceString);

   map = NULL;
   if (mRecord->GetCount() > 0) {
      map = (DeviceSourceMap *) mRecord->GetClientData(mRecord->GetSelection());
   }
   if (map) {
      AudioIORecordingDevice.Write(map->deviceString);
      AudioIORecordingSourceIndex.Write(map->sourceIndex);
      if (map->totalSources >= 1)
         AudioIORecordingSource.Write(map->sourceString);
      else
         AudioIORecordingSource.Reset();
      AudioIORecordChannels.Write(mChannels->GetSelection() + 1);
   }

   AudioIOLatencyDuration.Invalidate();
   AudioIOLatencyCorrection.Invalidate();
   return true;
}

PrefsPanel *DevicePrefsFactory(wxWindow *parent, wxWindowID winid, AudacityProject *project)
{
   wxASSERT(parent); // to justify safenew
   return safenew DevicePrefs(parent, winid, project);
}

namespace{
   PrefsPanel::Registration sAttachment{ "Device",
      DevicePrefsFactory
   };
}
