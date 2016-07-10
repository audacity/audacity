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

#include "../Audacity.h"

#include <wx/defs.h>

#include <wx/choice.h>
#include <wx/intl.h>
#include <wx/log.h>

#include "portaudio.h"

#include "../AudioIO.h"
#include "../Internat.h"
#include "../Prefs.h"
#include "../ShuttleGui.h"
#include "../DeviceManager.h"

#include "DevicePrefs.h"

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

DevicePrefs::DevicePrefs(wxWindow * parent)
:  PrefsPanel(parent, _("Devices"))
{
   Populate();
}

DevicePrefs::~DevicePrefs()
{
}

void DevicePrefs::Populate()
{
   // First any pre-processing for constructing the GUI.
   GetNamesAndLabels();

   // Get current setting for devices
   mPlayDevice = gPrefs->Read(wxT("/AudioIO/PlaybackDevice"), wxT(""));
   mRecordDevice = gPrefs->Read(wxT("/AudioIO/RecordingDevice"), wxT(""));
   mRecordSource = gPrefs->Read(wxT("/AudioIO/RecordingSource"), wxT(""));
   mRecordChannels = gPrefs->Read(wxT("/AudioIO/RecordChannels"), 2L);

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
         if (mHostNames.Index(name) == wxNOT_FOUND) {
            mHostNames.Add(name);
            mHostLabels.Add(name);
         }
      }
   }
}

void DevicePrefs::PopulateOrExchange(ShuttleGui & S)
{
   wxArrayString empty;

   S.SetBorder(2);

   S.StartStatic(_("Interface"));
   {
      S.StartMultiColumn(2);
      {
         S.Id(HostID);
         mHost = S.TieChoice(_("&Host:"),
                             wxT("/AudioIO/Host"),
                             wxT(""),
                             mHostNames,
                             mHostLabels);
         S.SetSizeHints(mHostNames);

         S.AddPrompt(_("Using:"));
         S.AddFixedText(wxString(wxSafeConvertMB2WX(Pa_GetVersionText())));
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   S.StartStatic(_("Playback"));
   {
      S.StartMultiColumn(2);
      {
         S.Id(PlayID);
         mPlay = S.AddChoice(_("&Device:"),
                             wxEmptyString,
                             &empty);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   S.StartStatic(_("Recording"));
   {
      S.StartMultiColumn(2);
      {
         S.Id(RecordID);
         mRecord = S.AddChoice(_("De&vice:"),
                               wxEmptyString,
                               &empty);

         S.Id(ChannelsID);
         mChannels = S.AddChoice(_("Cha&nnels:"),
                                 wxEmptyString,
                                 &empty);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();
}

void DevicePrefs::OnHost(wxCommandEvent & e)
{
   // Bail if we have no hosts
   if (mHostNames.size() < 1)
      return;

   // Find the index for the host API selected
   int index = -1;
   wxString apiName = mHostNames[mHost->GetCurrentSelection()];
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
   if (this->mRecordSource != wxT(""))
      recDevice += wxT(": ") + mRecordSource;

   mRecord->Clear();
   for (i = 0; i < inMaps.size(); i++) {
      if (index == inMaps[i].hostIndex) {
         device   = MakeDeviceSourceString(&inMaps[i]);
         devindex = mRecord->Append(device);
         // We need to const cast here because SetClientData is a wx function
         // It is okay beause the original variable is non-const.
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
      playnames.Add(_("No devices found"));
      mPlay->Append(playnames[0], (void *) NULL);
      mPlay->SetSelection(0);
   }
   if (mRecord->GetCount() == 0) {
      recordnames.Add(_("No devices found"));
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

   ShuttleGui S(this, eIsCreating);
   S.SetSizeHints(mPlay, mPlay->GetStrings());
   S.SetSizeHints(mRecord, mRecord->GetStrings());
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

   // Place an artifical limit on the number of channels to prevent an
   // outrageous number.  I don't know if this is really necessary, but
   // it doesn't hurt.
   if (cnt > 256) {
      cnt = 256;
   }

   wxArrayString channelnames;

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

      channelnames.Add(name);
      int index = mChannels->Append(name);
      if (i == mRecordChannels - 1) {
         mChannels->SetSelection(index);
      }
   }

   if (mChannels->GetCount() && mChannels->GetCurrentSelection() == wxNOT_FOUND) {
      mChannels->SetSelection(0);
   }

   ShuttleGui S(this, eIsCreating);
   S.SetSizeHints(mChannels, channelnames);
   Layout();
}

bool DevicePrefs::Apply()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);
   DeviceSourceMap *map = NULL;

   if (mPlay->GetCount() > 0) {
      map = (DeviceSourceMap *) mPlay->GetClientData(
            mPlay->GetSelection());
   }
   if (map) {
      gPrefs->Write(wxT("/AudioIO/PlaybackDevice"), map->deviceString);
   }

   map = NULL;
   if (mRecord->GetCount() > 0) {
      map = (DeviceSourceMap *) mRecord->GetClientData(mRecord->GetSelection());
   }
   if (map) {
      gPrefs->Write(wxT("/AudioIO/RecordingDevice"),
                    map->deviceString);
      gPrefs->Write(wxT("/AudioIO/RecordingSourceIndex"),
                    map->sourceIndex);
      if (map->totalSources >= 1) {
         gPrefs->Write(wxT("/AudioIO/RecordingSource"),
                       map->sourceString);
      } else {
         gPrefs->Write(wxT("/AudioIO/RecordingSource"),
                       wxT(""));
      }
      gPrefs->Write(wxT("/AudioIO/RecordChannels"),
                    mChannels->GetSelection() + 1);
   }

   return true;
}

PrefsPanel *DevicePrefsFactory::Create(wxWindow *parent)
{
   wxASSERT(parent); // to justify safenew
   return safenew DevicePrefs(parent);
}
