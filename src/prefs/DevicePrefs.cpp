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

void DevicePrefs::GetNamesAndLabels()
{
   // Gather list of hosts.  Only added hosts that have devices attached.
   int nDevices = Pa_GetDeviceCount();
   for (int i = 0; i < nDevices; i++) {
      const PaDeviceInfo *info = Pa_GetDeviceInfo(i);
      if (info->maxOutputChannels > 0 || info->maxInputChannels > 0) {
         wxString name(Pa_GetHostApiInfo(info->hostApi)->name, wxConvLocal);
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
         mHost = S.TieChoice(_("&Host") + wxString(wxT(":")),
                             wxT("/AudioIO/Host"), 
                             wxT(""),
                             mHostNames,
                             mHostLabels);
         S.SetSizeHints(mHostNames);

         S.AddPrompt(_("Using:"));
         S.AddFixedText(wxString(Pa_GetVersionText(), wxConvLocal));
      }
      S.EndMultiColumn();
   }                              
   S.EndStatic();

   S.StartStatic(_("Playback"));
   {
      S.StartMultiColumn(2);
      {
         S.Id(PlayID);
         mPlay = S.AddChoice(_("&Device") + wxString(wxT(":")),
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
         mRecord = S.AddChoice(_("De&vice") + wxString(wxT(":")),
                               wxEmptyString,
                               &empty);

         S.Id(ChannelsID);
         mChannels = S.AddChoice(_("Cha&nnels") + wxString(wxT(":")),
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
      wxString name(Pa_GetHostApiInfo(i)->name, wxConvLocal);
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

   if (nDevices == 0) {
      mHost->Clear();
      mHost->Append(_("No audio interfaces"), (void *) NULL);
      mHost->SetSelection(0);
   }

   mPlay->Clear();
   mRecord->Clear();

   wxArrayString playnames;
   wxArrayString recordnames;

   int devindex;  /* temp variable to hold the numeric ID of each device in turn */

   for (int i = 0; i < nDevices; i++) {
      const PaDeviceInfo *info = Pa_GetDeviceInfo(i);
      if (info->hostApi == index) { /* if the device is for the current HostAPI */
         wxString name(info->name, wxConvLocal);
         wxString device = DeviceName(info);


         if (info->maxOutputChannels > 0) {
            playnames.Add(name);
            devindex = mPlay->Append(name, (void *) info);
            if (device == mPlayDevice) {  /* if this is the default device, select it */
               mPlay->SetSelection(devindex);
            }
         }

         if (info->maxInputChannels > 0) {
            recordnames.Add(name);
            devindex = mRecord->Append(name, (void *) info);
            if (device == mRecordDevice) {
               mRecord->SetSelection(devindex);
            }
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
      wxLogDebug(wxT("DevicePrefs::OnHost(): no play device selected"));
      mPlay->SetStringSelection(GetDefaultPlayDevice(index));

      if (mPlay->GetSelection() == wxNOT_FOUND) {
         mPlay->SetSelection(0);
      }
   }

   if (mRecord->GetCount() && mRecord->GetSelection() == wxNOT_FOUND) {
      wxLogDebug(wxT("DevicePrefs::OnHost(): no record device selected"));
      mRecord->SetStringSelection(GetDefaultRecordDevice(index));

      if (mPlay->GetSelection() == wxNOT_FOUND) {
         mPlay->SetSelection(0);
      }
   }

   ShuttleGui S(this, eIsCreating);
   S.SetSizeHints(mPlay, playnames);
   S.SetSizeHints(mRecord, recordnames);
   OnDevice(e);
}

void DevicePrefs::OnDevice(wxCommandEvent & e)
{
   int ndx = mRecord->GetCurrentSelection();
   if (ndx == wxNOT_FOUND) {
      ndx = 0;
   }

   int sel = mChannels->GetSelection();
   int cnt = 0;

   const PaDeviceInfo *info = (const PaDeviceInfo *) mRecord->GetClientData(ndx);
   if (info != NULL) {
      cnt = info->maxInputChannels;
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

wxString DevicePrefs::GetDefaultPlayDevice(int index)
{
   if (index < 0 || index >= Pa_GetHostApiCount()) {
      return wxEmptyString;
   }

   const struct PaHostApiInfo *apiinfo = Pa_GetHostApiInfo(index);   // get info on API
   wxLogDebug(wxT("GetDefaultPlayDevice(): HostAPI index %d, name %s"), index, wxString(apiinfo->name, wxConvLocal).c_str());
   wxLogDebug(wxT("GetDefaultPlayDevice() default output %d"), apiinfo->defaultOutputDevice);
   const PaDeviceInfo* devinfo = Pa_GetDeviceInfo(apiinfo->defaultOutputDevice);
   if (devinfo == NULL) {
     wxLogDebug(wxT("GetDefaultPlayDevice() no default output device"));
     return wxString("", wxConvLocal);
   }
   wxString name(devinfo->name, wxConvLocal);
   wxLogDebug(wxT("GetDefaultPlayDevice() default output device name %s"), name.c_str());
   return name;
}

wxString DevicePrefs::GetDefaultRecordDevice(int index)
{
   if (index < 0 || index >= Pa_GetHostApiCount()) {
      return wxEmptyString;
   }

   const struct PaHostApiInfo *apiinfo = Pa_GetHostApiInfo(index);   // get info on API
   wxLogDebug(wxT("GetDefaultRecordDevice(): HostAPI index %d, name %s"), index, wxString(apiinfo->name, wxConvLocal).c_str());
   wxLogDebug(wxT("GetDefaultRecordDevice() default input %d"), apiinfo->defaultInputDevice);
   const PaDeviceInfo* devinfo = Pa_GetDeviceInfo(apiinfo->defaultInputDevice);
   if (devinfo == NULL) {
     wxLogDebug(wxT("GetDefaultRecordDevice() no default input device"));
     return wxString("", wxConvLocal);
   }
   wxString name(devinfo->name, wxConvLocal);
   wxLogDebug(wxT("GetDefaultRecordDevice() default input device name %s"), name.c_str());
   return name;
}

bool DevicePrefs::Apply()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   const PaDeviceInfo *info = NULL;

   if (mPlay->GetCount() > 0) {
      info = (const PaDeviceInfo *) mPlay->GetClientData(
            mPlay->GetSelection());
   }
   if (info) {
      gPrefs->Write(wxT("/AudioIO/PlaybackDevice"),
                    DeviceName(info));
   }

   info = NULL;
   if (mRecord->GetCount() > 0) {
      info = (const PaDeviceInfo *) mRecord->GetClientData(mRecord->GetSelection());
   }
   if (info) {
      gPrefs->Write(wxT("/AudioIO/RecordingDevice"),
                    DeviceName(info));

      gPrefs->Write(wxT("/AudioIO/RecordChannels"),
                    mChannels->GetSelection() + 1);
   }

   return true;
}


// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: d6904b91-a320-4194-8d60-caa9175b6bb4
