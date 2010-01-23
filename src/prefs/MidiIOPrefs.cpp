/**********************************************************************

  Audacity: A Digital Audio Editor

  MidiIOPrefs.cpp

  Joshua Haberman
  Dominic Mazzoni
  James Crook

*******************************************************************//**

\class MidiIOPrefs
\brief A PrefsPanel used to select recording and playback devices and
other settings.

  Presents interface for user to select the recording device and
  playback device, from the list of choices that PortMidi
  makes available.

  Also lets user decide whether or not to record in stereo, and
  whether or not to play other tracks while recording one (duplex).

*//********************************************************************/

#include "../Audacity.h"
#include "../Experimental.h"
#ifdef EXPERIMENTAL_MIDI_OUT

#include <wx/defs.h>

#include <wx/choice.h>
#include <wx/intl.h>

#include "portmidi.h"

#include "../AudioIO.h"
#include "../Internat.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../ShuttleGui.h"

#include "MidiIOPrefs.h"

enum {
   HostID = 10000,
   PlayID,
   RecordID,
   ChannelsID
};

BEGIN_EVENT_TABLE(MidiIOPrefs, PrefsPanel)
   EVT_CHOICE(HostID, MidiIOPrefs::OnHost)
//   EVT_CHOICE(RecordID, MidiIOPrefs::OnDevice)
END_EVENT_TABLE()

MidiIOPrefs::MidiIOPrefs(wxWindow * parent)
:  PrefsPanel(parent, _("MIDI Devices"))
{
   Populate();
}

MidiIOPrefs::~MidiIOPrefs()
{
}

void MidiIOPrefs::Populate()
{
   // First any pre-processing for constructing the GUI.
   GetNamesAndLabels();

   // Get current setting for devices
   mPlayDevice = gPrefs->Read(wxT("/MidiIO/PlaybackDevice"), wxT(""));
   mRecordDevice = gPrefs->Read(wxT("/MidiIO/RecordingDevice"), wxT(""));
//   mRecordChannels = gPrefs->Read(wxT("/MidiIO/RecordChannels"), 2L);

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

/// Gets the lists of names and lists of labels which are
/// used in the choice controls.
/// The names are what the user sees in the wxChoice.
/// The corresponding labels are what gets stored.
void MidiIOPrefs::GetNamesAndLabels() {
   // Gather list of hosts.  Only added hosts that have devices attached.
   int nDevices = Pm_CountDevices();
   for (int i = 0; i < nDevices; i++) {
      const PmDeviceInfo *info = Pm_GetDeviceInfo(i);
      if (info->output || info->input) { //should always happen
         wxString name(info->interf, wxConvLocal);
         if (mHostNames.Index(name) == wxNOT_FOUND) {
            mHostNames.Add(name);
            mHostLabels.Add(name);
         }
      }
   }
}

void MidiIOPrefs::PopulateOrExchange( ShuttleGui & S ) {
   wxArrayString empty;

   S.SetBorder(2);

   S.StartStatic(_("Interface"));
   {
      S.StartMultiColumn(2);
      {
         S.Id(HostID);
         mHost = S.TieChoice(_("Host") + wxString(wxT(":")),
                             wxT("/MidiIO/Host"), 
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
         mPlay = S.AddChoice(_("Device") + wxString(wxT(":")),
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
         mRecord = S.AddChoice(_("Device") + wxString(wxT(":")),
                               wxEmptyString,
                               &empty);

         S.Id(ChannelsID);
         /*
         mChannels = S.AddChoice(_("Channels") + wxString(wxT(":")),
                                 wxEmptyString,
                                 &empty);
         */
      }
      S.EndMultiColumn();
   }
   S.EndStatic();
}

// Not sure that these settings are needed right now.
#if 0
   S.StartStatic( _("Playthrough") );
   {
      S.TieCheckBox( _("&Play other tracks while recording new one"),
         wxT("Duplex"),true);
#ifdef __MACOSX__
      S.TieCheckBox( _("&Hardware Playthrough (Play new track while recording it)"),
         wxT("Playthrough"),false);
#endif
      S.TieCheckBox( _("&Software Playthrough (Play new track while recording it)"),
         wxT("SWPlaythrough"),false);
   }
   S.EndStatic();
   S.StartHorizontalLay( wxEXPAND, 0 );
   S.StartStatic( _("Cut Preview"),1 );
   {
      S.StartThreeColumn();
      S.TieTextBox( _("Play before cut region:"), wxT("CutPreviewBeforeLen"),1.0,9);
      S.AddUnits(  _("seconds") );
      S.TieTextBox( _("Play after cut region:"),wxT("CutPreviewAfterLen"), 1.0,9);
      S.AddUnits(  _("seconds") );
      S.EndThreeColumn();
   }
   S.EndStatic();
   S.StartStatic( _("Latency"),1 );
   {
      S.StartThreeColumn();
      // only show the following controls if we use Portaudio v19, because
      // for Portaudio v19 we always use default buffer sizes
      S.TieTextBox( _("Audio to buffer:"),wxT("LatencyDuration"),100.0,9);
      S.AddUnits(  _("milliseconds") );
      S.TieTextBox( _("Latency correction:"),wxT("LatencyCorrection"),0.0,9);
      S.AddUnits(  _("milliseconds") );
      S.EndThreeColumn();
   }
   S.EndStatic();
   S.EndHorizontalLay();
   S.StartHorizontalLay( wxEXPAND, 0 );
   S.StartStatic( _("Seek Time"),1 );
   {
      S.StartThreeColumn();
      S.TieTextBox( _("Short period:"), wxT("SeekShortPeriod"),1.0,9);
      S.AddUnits(  _("seconds") );
      S.TieTextBox( _("Long period:"),wxT("SeekLongPeriod"), 15.0,9);
      S.AddUnits(  _("seconds") );
      S.EndThreeColumn();
   }
   S.EndStatic();
   S.StartStatic( _("Effects Preview"),1 );
   {
      S.StartThreeColumn();
      S.TieTextBox( _("Play when previewing:"), wxT("EffectsPreviewLen"), 3.0,9);
      S.AddUnits( _("seconds") );
      S.EndThreeColumn();
   }

   gPrefs->SetPath(wxT("/"));
#endif

// JKC: This is some old code that was sizing control labels all the same, 
// even if in different static controls.  It made for a nicer layout.
// We might want to do something like that again in future.
#if 0

   // find out the biggest minimum size of labels
   int maxIndex = 0,r;
   wxSize maxMinSize = textSizer[0]->GetMinSize();
   for (r = 1; r < 3; r++) {
      if (textSizer[r]->GetMinSize().GetWidth() > maxMinSize.GetWidth()) {
         maxMinSize = textSizer[r]->GetMinSize();
         maxIndex = r;
      }
   }

   // set small minimum sizes to max minumum size
   for (r = 0; r < 3; r++) {
      if (r != maxIndex) 
         textSizer[r]->SetMinSize( maxMinSize );
   }
#endif

void MidiIOPrefs::OnHost(wxCommandEvent & e)
{
   int index = mHost->GetCurrentSelection();
   wxString itemAtIndex = mHostNames.Item(index);
   int nDevices = Pm_CountDevices();

   if (nDevices == 0) {
      mHost->Clear();
      mHost->Append(_("No MIDI interfaces"), (void *) NULL);
      mHost->SetSelection(0);
   }

   mPlay->Clear();
   mRecord->Clear();

   wxArrayString playnames;
   wxArrayString recordnames;

   for (int i = 0; i < nDevices; i++) {
      const PmDeviceInfo *info = Pm_GetDeviceInfo(i);
      wxString interf(info->interf, wxConvLocal);
      if (itemAtIndex.IsSameAs(interf)) {
         wxString name(info->name, wxConvLocal);
         wxString device = wxString::Format(wxT("%s: %s"),
                                            interf.c_str(),
                                            name.c_str());
         int index;

         if (info->output) {
            playnames.Add(name);
            index = mPlay->Append(name, (void *) info);
            if (device == mPlayDevice) {
               mPlay->SetSelection(index);
            }
         }

         if (info->input) {
            recordnames.Add(name);
            index = mRecord->Append(name, (void *) info);
            if (device == mRecordDevice) {
               mRecord->SetSelection(index);
            }
         }
      }
   }

   if (mPlay->GetCount() == 0) {
      playnames.Add(_("No devices found"));
      mPlay->Append(playnames[0], (void *) NULL);
   }

   if (mRecord->GetCount() == 0) {
      recordnames.Add(_("No devices found"));
      mRecord->Append(recordnames[0], (void *) NULL);
   }

   if (mPlay->GetCount() && mPlay->GetSelection() == wxNOT_FOUND) {
      mPlay->SetSelection(0);
   }

   if (mRecord->GetCount() && mRecord->GetSelection() == wxNOT_FOUND) {
      mRecord->SetSelection(0);
   }

   ShuttleGui S(this, eIsCreating);
   S.SetSizeHints(mPlay, playnames);
   S.SetSizeHints(mRecord, recordnames);
//   OnDevice(e);
}

/*
void MidiIOPrefs::OnDevice(wxCommandEvent & e)
{
   int ndx = mRecord->GetCurrentSelection();
   if (ndx == wxNOT_FOUND) {
      ndx = 0;
   }

   int sel = mChannels->GetSelection();
   int cnt = 0;

   const PmDeviceInfo *info = (const PmDeviceInfo *) mRecord->GetClientData(ndx);
   if (info != NULL) {
      cnt = info->input;
   }

   if (sel != wxNOT_FOUND) {
      mRecordChannels = sel + 1;
   }

   mChannels->Clear();

   // Limit cnt
   cnt = cnt <= 0  ? 16  : cnt;
   cnt = cnt > 256 ? 256 : cnt;
      
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
*/

bool MidiIOPrefs::Apply()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   const PmDeviceInfo *info;

   info = (const PmDeviceInfo *) mPlay->GetClientData(mPlay->GetSelection());
   if (info) {
      gPrefs->Write(wxT("/MidiIO/PlaybackDevice"),
                    wxString::Format(wxT("%s: %s"),
                                     wxString(info->interf, wxConvLocal).c_str(),
                                     wxString(info->name, wxConvLocal).c_str()));
   }

   info = (const PmDeviceInfo *) mRecord->GetClientData(mRecord->GetSelection());
   if (info) {
      gPrefs->Write(wxT("/MidiIO/RecordingDevice"),
                    wxString::Format(wxT("%s: %s"),
                                     wxString(info->interf, wxConvLocal).c_str(),
                                     wxString(info->name, wxConvLocal).c_str()));
   }

/*
   gPrefs->Write(wxT("/MidiIO/RecordChannels"),
                 wxString::Format(wxT("%d"),
                                  mChannels->GetSelection() + 1));
*/

   #if USE_PORTMIXER
   if (gAudioIO)
      gAudioIO->HandleDeviceChange();
   #endif // USE_PORTMIXER

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

#endif
