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
#ifdef EXPERIMENTAL_MIDI_IN
   mRecordDevice = gPrefs->Read(wxT("/MidiIO/RecordingDevice"), wxT(""));
#endif
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
   Pm_Terminate(); // close and open to refresh device lists
   Pm_Initialize();
   int nDevices = Pm_CountDevices();
   for (int i = 0; i < nDevices; i++) {
      const PmDeviceInfo *info = Pm_GetDeviceInfo(i);
      if (info->output || info->input) { //should always happen
         wxString name = wxSafeConvertMB2WX(info->interf);
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
         /* i18n-hint: (noun) */
         mHost = S.TieChoice(_("Host:"),
                             wxT("/MidiIO/Host"),
                             wxT(""),
                             mHostNames,
                             mHostLabels);
         S.SetSizeHints(mHostNames);

         S.AddPrompt(_("Using: PortMidi"));
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   S.StartStatic(_("Playback"));
   {
      S.StartMultiColumn(2);
      {
         S.Id(PlayID);
         mPlay = S.AddChoice(_("Device:"),
                             wxEmptyString,
                             &empty);
         int latency = gPrefs->Read(wxT("/MidiIO/OutputLatency"),
                                    DEFAULT_SYNTH_LATENCY);
         mLatency = S.TieNumericTextBox(_("MIDI Synthesizer Latency (ms):"),
                                        wxT("/MidiIO/SynthLatency"),
                                        latency, 3);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();
#ifdef EXPERIMENTAL_MIDI_IN
   S.StartStatic(_("Recording"));
   {
      S.StartMultiColumn(2);
      {
         S.Id(RecordID);
         mRecord = S.AddChoice(_("Device:"),
                               wxEmptyString,
                               &empty);

         S.Id(ChannelsID);
         /*
         mChannels = S.AddChoice(_("Channels:"),
                                 wxEmptyString,
                                 &empty);
         */
      }
      S.EndMultiColumn();
   }
   S.EndStatic();
#endif
}

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
#ifdef EXPERIMENTAL_MIDI_IN
   mRecord->Clear();
#endif

   wxArrayString playnames;
   wxArrayString recordnames;

   for (int i = 0; i < nDevices; i++) {
      const PmDeviceInfo *info = Pm_GetDeviceInfo(i);
      wxString interf = wxSafeConvertMB2WX(info->interf);
      if (itemAtIndex.IsSameAs(interf)) {
         wxString name = wxSafeConvertMB2WX(info->name);
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
#ifdef EXPERIMENTAL_MIDI_IN
         if (info->input) {
            recordnames.Add(name);
            index = mRecord->Append(name, (void *) info);
            if (device == mRecordDevice) {
               mRecord->SetSelection(index);
            }
         }
#endif
      }
   }

   if (mPlay->GetCount() == 0) {
      playnames.Add(_("No devices found"));
      mPlay->Append(playnames[0], (void *) NULL);
   }
#ifdef EXPERIMENTAL_MIDI_IN
   if (mRecord->GetCount() == 0) {
      recordnames.Add(_("No devices found"));
      mRecord->Append(recordnames[0], (void *) NULL);
   }
#endif
   if (mPlay->GetCount() && mPlay->GetSelection() == wxNOT_FOUND) {
      mPlay->SetSelection(0);
   }
#ifdef EXPERIMENTAL_MIDI_IN
   if (mRecord->GetCount() && mRecord->GetSelection() == wxNOT_FOUND) {
      mRecord->SetSelection(0);
   }
#endif
   ShuttleGui S(this, eIsCreating);
   S.SetSizeHints(mPlay, playnames);
#ifdef EXPERIMENTAL_MIDI_IN
   S.SetSizeHints(mRecord, recordnames);
#endif
//   OnDevice(e);
}

bool MidiIOPrefs::Apply()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   const PmDeviceInfo *info;

   info = (const PmDeviceInfo *) mPlay->GetClientData(mPlay->GetSelection());
   if (info) {
      gPrefs->Write(wxT("/MidiIO/PlaybackDevice"),
                    wxString::Format(wxT("%s: %s"),
                                     wxString(wxSafeConvertMB2WX(info->interf)).c_str(),
                                     wxString(wxSafeConvertMB2WX(info->name)).c_str()));
   }
#ifdef EXPERIMENTAL_MIDI_IN
   info = (const PmDeviceInfo *) mRecord->GetClientData(mRecord->GetSelection());
   if (info) {
      gPrefs->Write(wxT("/MidiIO/RecordingDevice"),
                    wxString::Format(wxT("%s: %s"),
                                     wxString(wxSafeConvertMB2WX(info->interf)).c_str(),
                                     wxString(wxSafeConvertMB2WX(info->name)).c_str()));
   }
#endif
   return gPrefs->Flush();
}

bool MidiIOPrefs::Validate()
{
   long latency;
   if (!mLatency->GetValue().ToLong(&latency)) {
      wxMessageBox(_("The MIDI Synthesizer Latency must be an integer"));
      return false;
   }
   return true;
}

PrefsPanel *MidiIOPrefsFactory::Create(wxWindow *parent)
{
   wxASSERT(parent); // to justify safenew
   return safenew MidiIOPrefs(parent);
}

#endif
