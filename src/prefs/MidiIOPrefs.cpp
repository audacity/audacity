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


#include "MidiIOPrefs.h"

#ifdef EXPERIMENTAL_MIDI_OUT

#include <wx/defs.h>

#include <wx/choice.h>
#include <wx/intl.h>
#include <wx/textctrl.h>

#include <portmidi.h>

#include "NoteTrack.h"
#include "Prefs.h"
#include "../ShuttleGui.h"
#include "../widgets/AudacityMessageBox.h"

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

MidiIOPrefs::MidiIOPrefs(wxWindow * parent, wxWindowID winid)
/* i18n-hint: untranslatable acronym for "Musical Instrument Device Interface" */
:  PrefsPanel(parent, winid, XO("MIDI Devices"))
{
   Populate();
}

MidiIOPrefs::~MidiIOPrefs()
{
}

ComponentInterfaceSymbol MidiIOPrefs::GetSymbol()
{
   return MIDI_IO_PREFS_PLUGIN_SYMBOL;
}

TranslatableString MidiIOPrefs::GetDescription()
{
   return XO("Preferences for MidiIO");
}

ManualPageID MidiIOPrefs::HelpPageName()
{
   return "MIDI_Devices_Preferences";
}

void MidiIOPrefs::Populate()
{
   // First any pre-processing for constructing the GUI.
   GetNamesAndLabels();

   // Get current setting for devices
   mPlayDevice = MIDIPlaybackDevice.Read();
#ifdef EXPERIMENTAL_MIDI_IN
   mRecordDevice = MIDIRecordingDevice.Read();
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
         if (!make_iterator_range(mHostNames)
            .contains( Verbatim( name ) )) {
            mHostNames.push_back( Verbatim( name ) );
            mHostLabels.push_back(name);
         }
      }
   }

   if (nDevices == 0) {
      mHostNames.push_back(XO("No MIDI interfaces"));
      mHostLabels.push_back(wxT("No MIDI interfaces"));
   }
}

void MidiIOPrefs::PopulateOrExchange( ShuttleGui & S ) {

   S.SetBorder(2);
   S.StartScroller();

   /* i18n-hint Software interface to MIDI */
   S.StartStatic(XC("Interface", "MIDI"));
   {
      S.StartMultiColumn(2);
      {
         S.Id(HostID);
         /* i18n-hint: (noun) */
         mHost = S.TieChoice( XXO("&Host:"),
            {
               wxT("/MidiIO/Host"),
               { ByColumns, mHostNames, mHostLabels }
            }
         );

         S.AddPrompt(XXO("Using: PortMidi"));
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
         mLatency = S.TieIntegerTextBox(XXO("MIDI Synth L&atency (ms):"),
                                        MIDISynthLatency_ms, 3);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();
#ifdef EXPERIMENTAL_MIDI_IN
   S.StartStatic(XO("Recording"));
   {
      S.StartMultiColumn(2);
      {
         S.Id(RecordID);
         mRecord = S.AddChoice(XO("De&vice:"),
                               {} );

         S.Id(ChannelsID);
         /*
         mChannels = S.AddChoice(XO("&Channels:"),
                                 wxEmptyString,
                                 {} );
         */
      }
      S.EndMultiColumn();
   }
   S.EndStatic();
#endif
   S.EndScroller();

}

void MidiIOPrefs::OnHost(wxCommandEvent & WXUNUSED(e))
{
   wxString itemAtIndex;
   int index = mHost->GetCurrentSelection();
   if (index >= 0 && index < (int)mHostNames.size())
      itemAtIndex = mHostLabels[index];
   int nDevices = Pm_CountDevices();

   mPlay->Clear();
#ifdef EXPERIMENTAL_MIDI_IN
   mRecord->Clear();
#endif

   wxArrayStringEx playnames;
   wxArrayStringEx recordnames;

   for (int i = 0; i < nDevices; i++) {
      const PmDeviceInfo *info = Pm_GetDeviceInfo(i);
      wxString interf = wxSafeConvertMB2WX(info->interf);
      if (itemAtIndex == interf) {
         wxString name = wxSafeConvertMB2WX(info->name);
         wxString device = wxString::Format(wxT("%s: %s"),
                                            interf,
                                            name);
         if (info->output) {
            playnames.push_back(name);
            index = mPlay->Append(name, (void *) info);
            if (device == mPlayDevice) {
               mPlay->SetSelection(index);
            }
         }
#ifdef EXPERIMENTAL_MIDI_IN
         if (info->input) {
            recordnames.push_back(name);
            index = mRecord->Append(name, (void *) info);
            if (device == mRecordDevice) {
               mRecord->SetSelection(index);
            }
         }
#endif
      }
   }

   if (mPlay->GetCount() == 0) {
      playnames.push_back(_("No devices found"));
      mPlay->Append(playnames[0], (void *) NULL);
   }
#ifdef EXPERIMENTAL_MIDI_IN
   if (mRecord->GetCount() == 0) {
      recordnames.push_back(_("No devices found"));
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
   ShuttleGui::SetMinSize(mPlay, playnames);
#ifdef EXPERIMENTAL_MIDI_IN
   ShuttleGui::SetMinSize(mRecord, recordnames);
#endif
//   OnDevice(e);
}

bool MidiIOPrefs::Commit()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   const PmDeviceInfo *info;

   info = (const PmDeviceInfo *) mPlay->GetClientData(mPlay->GetSelection());
   if (info) {
      MIDIPlaybackDevice.Write(
         wxString::Format(wxT("%s: %s"),
            wxString(wxSafeConvertMB2WX(info->interf)),
            wxString(wxSafeConvertMB2WX(info->name))));
   }
#ifdef EXPERIMENTAL_MIDI_IN
   info = (const PmDeviceInfo *) mRecord->GetClientData(mRecord->GetSelection());
   if (info) {
      MidiRecordingDevice.Write(
         wxString::Format(wxT("%s: %s"),
            wxString(wxSafeConvertMB2WX(info->interf)),
            wxString(wxSafeConvertMB2WX(info->name))));
   }
#endif
   return gPrefs->Flush();
}

bool MidiIOPrefs::Validate()
{
   long latency;
   if (!mLatency->GetValue().ToLong(&latency)) {
      AudacityMessageBox( XO(
"The MIDI Synthesizer Latency must be an integer") );
      return false;
   }
   return true;
}

#ifdef EXPERIMENTAL_MIDI_OUT
namespace{
PrefsPanel::Registration sAttachment{ "MidiIO",
   [](wxWindow *parent, wxWindowID winid, AudacityProject *)
   {
      wxASSERT(parent); // to justify safenew
      return safenew MidiIOPrefs(parent, winid);
   },
   false,
   // Register with an explicit ordering hint because this one is
   // only conditionally compiled
   { "", { Registry::OrderingHint::After, "Recording" } }
};
}
#endif

#endif
