/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIOPrefs.h

  Joshua Haberman
  James Crook

**********************************************************************/

#include "../Experimental.h"

class wxChoice;
class wxTextCtrl;
class ShuttleGui;

#ifdef EXPERIMENTAL_MIDI_OUT

#ifndef __AUDACITY_MIDI_IO_PREFS__
#define __AUDACITY_MIDI_IO_PREFS__

#include <wx/defs.h>

#include "PrefsPanel.h"

#define MIDI_IO_PREFS_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Midi IO") }

class MidiIOPrefs final : public PrefsPanel
{
 public:
   MidiIOPrefs(wxWindow * parent, wxWindowID winid);
   virtual ~MidiIOPrefs();
   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;

   bool Commit() override;
   bool Validate() override;
   wxString HelpPageName() override;
   void PopulateOrExchange(ShuttleGui & S) override;

 private:
   void Populate();
   void GetNamesAndLabels();

   void OnHost(wxCommandEvent & e);
//   void OnDevice(wxCommandEvent & e);

   TranslatableStrings mHostNames;
   wxArrayStringEx mHostLabels;

   wxString mPlayDevice;
#ifdef EXPERIMENTAL_MIDI_IN
   wxString mRecordDevice;
#endif
//   long mRecordChannels;

   wxChoice *mHost;
   wxChoice *mPlay;
   wxTextCtrl *mLatency;
#ifdef EXPERIMENTAL_MIDI_IN
   wxChoice *mRecord;
#endif
//   wxChoice *mChannels;

   DECLARE_EVENT_TABLE()
};

#endif

#endif
