/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIOPrefs.h

  Joshua Haberman
  James Crook

**********************************************************************/

class wxChoice;
class wxTextCtrl;
class ShuttleGui;

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
   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;

   bool Commit() override;
   bool Validate() override;
   ManualPageID HelpPageName() override;
   void PopulateOrExchange(ShuttleGui & S) override;

 private:
   void Populate();
   void GetNamesAndLabels();

   void OnHost(wxCommandEvent & e);
//   void OnDevice(wxCommandEvent & e);

   TranslatableStrings mHostNames;
   wxArrayStringEx mHostLabels;

   wxString mPlayDevice;


//   long mRecordChannels;

   wxChoice *mHost;
   wxChoice *mPlay;
   wxTextCtrl *mLatency;

   // These two are for Experimental::MidiIn
   wxString mRecordDevice;
   wxChoice *mRecord{};

//   wxChoice *mChannels;

   DECLARE_EVENT_TABLE()
};

#endif
