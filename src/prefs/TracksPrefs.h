/**********************************************************************

  Audacity: A Digital Audio Editor

  TracksPrefs.h

  Brian Gunlogson
  Joshua Haberman
  James Crook

**********************************************************************/

#ifndef __AUDACITY_TRACKS_PREFS__
#define __AUDACITY_TRACKS_PREFS__

//#include <wx/defs.h>

#include <vector>
#include "PrefsPanel.h"
#include "../WaveTrack.h"

class ShuttleGui;

class TracksPrefs final : public PrefsPanel
{
 public:
   TracksPrefs(wxWindow * parent, wxWindowID winid);
   ~TracksPrefs();
   bool Commit() override;
   wxString HelpPageName() override;

   static bool GetPinnedHeadPreference();
   static void SetPinnedHeadPreference(bool value, bool flush = false);
   
   static double GetPinnedHeadPositionPreference();
   static void SetPinnedHeadPositionPreference(double value, bool flush = false);
   
   static wxString GetDefaultAudioTrackNamePreference();

   static WaveTrack::WaveTrackDisplay ViewModeChoice();
   static WaveTrack::SampleDisplay SampleViewChoice();
   static WaveTrack::ZoomPresets Zoom1Choice();
   static WaveTrack::ZoomPresets Zoom2Choice();

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S) override;

   static int iPreferencePinned;
};

/// A PrefsPanelFactory that creates one TracksPrefs panel.
class TracksPrefsFactory final : public PrefsPanelFactory
{
public:
   PrefsPanel *operator () (wxWindow *parent, wxWindowID winid) override;
};
#endif
