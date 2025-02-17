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
#include "WaveChannelViewConstants.h"
#include "WaveformSettings.h" // for ScaleTypeValues

class ShuttleGui;

#define TRACKS_PREFS_PLUGIN_SYMBOL ComponentInterfaceSymbol { XO("Tracks") }

class AUDACITY_DLL_API TracksPrefs final : public PrefsPanel
{
public:
    static BoolSetting TracksFitVerticallyZoomed;

    TracksPrefs(wxWindow* parent, wxWindowID winid);
    ~TracksPrefs();
    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;

    bool Commit() override;
    ManualPageID HelpPageName() override;

    static bool GetPinnedHeadPreference();
    static void SetPinnedHeadPreference(bool value, bool flush = false);

    static double GetPinnedHeadPositionPreference();
    static void SetPinnedHeadPositionPreference(double value, bool flush = false);

    static WaveChannelViewConstants::Display ViewModeChoice();
    static WaveChannelViewConstants::SampleDisplay SampleViewChoice();
    static WaveChannelViewConstants::ZoomPresets Zoom1Choice();
    static WaveChannelViewConstants::ZoomPresets Zoom2Choice();

private:
    void Populate();
    void PopulateOrExchange(ShuttleGui& S) override;

    static int iPreferencePinned;
};

#endif
