/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectsPrefs.h

  Brian Gunlogson
  Joshua Haberman
  James Crook

**********************************************************************/

#ifndef __AUDACITY_EFFECTS_PREFS__
#define __AUDACITY_EFFECTS_PREFS__

#include <wx/defs.h>

#include "PrefsPanel.h"

class ChoiceSetting;
class PluginProvider;
class ShuttleGui;
class EffectsLocationPanel;

#define EFFECTS_PREFS_PLUGIN_SYMBOL ComponentInterfaceSymbol { XO("Effects") }

class EffectsPrefs final : public PrefsPanel
{
public:
    EffectsPrefs(wxWindow* parent, wxWindowID winid);
    ~EffectsPrefs();
    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;

    bool Commit() override;
    ManualPageID HelpPageName() override;
    void PopulateOrExchange(ShuttleGui& S) override;

private:
    void Populate();

    std::vector<std::pair<PluginProvider*, EffectsLocationPanel*> > mLocations;
};

AUDACITY_DLL_API extern BoolSetting SkipEffectsScanAtStartup;
AUDACITY_DLL_API extern ChoiceSetting EffectsGroupBy;
AUDACITY_DLL_API extern ChoiceSetting RealtimeEffectsGroupBy;
#endif
