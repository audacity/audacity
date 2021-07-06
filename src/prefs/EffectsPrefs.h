/**********************************************************************

  Sneedacity: A Digital Audio Editor

  EffectsPrefs.h

  Brian Gunlogson
  Joshua Haberman
  James Crook

**********************************************************************/

#ifndef __SNEEDACITY_EFFECTS_PREFS__
#define __SNEEDACITY_EFFECTS_PREFS__

#include <wx/defs.h>

#include "PrefsPanel.h"

class ChoiceSetting;
class ShuttleGui;

#define EFFECTS_PREFS_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Effects") }

class EffectsPrefs final : public PrefsPanel
{
 public:
   EffectsPrefs(wxWindow * parent, wxWindowID winid);
   ~EffectsPrefs();
   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;

   bool Commit() override;
   ManualPageID HelpPageName() override;
   void PopulateOrExchange(ShuttleGui & S) override;

 private:
   void Populate();
};

SNEEDACITY_DLL_API extern ChoiceSetting EffectsGroupBy;
#endif
