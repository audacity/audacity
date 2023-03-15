/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectsPrefs.cpp

  Brian Gunlogson
  Joshua Haberman
  Dominic Mazzoni
  James Crook


*******************************************************************//**

\class EffectsPrefs
\brief A PrefsPanel for general GUI preferences.

*//*******************************************************************/


#include "EffectsPrefs.h"

#include <wx/choice.h>
#include <wx/defs.h>
#include <wx/button.h>

#include "PluginManager.h"
#include "PluginRegistrationDialog.h"
#include "Menus.h"
#include "Prefs.h"
#include "ShuttleGui.h"

EffectsPrefs::EffectsPrefs(wxWindow * parent, wxWindowID winid)
:  PrefsPanel(parent, winid, XO("Effects"))
{
   Populate();
}

EffectsPrefs::~EffectsPrefs()
{
}

ComponentInterfaceSymbol EffectsPrefs::GetSymbol() const
{
   return EFFECTS_PREFS_PLUGIN_SYMBOL;
}

TranslatableString EffectsPrefs::GetDescription() const
{
   return XO("Preferences for Effects");
}

ManualPageID EffectsPrefs::HelpPageName()
{
   return "Effects_Preferences";
}

void EffectsPrefs::Populate()
{
   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

EnumValueSymbols EffectsGroupSymbols {
      ByColumns,
      {
         XO("Sort by effect name") ,
         XO("Sort by publisher and effect name") ,
         XO("Sort by type and effect name") ,
         XO("Group by publisher") ,
         XO("Group by type") ,
         XO("Group by category"),
         XO("Group by type and publisher")
      },
      {
         wxT("sortby:name") ,
         wxT("sortby:publisher:name") ,
         wxT("sortby:type:name") ,
         wxT("groupby:publisher") ,
         wxT("groupby:type") ,
         wxT("default"),
         wxT("groupby:type:publisher")
      }
};

ChoiceSetting EffectsGroupBy{
   wxT("/Effects/GroupBy"),
   EffectsGroupSymbols,
   5 // "default"
};

ChoiceSetting RealtimeEffectsGroupBy{
   wxT("/Effects/RealtimeGroupBy"),
   EffectsGroupSymbols,
   6 // "groupby:type:publisher"
};

void EffectsPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);
   S.StartScroller();

   S.StartStatic(XO("Effect Options"));
   {
      S.StartMultiColumn(2);
      {
         S.MinSize()
          .TieChoice( XXO("Effect menu &organization:"), EffectsGroupBy);
         S.MinSize()
          .TieChoice( XXO("Realtime effect o&rganization:"), RealtimeEffectsGroupBy);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

#ifdef EXPERIMENTAL_EQ_SSE_THREADED
   S.StartStatic(XO("Instruction Set"));
   {
      S.TieCheckBox(XXO("&Use SSE/SSE2/.../AVX"),
                    {wxT("/SSE/GUI"),
                    true});
   }
   S.EndStatic();
#endif

   S.AddButton(XO("Open Plugin Manager"), wxALIGN_LEFT)->Bind(wxEVT_BUTTON, [this](auto) {
      //Adding dependency on PluginRegistrationDialog, not good. Alternatively
      //that could be done with events, though event should be visible here too...
      PluginRegistrationDialog dialog(wxGetTopLevelParent(this));
      if(dialog.ShowModal() == wxID_OK)
         MenuCreator::RebuildAllMenuBars();
   });

   S.EndScroller();
}

bool EffectsPrefs::Commit()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   return true;
}

namespace{
PrefsPanel::Registration sAttachment{ "Effects",
   [](wxWindow *parent, wxWindowID winid, AudacityProject *)
   {
      wxASSERT(parent); // to justify safenew
      return safenew EffectsPrefs(parent, winid);
   }
};
}
