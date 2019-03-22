/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectsPrefs.cpp

  Brian Gunlogson
  Joshua Haberman
  Dominic Mazzoni
  James Crook


*******************************************************************//**

\class EffectsPrefs
\brief A PrefsPanel for general GUI prefernces.

*//*******************************************************************/

#include "../Audacity.h" // for USE_* macros
#include "EffectsPrefs.h"

#include "../Experimental.h"

#include <wx/choice.h>
#include <wx/defs.h>

#include "../Languages.h"
#include "../PluginManager.h"
#include "../Prefs.h"
#include "../ShuttleGui.h"

#include "../Internat.h"

EffectsPrefs::EffectsPrefs(wxWindow * parent, wxWindowID winid)
:  PrefsPanel(parent, winid, _("Effects"))
{
   Populate();
}

EffectsPrefs::~EffectsPrefs()
{
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

void EffectsPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);
   S.StartScroller();

   S.StartStatic(_("Enable Effects"));
   {

#if USE_AUDIO_UNITS
      S.TieCheckBox(_("Audio Unit"),
                    wxT("/AudioUnit/Enable"),
                    true);
#endif

      // JKC: LADSPA, LV2, Nyquist, VST, VAMP should not be translated.
#if USE_LADSPA
      S.TieCheckBox(wxT("&LADSPA"),
                    wxT("/Ladspa/Enable"),
                    true);
#endif

#if USE_LV2
      S.TieCheckBox(wxT("LV&2"),
                    wxT("/LV2/Enable"),
                    true);
#endif
#if USE_NYQUIST
      S.TieCheckBox(wxT("N&yquist"),
                    wxT("/Nyquist/Enable"),
                    true);
#endif

#if USE_VAMP
      S.TieCheckBox(wxT("&Vamp"),
                    wxT("/VAMP/Enable"),
                    true);
#endif

#if USE_VST
      S.TieCheckBox(wxT("V&ST"),
                    wxT("/VST/Enable"),
                    true);
#endif
   }
   S.EndStatic();

   S.StartStatic(_("Effect Options"));
   {
      S.StartMultiColumn(2);
      {
         wxArrayStringEx visualgroups{
            _("Sorted by Effect Name") ,
            _("Sorted by Publisher and Effect Name") ,
            _("Sorted by Type and Effect Name") ,
            _("Grouped by Publisher") ,
            _("Grouped by Type") ,
         };

         wxArrayStringEx prefsgroups{
            wxT("sortby:name") ,
            wxT("sortby:publisher:name") ,
            wxT("sortby:type:name") ,
            wxT("groupby:publisher") ,
            wxT("groupby:type") ,
         };

         wxChoice *c = S.TieChoice(_("S&ort or Group:"),
                                   wxT("/Effects/GroupBy"),
                                   wxT("name"),
                                   visualgroups,
                                   prefsgroups);
         if( c ) c->SetMinSize(c->GetBestSize());

         S.TieNumericTextBox(_("&Maximum effects per group (0 to disable):"),
                             wxT("/Effects/MaxPerGroup"),
#if defined(__WXGTK__)
                             15,
#else
                             0,
#endif
                             5);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

#ifndef EXPERIMENTAL_EFFECT_MANAGEMENT
   S.StartStatic(_("Plugin Options"));
   {
      S.TieCheckBox(_("Check for updated plugins when Audacity starts"),
                     wxT("/Plugins/CheckForUpdates"),
                     true);
      S.TieCheckBox(_("Rescan plugins next time Audacity is started"),
                     wxT("/Plugins/Rescan"),
                     false);
   }
   S.EndStatic();
#endif

#ifdef EXPERIMENTAL_EQ_SSE_THREADED
   S.StartStatic(_("Instruction Set"));
   {
      S.TieCheckBox(_("&Use SSE/SSE2/.../AVX"),
                    wxT("/SSE/GUI"),
                    true);
   }
   S.EndStatic();
#endif
   S.EndScroller();
}

bool EffectsPrefs::Commit()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   return true;
}

wxString EffectsPrefs::HelpPageName()
{
   return "Effects_Preferences";
}

PrefsPanel *EffectsPrefsFactory::operator () (wxWindow *parent, wxWindowID winid)
{
   wxASSERT(parent); // to justify safenew
   return safenew EffectsPrefs(parent, winid);
}
