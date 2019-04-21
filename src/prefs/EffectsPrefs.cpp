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

ComponentInterfaceSymbol EffectsPrefs::GetSymbol()
{
   return EFFECTS_PREFS_PLUGIN_SYMBOL;
}

wxString EffectsPrefs::GetDescription()
{
   return _("Preferences for Effects");
}

wxString EffectsPrefs::HelpPageName()
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

void EffectsPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);
   S.StartScroller();

   S.StartStatic(_("Enable Effects"));
   {

#if USE_AUDIO_UNITS
/* i18n-hint: Audio Unit is the name of an Apple audio software protocol */
      S.TieCheckBox(_("Audio Unit"),
                    wxT("/AudioUnit/Enable"),
                    true);
#endif

      // JKC: LADSPA, LV2, Nyquist, VST, VAMP should not be translated.
#if USE_LADSPA
/* i18n-hint: abbreviates "Linux Audio Developer's Simple Plugin API"
   (Application programming interface)
 */
      S.TieCheckBox(_("&LADSPA"),
                    wxT("/LADSPA/Enable"),
                    true);
#endif

#if USE_LV2
/* i18n-hint: abbreviates
   "Linux Audio Developer's Simple Plugin API (LADSPA) version 2" */
      S.TieCheckBox(_("LV&2"),
                    wxT("/LV2/Enable"),
                    true);
#endif
#if USE_NYQUIST
/* i18n-hint: "Nyquist" is an embedded interpreted programming language in
 Audacity, named in honor of the Swedish-American Harry Nyquist (or Nyqvist).
 In the translations of this and other strings, you may transliterate the
 name into another alphabet.  */
      S.TieCheckBox(_("N&yquist"),
                    wxT("/Nyquist/Enable"),
                    true);
#endif

#if USE_VAMP
/* i18n-hint: Vamp is the proper name of a software protocol for sound analysis.
   It is not an abbreviation for anything.  See http://vamp-plugins.org */
      S.TieCheckBox(_("&Vamp"),
                    wxT("/Vamp/Enable"),
                    true);
#endif

#if USE_VST
/* i18n-hint: Abbreviates Virtual Studio Technology, an audio software protocol
   developed by Steinberg GmbH */
      S.TieCheckBox(_("V&ST"),
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
                                   wxT("sortby:name"),
                                   visualgroups,
                                   prefsgroups);
         if( c ) c->SetMinSize(c->GetBestSize());

         S.TieNumericTextBox(_("&Group size (0 to disable):"),
                             wxT("/Effects/MaxPerGroup"),
#if defined(__WXGTK__)
                             15,
#else
#ifdef EXPERIMENTAL_DA
                             15,
#else
                             0,
#endif
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

PrefsPanel *EffectsPrefsFactory::operator () (wxWindow *parent, wxWindowID winid)
{
   wxASSERT(parent); // to justify safenew
   return safenew EffectsPrefs(parent, winid);
}
