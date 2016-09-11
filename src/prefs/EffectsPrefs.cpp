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

#include "../Audacity.h"

#include <wx/defs.h>

#include "../AudacityApp.h"
#include "../Languages.h"
#include "../PluginManager.h"
#include "../Prefs.h"
#include "../ShuttleGui.h"

#include "EffectsPrefs.h"

#include "../Experimental.h"

EffectsPrefs::EffectsPrefs(wxWindow * parent)
:  PrefsPanel(parent, _("Effects"))
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
      S.TieCheckBox(wxT("&VAMP"),
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
         wxArrayString visualgroups;
         wxArrayString prefsgroups;
      
         visualgroups.Add(_("Sorted by Effect Name"));
         visualgroups.Add(_("Sorted by Publisher and Effect Name"));
         visualgroups.Add(_("Sorted by Type and Effect Name"));
         visualgroups.Add(_("Grouped by Publisher"));
         visualgroups.Add(_("Grouped by Type"));
      
         prefsgroups.Add(wxT("sortby:name"));
         prefsgroups.Add(wxT("sortby:publisher:name"));
         prefsgroups.Add(wxT("sortby:type:name"));
         prefsgroups.Add(wxT("groupby:publisher"));
         prefsgroups.Add(wxT("groupby:type"));

         wxChoice *c = S.TieChoice(_("Effects in menus are:"),
                                   wxT("/Effects/GroupBy"),
                                   wxT("name"),
                                   visualgroups,
                                   prefsgroups);
         c->SetMinSize(c->GetBestSize());
                     
         S.TieNumericTextBox(_("Maximum effects per group (0 to disable):"),
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
}

bool EffectsPrefs::Apply()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   return true;
}

PrefsPanel *EffectsPrefsFactory::Create(wxWindow *parent)
{
   wxASSERT(parent); // to justify safenew
   return safenew EffectsPrefs(parent);
}
