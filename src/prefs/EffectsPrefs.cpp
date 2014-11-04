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
                    wxT("/AudioUnits/Enable"),
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

      S.AddFixedText(_("Restart Audacity to apply changes."));
   }
   S.EndStatic();

   S.StartStatic(_("Effect Options"));
   {
      S.StartMultiColumn(2);
      {
         wxArrayString visualgroups;
         wxArrayString prefsgroups;
      
         visualgroups.Add(_("Publisher: Effect Name"));
         visualgroups.Add(_("Name"));
         visualgroups.Add(_("Publisher"));
         visualgroups.Add(_("Type (Internal, Ladspa, VST, etc.)"));
      
         prefsgroups.Add(wxT("default"));
         prefsgroups.Add(wxT("name"));
         prefsgroups.Add(wxT("publisher"));
         prefsgroups.Add(wxT("family"));

         S.TieChoice(_("Group effects in menus by:"),
                     wxT("/Effects/GroupBy"),
                     wxT("default"),
                     visualgroups,
                     prefsgroups);
         S.TieNumericTextBox(_("Maximum effects per group (0 to disable):"),
                             wxT("/Effects/MaxPerGroup"),
                             0,
                             5);
      }
      S.EndMultiColumn();

      S.AddSpace(5);

      S.TieCheckBox(_("Display effects in graphical mode when supported"),
                     wxT("/Effects/GUI"),
                     true);
   }
   S.EndStatic();

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

void EffectsPrefs::SetState(const wxString & family, const wxString & key)
{
   PluginManager & pm = PluginManager::Get();
   bool state = gPrefs->Read(wxT("/Nyquist/Enable"), true);

   const PluginDescriptor *plug = pm.GetFirstPluginForEffectFamily(family);
   while (plug)
   {
      pm.EnablePlugin(plug->GetID(), state);
      plug = pm.GetNextPluginForEffectFamily(family);
   }
}

bool EffectsPrefs::Apply()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

#ifdef USE_NYQUIST
   SetState(wxT("Nyquist"), wxT("/Nyquist/Enable"));
#endif

#ifdef USE_LADSPA
   SetState(wxT("Ladspa"), wxT("/Ladspa/Enable"));
#endif

#ifdef USE_LV2
   SetState(wxT("LV2"), wxT("/LV2/Enable"));
#endif

#ifdef USE_AUDIO_UNITS
   SetState(wxT("AudioUnit"), wxT("/AudioUnits/Enable"));
#endif

#ifdef USE_VAMP
   SetState(wxT("VAMP"), wxT("/VAMP/Enable"));
#endif

   return true;
}
