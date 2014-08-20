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

#if USE_AUDIO_UNITS
   S.StartStatic(_("Audio Unit Effects"));
   {
      S.TieCheckBox(_("Display Audio Unit effects in Graphical Mode"),
                    wxT("/AudioUnits/GUI"),
                    true);
#if 0
      S.TieCheckBox(_("Rescan VST effects next time Audacity is started"),
                    wxT("/VST/Rescan"),
                    false);
#endif
   }
   S.EndStatic();
#endif

#if USE_VST
   S.StartStatic(_("VST Effects"));
   {
      S.TieCheckBox(_("&Display VST effects in Graphical Mode"),
                    wxT("/VST/GUI"),
                    true);
      S.TieCheckBox(_("&Rescan VST effects next time Audacity is started"),
                    wxT("/VST/Rescan"),
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
