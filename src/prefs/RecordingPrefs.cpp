/**********************************************************************

  Audacity: A Digital Audio Editor

  RecordingPrefs.cpp

  Joshua Haberman
  Dominic Mazzoni
  James Crook

*******************************************************************//**

\class RecordingPrefs
\brief A PrefsPanel used to select recording options.

  Presents interface for user to update the various recording options
  like playthrough, latency correction, and others.

*//********************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/textctrl.h>
#include <algorithm>

#include "../AudioIO.h"
#include "../Envelope.h"
#include "../Prefs.h"
#include "../ShuttleGui.h"

#include "RecordingPrefs.h"

using std::min;

RecordingPrefs::RecordingPrefs(wxWindow * parent)
:  PrefsPanel(parent, _("Recording"))
{
   Populate();
}

RecordingPrefs::~RecordingPrefs()
{
}

void RecordingPrefs::Populate()
{
   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

void RecordingPrefs::PopulateOrExchange(ShuttleGui & S)
{
   wxTextCtrl *w;

   S.SetBorder(2);

   S.StartStatic(_("Playthrough"));
   {
      S.TieCheckBox(_("Overdub: &Play other tracks while recording new one"),
                    wxT("/AudioIO/Duplex"),
                    true);
#if defined(__WXMAC__)
      S.TieCheckBox(_("&Hardware Playthrough: Listen while recording or monitoring new track"),
                    wxT("/AudioIO/Playthrough"),
                    false);
#endif
      S.TieCheckBox(_("&Software Playthrough: Listen while recording or monitoring new track"),
                    wxT("/AudioIO/SWPlaythrough"),
                    false);
#if !defined(__WXMAC__)
      S.AddUnits(wxString(wxT("     ")) + _("(uncheck when recording \"stereo mix\")"));
#endif
   }
   S.EndStatic();

   S.StartStatic( _("Latency"));
   {
      S.StartThreeColumn();
      {
         // only show the following controls if we use Portaudio v19, because
         // for Portaudio v18 we always use default buffer sizes
         w = S.TieNumericTextBox(_("Audio to &buffer:"),
                                 wxT("/AudioIO/LatencyDuration"),
                                 DEFAULT_LATENCY_DURATION,
                                 9);
         S.AddUnits(_("milliseconds (higher = more latency)"));
         w->SetName(w->GetName() + wxT(" ") + _("milliseconds (higher = more latency)"));

         w = S.TieNumericTextBox(_("L&atency correction:"),
                                 wxT("/AudioIO/LatencyCorrection"),
                                 DEFAULT_LATENCY_CORRECTION,
                                 9);
         S.AddUnits(_("milliseconds (negative = backwards)"));
         w->SetName(w->GetName() + wxT(" ") + _("milliseconds (negative = backwards)"));
      }
      S.EndThreeColumn();
   }
   S.EndStatic();

   S.StartStatic(_("Sound Activated Recording"));
   {
      S.TieCheckBox(_("Sound Activated &Recording"),
                    wxT("/AudioIO/SoundActivatedRecord"),
                    false);

      S.StartMultiColumn(2, wxEXPAND);
      {
         S.SetStretchyCol(1);

         int dBRange = gPrefs->Read(wxT("/GUI/EnvdBRange"), ENV_DB_RANGE);
         S.TieSlider(_("Sound Activation Le&vel (dB):"),
                     wxT("/AudioIO/SilenceLevel"),
                     -50,
                     0,
                     -dBRange);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   #ifdef AUTOMATED_INPUT_LEVEL_ADJUSTMENT
      S.StartStatic(_("Automated Recording Level Adjustment"));
      {
         S.TieCheckBox(_("Enable Automated Recording Level Adjustment."),
                       wxT("/AudioIO/AutomatedInputLevelAdjustment"),
                       false);

         S.StartMultiColumn(2, wxEXPAND);
         {
            S.SetStretchyCol(1);

            /* i18n-hint: Desired maximum (peak) volume for sound */
            S.TieSlider(_("Target Peak:"),
                        wxT("/AudioIO/TargetPeak"),
                        AILA_DEF_TARGET_PEAK,
                        100,
                        0);

            S.TieSlider(_("Within:"),
                     wxT("/AudioIO/DeltaPeakVolume"),
                     AILA_DEF_DELTA_PEAK,
                     100,
                     0);
         }
         S.EndMultiColumn();

         S.StartThreeColumn();
         {
            S.TieNumericTextBox(_("Analysis Time:"),
                                wxT("/AudioIO/AnalysisTime"),
                                AILA_DEF_ANALYSIS_TIME,
                                9);
            S.AddUnits(_("milliseconds (time of one analysis)"));

            S.TieNumericTextBox(_("Number of consecutive analysis:"),
                                wxT("/AudioIO/NumberAnalysis"),
                                AILA_DEF_NUMBER_ANALYSIS,
                                2);
            S.AddUnits(_("0 means endless"));
          }
          S.EndThreeColumn();
      }
      S.EndStatic();
   #endif
}

bool RecordingPrefs::Apply()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   double latencyDuration = DEFAULT_LATENCY_DURATION;
   gPrefs->Read(wxT("/AudioIO/LatencyDuration"), &latencyDuration);
   if (latencyDuration < 0) {
      gPrefs->Write(wxT("/AudioIO/LatencyDuration"), DEFAULT_LATENCY_DURATION);
   }

   #ifdef AUTOMATED_INPUT_LEVEL_ADJUSTMENT
      double targetpeak, deltapeak;
      gPrefs->Read(wxT("/AudioIO/TargetPeak"),  &targetpeak);
      gPrefs->Read(wxT("/AudioIO/DeltaPeakVolume"), &deltapeak);
      if (targetpeak + deltapeak > 100.0 || targetpeak - deltapeak < 0.0)
      {
         gPrefs->Write(wxT("/AudioIO/DeltaPeakVolume"), min(100.0 - targetpeak, targetpeak));
         gPrefs->Flush();
      }

      int value;
      gPrefs->Read(wxT("/AudioIO/AnalysisTime"), &value);
      if (value <= 0)
         gPrefs->Write(wxT("/AudioIO/AnalysisTime"), AILA_DEF_ANALYSIS_TIME);

      gPrefs->Read(wxT("/AudioIO/NumberAnalysis"), &value);
      if (value < 0)
         gPrefs->Write(wxT("/AudioIO/NumberAnalysis"), AILA_DEF_NUMBER_ANALYSIS);
   #endif
   return gPrefs->Flush();
}
