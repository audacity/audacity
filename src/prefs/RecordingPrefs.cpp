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
#include "RecordingPrefs.h"

#include "../Experimental.h"

#include <wx/defs.h>
#include <wx/textctrl.h>
#include <algorithm>

#include "../AudioIO.h"
#include "../prefs/GUISettings.h"
#include "../Prefs.h"
#include "../ShuttleGui.h"

#include "../Internat.h"

#include "../widgets/Warning.h"

using std::min;

enum {
   UseCustomTrackNameID = 1000,
};

BEGIN_EVENT_TABLE(RecordingPrefs, PrefsPanel)
   EVT_CHECKBOX(UseCustomTrackNameID, RecordingPrefs::OnToggleCustomName)
END_EVENT_TABLE()

RecordingPrefs::RecordingPrefs(wxWindow * parent, wxWindowID winid)
:  PrefsPanel(parent, winid, _("Recording"))
{
   gPrefs->Read(wxT("/GUI/TrackNames/RecordingNameCustom"), &mUseCustomTrackName, false);
   mOldNameChoice = mUseCustomTrackName;
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
   S.SetBorder(2);
   S.StartScroller();

   S.StartStatic(_("Options"));
   {
      // Start wording of options with a verb, if possible.
      S.TieCheckBox(_("Play &other tracks while recording (overdub)"),
                    wxT("/AudioIO/Duplex"),
#ifdef EXPERIMENTAL_DA
                    false);
#else
                    true);
#endif

//#if defined(__WXMAC__)
// Bug 388.  Feature not supported on any Mac Hardware.
#if 0
      S.TieCheckBox(_("Use &hardware to play other tracks"),
                    wxT("/AudioIO/Playthrough"),
                    false);
#endif
      S.TieCheckBox(_("&Software playthrough of input"),
                    wxT("/AudioIO/SWPlaythrough"),
                    false);
#if !defined(__WXMAC__)
      //S.AddUnits(wxString(wxT("     ")) + _("(uncheck when recording computer playback)"));
#endif

       S.TieCheckBox(_("Record on a new track"),
                    wxT("/GUI/PreferNewTrackRecord"),
                    false);

/* i18n-hint: Dropout is a loss of a short sequence audio sample data from the recording */
       S.TieCheckBox(_("Detect dropouts"),
                     WarningDialogKey(wxT("DropoutDetected")),
                     true);


   }
   S.EndStatic();

   S.StartStatic(_("Sound Activated Recording"));
   {
      S.TieCheckBox(_("&Enable"),
                    wxT("/AudioIO/SoundActivatedRecord"),
                    false);

      S.StartMultiColumn(2, wxEXPAND);
      {
         S.SetStretchyCol(1);

         int dBRange = gPrefs->Read(ENV_DB_KEY, ENV_DB_RANGE);
         S.TieSlider(_("Le&vel (dB):"),
                     wxT("/AudioIO/SilenceLevel"),
                     -50,
                     0,
                     -dBRange);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   S.StartStatic(_("Name newly recorded tracks"));
   {
      // Nested multicolumns to indent by 'With:' width, in a way that works if 
      // translated.
      // This extra step is worth doing to get the check boxes lined up nicely.
      S.StartMultiColumn( 2 );
      {
         S.AddFixedText(_("With:")) ;
         S.StartMultiColumn(3);
         {
            S.Id(UseCustomTrackNameID).TieCheckBox(_("Custom Track &Name"),
                                            wxT("/GUI/TrackNames/RecordingNameCustom"),
                                            mUseCustomTrackName ? true : false);

            mToggleCustomName = S.TieTextBox( {},
                                              wxT("/GUI/TrackNames/RecodingTrackName"),
                                             _("Recorded_Audio"),
                                             30);
            if( mToggleCustomName ) {
               mToggleCustomName->SetName(_("Custom name text"));
               mToggleCustomName->Enable(mUseCustomTrackName);
            }
         }
         S.EndMultiColumn();

         S.AddFixedText(  {} );
         S.StartMultiColumn(3);
         {
            S.TieCheckBox(_("&Track Number"),
                          wxT("/GUI/TrackNames/TrackNumber"),
                          false);

            S.TieCheckBox(_("System &Date"),
                          wxT("/GUI/TrackNames/DateStamp"),
                          false);

            S.TieCheckBox(_("System T&ime"),
                          wxT("/GUI/TrackNames/TimeStamp"),
                          false);
         }
         S.EndMultiColumn();
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   #ifdef EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT
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

#ifdef EXPERIMENTAL_PUNCH_AND_ROLL
      S.StartStatic(_("Punch and Roll Recording"));
      {
         S.StartThreeColumn();
         {
            auto w = S.TieNumericTextBox(_("Pre-ro&ll:"),
               AUDIO_PRE_ROLL_KEY,
               DEFAULT_PRE_ROLL_SECONDS,
               9);
            S.AddUnits(_("seconds"));
            w->SetName(w->GetName() + wxT(" ") + _("seconds"));
         }
         {
            auto w = S.TieNumericTextBox(_("Cross&fade:"),
               AUDIO_ROLL_CROSSFADE_KEY,
               DEFAULT_ROLL_CROSSFADE_MS,
               9);
            S.AddUnits(_("milliseconds"));
            w->SetName(w->GetName() + wxT(" ") + _("milliseconds"));
         }
         S.EndThreeColumn();
      }
      S.EndStatic();
#endif

      S.EndScroller();
}

bool RecordingPrefs::Commit()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   double latencyDuration = DEFAULT_LATENCY_DURATION;
   gPrefs->Read(wxT("/AudioIO/LatencyDuration"), &latencyDuration);
   if (latencyDuration < 0) {
      gPrefs->Write(wxT("/AudioIO/LatencyDuration"), DEFAULT_LATENCY_DURATION);
   }

   #ifdef EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT
      double targetpeak, deltapeak;
      gPrefs->Read(wxT("/AudioIO/TargetPeak"),  &targetpeak);
      gPrefs->Read(wxT("/AudioIO/DeltaPeakVolume"), &deltapeak);
      if (targetpeak + deltapeak > 100.0 || targetpeak - deltapeak < 0.0)
      {
         gPrefs->Write(wxT("/AudioIO/DeltaPeakVolume"), min(100.0 - targetpeak, targetpeak));
      }

      int value;
      gPrefs->Read(wxT("/AudioIO/AnalysisTime"), &value);
      if (value <= 0)
         gPrefs->Write(wxT("/AudioIO/AnalysisTime"), AILA_DEF_ANALYSIS_TIME);

      gPrefs->Read(wxT("/AudioIO/NumberAnalysis"), &value);
      if (value < 0)
         gPrefs->Write(wxT("/AudioIO/NumberAnalysis"), AILA_DEF_NUMBER_ANALYSIS);
   #endif
   return true;
}

void RecordingPrefs::OnToggleCustomName(wxCommandEvent & /* Evt */)
{
   mUseCustomTrackName = !mUseCustomTrackName;
   mToggleCustomName->Enable(mUseCustomTrackName);
}

wxString RecordingPrefs::HelpPageName()
{
   return "Recording_Preferences";
}

PrefsPanel *RecordingPrefsFactory::operator () (wxWindow *parent, wxWindowID winid)
{
   wxASSERT(parent); // to justify safenew
   return safenew RecordingPrefs(parent, winid);
}
