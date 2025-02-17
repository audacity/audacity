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

#include "RecordingPrefs.h"
#include "AudioIO.h"

#include <wx/defs.h>
#include <wx/textctrl.h>
#include <algorithm>

#include "Decibels.h"
#include "Prefs.h"
#include "ShuttleGui.h"

using std::min;

enum {
    UseCustomTrackNameID = 1000,
};

BEGIN_EVENT_TABLE(RecordingPrefs, PrefsPanel)
EVT_CHECKBOX(UseCustomTrackNameID, RecordingPrefs::OnToggleCustomName)
END_EVENT_TABLE()

RecordingPrefs::RecordingPrefs(wxWindow* parent, wxWindowID winid)
// i18n-hint: modifier as in "Recording preferences", not progressive verb
    :  PrefsPanel(parent, winid, XC("Recording", "preference"))
{
    gPrefs->Read(wxT("/GUI/TrackNames/RecordingNameCustom"), &mUseCustomTrackName, false);
    mOldNameChoice = mUseCustomTrackName;
    Populate();
}

RecordingPrefs::~RecordingPrefs()
{
}

ComponentInterfaceSymbol RecordingPrefs::GetSymbol() const
{
    return RECORDING_PREFS_PLUGIN_SYMBOL;
}

TranslatableString RecordingPrefs::GetDescription() const
{
    return XO("Preferences for Recording");
}

ManualPageID RecordingPrefs::HelpPageName()
{
    return "Recording_Preferences";
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

void RecordingPrefs::PopulateOrExchange(ShuttleGui& S)
{
    S.SetBorder(2);
    S.StartScroller();

    S.StartStatic(XO("Options"));
    {
        // Start wording of options with a verb, if possible.
        S.TieCheckBox(XXO("Hear &other tracks while recording (overdub)"),
                      { wxT("/AudioIO/Duplex"),
                        true });

//#if defined(__WXMAC__)
// Bug 388.  Feature not supported on any Mac Hardware.
#if 0
        S.TieCheckBox(XO("Use &hardware to play other tracks"),
                      { wxT("/AudioIO/Playthrough"),
                        false });
#endif
        S.TieCheckBox(XXO("Audible input &monitoring"),
                      { wxT("/AudioIO/SWPlaythrough"),
                        false });
#if !defined(__WXMAC__)
        //S.AddUnits(XO("     (uncheck when recording computer playback)"));
#endif

        S.TieCheckBox(XXO("Record on a new track"),
                      { wxT("/GUI/PreferNewTrackRecord"),
                        false });

/* i18n-hint: Dropout is a loss of a short sequence audio sample data from the recording */
        S.TieCheckBox(XXO("Detect dropouts"),
                      { WarningDialogKey(wxT("DropoutDetected")),
                        true });
    }
    S.EndStatic();

    S.StartStatic(XO("Sound Activated Recording"));
    {
        S.TieCheckBox(XXO("&Enable"), SoundActivatedRecord);

        S.StartMultiColumn(2, wxEXPAND);
        {
            S.SetStretchyCol(1);

            S.TieSlider(XXO("Le&vel (dB):"),
                        { wxT("/AudioIO/SilenceLevel"),
                          -50 },
                        0,
                        -DecibelScaleCutoff.Read());
        }
        S.EndMultiColumn();
    }
    S.EndStatic();

    /* i18n-hint: start of two-part phrase, "Name newly recorded tracks with:" */
    S.StartStatic(XO("Name newly recorded tracks"));
    {
        // Nested multicolumns to indent by 'With:' width, in a way that works if
        // translated.
        // This extra step is worth doing to get the check boxes lined up nicely.
        S.StartMultiColumn(2);
        {
            /* i18n-hint: end of two-part phrase, "Name newly recorded tracks with:" */
            S.AddFixedText(XO("With:"));
            S.StartMultiColumn(3);
            {
                S.Id(UseCustomTrackNameID).TieCheckBox(XXO("Custom Track &Name"),
                                                       { wxT("/GUI/TrackNames/RecordingNameCustom"),
                                                         mUseCustomTrackName });

                mToggleCustomName = S
                                    .Name(XO("Custom name text"))
                                    .Disable(!mUseCustomTrackName)
                                    .TieTextBox({},
                                                { wxT("/GUI/TrackNames/RecodingTrackName"),
                                                  _("Recorded_Audio") },
                                                30);
            }

            S.EndMultiColumn();

            S.AddFixedText({});
            S.StartMultiColumn(3);
            {
                S.TieCheckBox(XXO("&Track Number"),
                              { wxT("/GUI/TrackNames/TrackNumber"),
                                false });

                S.TieCheckBox(XXO("System &Date"),
                              { wxT("/GUI/TrackNames/DateStamp"),
                                false });

                S.TieCheckBox(XXO("System T&ime"),
                              { wxT("/GUI/TrackNames/TimeStamp"),
                                false });
            }
            S.EndMultiColumn();
        }
        S.EndMultiColumn();
    }
    S.EndStatic();

    S.StartStatic(XO("Punch and Roll Recording"));
    {
        S.StartThreeColumn();
        {
            auto w = S
                     .NameSuffix(XO("seconds"))
                     .TieNumericTextBox(XXO("Pre-ro&ll:"),
                                        { AUDIO_PRE_ROLL_KEY,
                                          DEFAULT_PRE_ROLL_SECONDS },
                                        9);
            S.AddUnits(XO("seconds"));
        }
        {
            auto w = S
                     .NameSuffix(XO("milliseconds"))
                     .TieNumericTextBox(XXO("Cross&fade:"),
                                        { AUDIO_ROLL_CROSSFADE_KEY,
                                          DEFAULT_ROLL_CROSSFADE_MS },
                                        9);
            S.AddUnits(XO("milliseconds"));
        }
        S.EndThreeColumn();
    }
    S.EndStatic();

    S.EndScroller();
}

bool RecordingPrefs::Commit()
{
    ShuttleGui S(this, eIsSavingToPrefs);
    PopulateOrExchange(S);

    SoundActivatedRecord.Invalidate();

    if (AudioIOLatencyDuration.Read() < 0) {
        AudioIOLatencyDuration.Reset();
    }

    return true;
}

void RecordingPrefs::OnToggleCustomName(wxCommandEvent& /* Evt */)
{
    mUseCustomTrackName = !mUseCustomTrackName;
    mToggleCustomName->Enable(mUseCustomTrackName);
}

namespace {
PrefsPanel::Registration sAttachment{ "Recording",
                                      [](wxWindow* parent, wxWindowID winid, AudacityProject*)
    {
        wxASSERT(parent); // to justify safenew
        return safenew RecordingPrefs(parent, winid);
    }
};
}
