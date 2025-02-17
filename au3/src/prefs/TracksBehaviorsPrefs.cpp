/**********************************************************************

  Audacity: A Digital Audio Editor

  TracksBehaviorsPrefs.cpp

  Steve Daulton


*******************************************************************//**

\class TracksBehaviorsPrefs
\brief A PrefsPanel for Tracks Behaviors settings.

*//*******************************************************************/

#include "TracksBehaviorsPrefs.h"
#include "ViewInfo.h"

#include <wx/stattext.h>
#include <wx/radiobut.h>
#include <wx/statbox.h>

#include "Prefs.h"
#include "ShuttleGui.h"
#include "WaveTrack.h"
#include "WindowAccessible.h"

TracksBehaviorsPrefs::TracksBehaviorsPrefs(wxWindow* parent, wxWindowID winid)
/* i18n-hint: i.e. the behaviors of tracks */
    :  PrefsPanel(parent, winid, XO("Tracks Behaviors"))
{
    Populate();
}

TracksBehaviorsPrefs::~TracksBehaviorsPrefs()
{
}

ComponentInterfaceSymbol TracksBehaviorsPrefs::GetSymbol() const
{
    return TRACKS_BEHAVIORS_PREFS_PLUGIN_SYMBOL;
}

TranslatableString TracksBehaviorsPrefs::GetDescription() const
{
    return XO("Preferences for TracksBehaviors");
}

ManualPageID TracksBehaviorsPrefs::HelpPageName()
{
    return "Tracks_Behaviors_Preferences";
}

void TracksBehaviorsPrefs::Populate()
{
    //------------------------- Main section --------------------
    // Now construct the GUI itself.
    ShuttleGui S(this, eIsCreatingFromPrefs);
    PopulateOrExchange(S);
    // ----------------------- End of main section --------------
}

namespace {
const TranslatableString audioPasteModeText[] = {
    XO("Smart clip.\n"
       "The entire source clip will be pasted into your project, allowing you to access\ntrimmed audio data anytime."),
    XO("Selected audio only.\n"
       "Only the selected portion of the source clip will be pasted."),
    XO("Ask me each time.\n"
       "Show dialog each time audio is pasted."),
};
}

ChoiceSetting TracksBehaviorsAudioTrackPastePolicy {
    wxT("/GUI/AudioTrackPastePolicy"),
    {
        ByColumns,
        { {}, {}, {} },
        { wxT("Keep"), wxT("Discard"),  wxT("Ask") }
    },
    2, // "Ask"
};

void TracksBehaviorsPrefs::PopulateOrExchange(ShuttleGui& S)
{
    S.SetBorder(2);
    S.StartScroller();

    S.StartStatic(XO("Behaviors"));
    {
        S.TieCheckBox(XXO("&Select all audio, if selection required"),
                      { wxT("/GUI/SelectAllOnNone"),
                        false });
        /* i18n-hint: Cut-lines are lines that can expand to show the cut audio.*/
        S.TieCheckBox(XXO("Enable cut &lines"),
                      { wxT("/GUI/EnableCutLines"),
                        false });
        S.TieCheckBox(XXO("Editing a clip can &move other clips"),
                      EditClipsCanMove);
        S.TieCheckBox(
            XXO("&Always paste audio as new clips"),
            { wxT("/GUI/PasteAsNewClips"), false });
        S.TieCheckBox(XXO("\"Move track focus\" c&ycles repeatedly through tracks"),
                      { wxT("/GUI/CircularTrackNavigation"),
                        false });
        S.TieCheckBox(XXO("&Type to create a label"),
                      { wxT("/GUI/TypeToCreateLabel"),
                        false });
        S.TieCheckBox(XXO("Use dialog for the &name of a new label"),
                      { wxT("/GUI/DialogForNameNewLabel"),
                        false });

        S.AddSpace(10);

        S.StartMultiColumn(2);
        {
            S.TieChoice(XXO("Solo &Button:"), TracksBehaviorsSolo);
        }
        S.EndMultiColumn();
    }
    S.EndStatic();
    auto pastedAudioBox = S.StartStatic(XO("Pasted audio"));
    {
        const auto header = S.AddVariableText(XO("Paste audio from other Audacity project as"));
#if wxUSE_ACCESSIBILITY
        if (pastedAudioBox != nullptr) {
            pastedAudioBox->SetName(header->GetLabel());
            safenew WindowAccessible(pastedAudioBox);
        }
#endif
        S.StartRadioButtonGroup(TracksBehaviorsAudioTrackPastePolicy);
        for (int i = 0;
             i < TracksBehaviorsAudioTrackPastePolicy.GetSymbols().size();
             ++i) {
            S.StartHorizontalLay(wxALIGN_LEFT);
            {
                wxRadioButton* radioButton{};
                S.StartVerticalLay(0);
                {
                    radioButton
                        =S.Name(audioPasteModeText[i]).TieRadioButton();
                    S.AddSpace(0, 0, 1);
                }
                S.EndVerticalLay();

                if (auto pText = S.AddVariableText(audioPasteModeText[i])) {
                    pText->Bind(
                        wxEVT_LEFT_UP, [=](auto)
                    {
                        radioButton->SetValue(true);
                    });
                }
   #if wxUSE_ACCESSIBILITY
                safenew WindowAccessible(radioButton);
   #endif
            }
            S.EndHorizontalLay();
        }
        S.EndRadioButtonGroup();
    }
    S.EndStatic();

    S.EndScroller();
}

bool TracksBehaviorsPrefs::Commit()
{
    ShuttleGui S(this, eIsSavingToPrefs);
    PopulateOrExchange(S);
    EditClipsCanMove.Invalidate();

    return true;
}

namespace {
PrefsPanel::Registration sAttachment{ "TracksBehaviors",
                                      [](wxWindow* parent, wxWindowID winid, AudacityProject*)
    {
        wxASSERT(parent); // to justify safenew
        return safenew TracksBehaviorsPrefs(parent, winid);
    },
                                      false,
                                      // Place it at a lower tree level
                                      { "Tracks" }
};
}
