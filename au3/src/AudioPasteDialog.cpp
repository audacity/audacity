/**********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioPasteDialog.cpp

  @author Vitaly Sverchinsky

**********************************************************************/

#include "AudioPasteDialog.h"

#include <wx/sizer.h>
#include <wx/checkbox.h>
#include <wx/stattext.h>
#include <wx/radiobut.h>

#include "ShuttleGui.h"
#include "prefs/TracksBehaviorsPrefs.h"
#include "WindowAccessible.h"

BEGIN_EVENT_TABLE(AudioPasteDialog, wxDialogWrapper)
EVT_BUTTON(wxID_OK, AudioPasteDialog::OnContinue)
EVT_BUTTON(wxID_CANCEL, AudioPasteDialog::OnCancel)
END_EVENT_TABLE()

namespace {
const TranslatableString audioPasteModeText[] = {
    XO("Smart clip.\n"
       "The entire source clip will be pasted into your project, allowing you to access\ntrimmed audio data anytime."),
    XO("Selected audio only.\n"
       "Only the selected portion of the source clip will be pasted.")
};

//Setting is different from TracksBehaviorsAudioTrackPastePolicy
//This is to remember the last mode used in the dialog
ChoiceSetting AudioPasteDialogDefaultModeSetting = {
    wxT("/GUI/AudioPasteDialogDefaultMode"),
    {
        ByColumns,
        { {}, {} },
        { wxT("Keep"), wxT("Discard") }
    },
    0, // "Keep"
};
}

AudioPasteDialog::AudioPasteDialog(wxWindow* parent, wxULongLong nCopiedBytes, int winid)
    : wxDialogWrapper(parent, winid, XO("Paste audio")), mCopiedBytesNum(nCopiedBytes)
{
    ShuttleGui S(this, eIsCreatingFromPrefs);
    PopulateOrExchange(S);

    SetInitialSize({ 520, -1 });

    Layout();
    Fit();
    Center();
}

void AudioPasteDialog::PopulateOrExchange(ShuttleGui& S)
{
    S.SetBorder(20);
    S.AddFixedText(XO("How would you like to paste your audio?"));
    S.AddSpace(10);
    S.StartRadioButtonGroup(AudioPasteDialogDefaultModeSetting);
    {
        for (int i = 0, count =AudioPasteDialogDefaultModeSetting.GetSymbols().size();
             i < count;
             ++i) {
            S.StartHorizontalLay(wxALIGN_LEFT);
            {
                S.SetBorder(0);
                S.AddSpace(20);
                wxRadioButton* radioButton{};
                S.StartVerticalLay(0);
                {
                    radioButton
                        =S.Name(audioPasteModeText[i]).TieRadioButton();
                    S.AddSpace(0, 0, 1);
                }
                S.EndVerticalLay();

                S.AddVariableText(audioPasteModeText[i])->Bind(
                    wxEVT_LEFT_UP, [=](auto)
                {
                    radioButton->SetValue(true);
                });
   #if wxUSE_ACCESSIBILITY
                safenew WindowAccessible(radioButton);
   #endif
                S.AddSpace(20, 0);
            }
            S.EndHorizontalLay();
            if (i != count - 1) {
                S.AddSpace(0, 10);
            }
        }
    }
    S.EndRadioButtonGroup();

    S.SetBorder(20);
    S.AddFixedText(
        /* i18n-hint: %s substitutes for a file size, e.g. "345 MB". A "smart clip" is an audio clip containing hidden trimmed data. */
        XO("The full smart clip is %s. Larger sizes will take longer to paste.")
        .Format(wxFileName::GetHumanReadableSize(mCopiedBytesNum))
        );

    S.SetBorder(0);
    S.StartHorizontalLay(wxEXPAND);
    {
        S.AddSpace(20, 0);
        mDontShowAgain = S
                         .AddCheckBox(XO("Remember my choice and don't ask again"), false);
        if (mDontShowAgain != nullptr) {
            mDontShowAgain->SetFont(wxFont(wxFontInfo().Bold()));
        }
        S.AddSpace(0, 0, 1);
        S.Id(wxID_CANCEL).AddButton(XXO("Cancel"), wxALIGN_CENTER);
        S.AddSpace(6, 0);
        S.Id(wxID_OK).AddButton(XXO("Continue"), wxALIGN_CENTER, true);
        S.AddSpace(20, 0);
    }
    S.EndHorizontalLay();

    S.AddSpace(0, 10);
}

void AudioPasteDialog::OnContinue(wxCommandEvent&)
{
    ShuttleGui S(this, eIsSavingToPrefs);
    PopulateOrExchange(S);

    auto pasteMode = AudioPasteDialogDefaultModeSetting.Read();
    if (mDontShowAgain->IsChecked()) {
        TracksBehaviorsAudioTrackPastePolicy.Write(pasteMode);
    }

    gPrefs->Flush();

    EndModal(pasteMode == wxT("Discard") ? DISCARD : KEEP);
}

void AudioPasteDialog::OnCancel(wxCommandEvent&)
{
    EndModal(wxID_CANCEL);
}
