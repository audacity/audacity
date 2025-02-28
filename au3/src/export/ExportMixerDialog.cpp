/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportMixerDialog.cpp

  Dominic Mazzoni

**********************************************************************/

#include "ExportMixerDialog.h"
#include "ExportMixerPanel.h"
#include "ExportUtils.h"

#include <wx/sizer.h>
#include <wx/stattext.h>

#include "WaveTrack.h"
#include "MixerOptions.h"

#include "ShuttleGui.h"
#include "HelpSystem.h"

enum
{
    ID_MIXERPANEL = 10001,
    ID_SLIDER_CHANNEL
};

BEGIN_EVENT_TABLE(ExportMixerDialog, wxDialogWrapper)
EVT_BUTTON(wxID_OK, ExportMixerDialog::OnOk)
EVT_BUTTON(wxID_CANCEL, ExportMixerDialog::OnCancel)
EVT_BUTTON(wxID_HELP, ExportMixerDialog::OnMixerPanelHelp)
EVT_SIZE(ExportMixerDialog::OnSize)
EVT_SLIDER(ID_SLIDER_CHANNEL, ExportMixerDialog::OnSlider)
END_EVENT_TABLE()

ExportMixerDialog::~ExportMixerDialog() = default;

ExportMixerDialog::ExportMixerDialog(TrackIterRange<const WaveTrack> tracks, MixerOptions::Downmix* mixerSpec,
                                     wxWindow* parent, wxWindowID id, const TranslatableString& title,
                                     const wxPoint& position, const wxSize& size, long style)
    : wxDialogWrapper(parent, id, title, position, size, style | wxRESIZE_BORDER)
    , mMixerSpec(mixerSpec)
{
    SetName();

    for (auto t : tracks) {
        const wxString sTrackName = (t->GetName()).Left(20);
        if (IsMono(*t)) {
            // No matter whether it's panned hard left or right
            mTrackNames.push_back(sTrackName);
        } else {
            /* i18n-hint: track name and L abbreviating Left channel */
            mTrackNames.push_back(XO("%s - L").Format(sTrackName).Translation());
            /* i18n-hint: track name and R abbreviating Right channel */
            mTrackNames.push_back(XO("%s - R").Format(sTrackName).Translation());
        }
    }

    auto label = XO("Output Channels: %2d")
                 .Format(mMixerSpec->GetNumChannels());

    ShuttleGui S{ this, eIsCreating };
    {
        S.SetBorder(5);

        auto mixerPanel = safenew ExportMixerPanel(
            S.GetParent(), ID_MIXERPANEL, mMixerSpec,
            mTrackNames, wxDefaultPosition, wxSize(400, -1));
        S.Prop(1)
        .Name(XO("Mixer Panel"))
        .Position(wxEXPAND | wxALL)
        .AddWindow(mixerPanel);

        S.StartHorizontalLay(wxALIGN_CENTER | wxALL, 0);
        {
            mChannelsText = S.AddVariableText(
                label,
                false, wxALIGN_LEFT | wxALL);

            if (mMixerSpec->GetMaxNumChannels() > 1) {
                S
                .Id(ID_SLIDER_CHANNEL)
                .Name(label)
                .Size({ 300, -1 })
                .Style(wxSL_HORIZONTAL)
                .Position(wxEXPAND | wxALL)
                .AddSlider({},
                           mMixerSpec->GetNumChannels(),
                           mMixerSpec->GetMaxNumChannels(), 1);
            }
        }
        S.EndHorizontalLay();

        S.AddStandardButtons(eCancelButton | eOkButton | eHelpButton);
    }

    SetAutoLayout(true);
    GetSizer()->Fit(this);
    GetSizer()->SetSizeHints(this);

    SetSizeHints(640, 480, 20000, 20000);

    SetSize(640, 480);
    Center();
}

void ExportMixerDialog::OnSize(wxSizeEvent& event)
{
    ExportMixerPanel* pnl = ((ExportMixerPanel*)FindWindow(ID_MIXERPANEL));
    pnl->Refresh(false);
    event.Skip();
}

void ExportMixerDialog::OnSlider(wxCommandEvent& WXUNUSED(event))
{
    wxSlider* channels = (wxSlider*)FindWindow(ID_SLIDER_CHANNEL);
    ExportMixerPanel* pnl = ((ExportMixerPanel*)FindWindow(ID_MIXERPANEL));
    mMixerSpec->SetNumChannels(channels->GetValue());
    pnl->Refresh(false);
    wxString label;
    label.Printf(_("Output Channels: %2d"), mMixerSpec->GetNumChannels());
    mChannelsText->SetLabel(label);
    channels->SetName(label);
}

void ExportMixerDialog::OnOk(wxCommandEvent& WXUNUSED(event))
{
    EndModal(wxID_OK);
}

void ExportMixerDialog::OnCancel(wxCommandEvent& WXUNUSED(event))
{
    EndModal(wxID_CANCEL);
}

void ExportMixerDialog::OnMixerPanelHelp(wxCommandEvent& WXUNUSED(event))
{
    HelpSystem::ShowHelp(this, L"Advanced_Mixing_Options", true);
}
