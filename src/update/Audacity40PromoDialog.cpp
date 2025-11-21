/*!********************************************************************
 Audacity: A Digital Audio Editor

 @file Audacity40PromoDialog.cpp
 @brief Promotional dialog for Audacity 4.0 release.

 **********************************************************************/

#include "Audacity40PromoDialog.h"

#include "HyperLink.h"

#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/statbmp.h>
#include <wx/button.h>
#include <wx/utils.h>
#include <wx/image.h>
#include <wx/mstream.h>
#include <wx/log.h>

#include "../../images/Audacity40Promo.h"

namespace
{
    // TODO: Update these links with final URLs
    constexpr auto InstallAudacity4Link = "https://www.audacityteam.org/download/";
    constexpr auto FeatureBreakdownLink = "https://www.audacityteam.org/blog/audacity-4-0-release/";

    wxBitmap LoadEmbeddedPNG(const unsigned char* data, size_t len)
    {
        wxMemoryInputStream stream(data, len);
        wxImage image;
        if (!image.LoadFile(stream, wxBITMAP_TYPE_PNG))
        {
            wxLogError("Failed to load embedded PNG image.");
            return wxBitmap();
        }
        return wxBitmap(image);
    }
}

Audacity40PromoDialog::Audacity40PromoDialog(wxWindow* parent, const Notification& notification)
    : wxDialogWrapper(parent, wxID_ANY, XO("Install Audacity 4"),
                     wxDefaultPosition, wxDefaultSize,
                     wxDEFAULT_DIALOG_STYLE)
{
    auto mainSizer = safenew wxBoxSizer(wxVERTICAL);

    auto descText = safenew wxStaticText(this, wxID_ANY,
        _("Audacity 4 is here! Packed with hundreds of upgrades that make it faster and\neasier to use than ever."));
    mainSizer->Add(descText, 0, wxALL, 15);

    wxBitmap bitmap = LoadEmbeddedPNG(Audacity_4_0_Promo_png, Audacity_4_0_Promo_png_len);
    if (bitmap.IsOk())
    {
        auto bitmapCtrl = safenew wxStaticBitmap(this, wxID_ANY, bitmap);
        mainSizer->Add(bitmapCtrl, 0, wxLEFT | wxRIGHT, 15);
        mainSizer->AddSpacer(20);
    }

    auto footerText = safenew wxStaticText(this, wxID_ANY,
        _("Installation does not affect Audacity 3, so your existing setup remains untouched."));
    mainSizer->Add(footerText, 0, wxLEFT | wxRIGHT, 15);

    mainSizer->AddSpacer(5);
    auto link = safenew HyperLink(this, wxID_ANY,
        _("Discover what's new in our full feature breakdown."),
        FeatureBreakdownLink);
    mainSizer->Add(link, 0, wxLEFT | wxRIGHT, 15);

    mainSizer->AddSpacer(40);

    auto buttonSizer = safenew wxBoxSizer(wxHORIZONTAL);
    buttonSizer->AddStretchSpacer();

    auto noThanksBtn = safenew wxButton(this, wxID_CANCEL, _("No thanks"));
    buttonSizer->Add(noThanksBtn, 0, wxRIGHT, 5);

    auto remindLaterBtn = safenew wxButton(this, wxID_APPLY, _("Remind me later"));
    buttonSizer->Add(remindLaterBtn, 0, wxRIGHT, 5);

    auto installBtn = safenew wxButton(this, wxID_OK, _("Install Audacity 4"));
    installBtn->SetDefault();
    buttonSizer->Add(installBtn, 0);

    mainSizer->Add(buttonSizer, 0, wxEXPAND | wxLEFT | wxRIGHT | wxBOTTOM, 15);

    SetSizerAndFit(mainSizer);
    Center();

    Bind(wxEVT_BUTTON, [this](wxCommandEvent& evt) {
        if (evt.GetId() == wxID_OK)
        {
            wxLaunchDefaultBrowser(InstallAudacity4Link);
        }
        EndModal(evt.GetId());
    });
}
