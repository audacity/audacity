/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudLocationDialog.cpp

  Dmitry Vedenko

**********************************************************************/

#include "CloudLocationDialog.h"

#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/sizer.h>
#include <wx/statbmp.h>
#include <wx/statline.h>
#include <wx/stattext.h>

#include "AccessibilityUtils.h"

#include "CloudModuleSettings.h"

#include "../images/CloudImages.hpp"

namespace audacity::cloud::audiocom::sync {
namespace {
enum class ChoiceMode
{
    Local,
    User
};
struct DialogDescription final
{
    TranslatableString DialogTitle;

    // This pointer to the image is initialized on module load, we have to use a
    // pointer pointer here
    wxBitmap** RemoteImage;
    TranslatableString RemoteTitle;
    TranslatableString RemoteDescription;
    TranslatableString RemoteButtonLabel;

    wxBitmap** LocalImage;
    TranslatableString LocalTitle;
    TranslatableString LocalDescription;
    TranslatableString LocalButtonTitle;

    TranslatableString DoNotShowLabel;

    EnumSetting<CloudLocationMode>& LocationMode;

    ChoiceMode RememberChoiceMode;
};

const DialogDescription SaveDialogDescription = {
    /* i18n-hint: A title that is shown on the first project save that allows the
       user to select Cloud or local save. */
    XO("How would you like to save?"),
    &bin2c_SaveRemote_png,
    XO("Save to the Cloud (free)"),
    XO(
        "Your project is backed up privately on audio.com. You can access your work from any device and collaborate on your project with others. Cloud saving is free for a limited number of projects."),
    XXO("&Save to Cloud"),
    &bin2c_SaveLocally_png,
    XO("On your computer"),
    XO("Files are saved on your device.\nNote: To export MP3 and WAV files, use File > Export Audio instead."),
    XXO("Save to &computer"),
    XO("&Remember my choice and don't show again"),
    SaveLocationMode,
    ChoiceMode::User,
};

const DialogDescription ExportDialogDescription = {
    /* i18n-hint: A title that is shown on export that allows the user to select
       Cloud or local export. */
    XO("How would you like to export?"),
    &bin2c_ExportRemote_png,
    XO("Share to audio.com"),
    XO(
        "Uploads an uncompressed audio file and generates a shareable link. This link allows others to download the file in either .wav or .mp3 format."),
    XXO("&Share to audio.com"),
    &bin2c_ExportLocally_png,
    XO("On your computer"),
    XO("Export MP3s, WAVs, FLACs and other formats to your computer."),
    XXO("Export to &computer"),
    XO("&Don't show again"),
    ExportLocationMode,
    ChoiceMode::Local,
};

constexpr auto leftPadding = 16;

auto GetWrapWidth()
{
    return bin2c_SaveLocally_png->GetWidth() - leftPadding * 2;
}

std::unique_ptr<wxBoxSizer> SetupVerticalSizer(
    wxStaticBitmap* image, wxStaticText* title, wxStaticText* description,
    wxButton* button)
{
    const auto leftPaddingFlags = wxSizerFlags {}.Border(wxLEFT, leftPadding);

    auto sizer = std::make_unique<wxBoxSizer>(wxVERTICAL);

    sizer->Add(image);
    sizer->AddSpacer(24);
    sizer->Add(title, leftPaddingFlags);
    sizer->AddSpacer(12);
    sizer->Add(description, leftPaddingFlags);
    sizer->AddSpacer(40);
    sizer->AddStretchSpacer(1);
    sizer->Add(button, leftPaddingFlags);

    return sizer;
}

wxButton* CreateButton(
    wxWindow* parent, const wxFont& font, const TranslatableString& label)
{
    const auto transalatedLabel = label.Translation();

    auto button = safenew wxButton(parent, wxID_ANY, transalatedLabel);

    button->SetFont(font);

    const auto textSize = button->GetTextExtent(transalatedLabel);

    button->SetMinSize({ textSize.x + 12 * 2, 32 });

    return button;
}
} // namespace

CloudLocationDialog::CloudLocationDialog(
    wxWindow* parent, LocationDialogType type)
    : wxDialogWrapper{parent,         wxID_ANY,
                      XO("Audacity"), wxDefaultPosition,
                      { 442, -1 },    wxDEFAULT_DIALOG_STYLE}
    , mType{type}
{
    auto& description = type == LocationDialogType::Save
                        ? SaveDialogDescription
                        : ExportDialogDescription;

    wxFont titleFont = GetFont();
    titleFont.SetWeight(wxFONTWEIGHT_BOLD);
    titleFont.SetPixelSize({ 0, 18 });

    wxFont descriptionFont = GetFont();
    descriptionFont.SetPixelSize({ 0, 14 });

    auto title = safenew wxStaticText(
        this, wxID_ANY, description.DialogTitle.Translation());
    title->SetFont(titleFont);

    auto saveToCloudImage
        =safenew wxStaticBitmap(this, wxID_ANY, **description.RemoteImage);

    auto saveToCloudTitle = safenew wxStaticText(
        this, wxID_ANY, description.RemoteTitle.Translation());
    saveToCloudTitle->SetFont(titleFont);

    auto saveToCloudDescription = safenew wxStaticText(
        this, wxID_ANY, description.RemoteDescription.Translation());
    saveToCloudDescription->SetFont(descriptionFont);
    saveToCloudDescription->Wrap(GetWrapWidth());

    auto saveToCloudButton
        =CreateButton(this, descriptionFont, description.RemoteButtonLabel);

    auto saveToComputerImage
        =safenew wxStaticBitmap(this, wxID_ANY, **description.LocalImage);

    auto saveToComputerTitle = safenew wxStaticText(
        this, wxID_ANY, description.LocalTitle.Translation());
    saveToComputerTitle->SetFont(titleFont);

    auto saveToComputerDescription = safenew wxStaticText(
        this, wxID_ANY, description.LocalDescription.Translation());
    saveToComputerDescription->SetFont(descriptionFont);
    saveToComputerDescription->Wrap(GetWrapWidth());

    auto saveToComputerButton
        =CreateButton(this, descriptionFont, description.LocalButtonTitle);

    auto rememberChoiceCheckbox = safenew wxCheckBox(
        this, wxID_ANY,
        description.DoNotShowLabel.Translation());

    mDoNotShow = description.LocationMode.ReadEnum() != CloudLocationMode::Ask;
    rememberChoiceCheckbox->SetValue(mDoNotShow);

    auto sizer = safenew wxBoxSizer(wxVERTICAL);

    sizer->Add(title, wxSizerFlags {}.CenterHorizontal().Border(wxTOP, 16));

    auto topSizer = safenew wxBoxSizer(wxHORIZONTAL);

    auto cloudSizer = SetupVerticalSizer(
        saveToCloudImage, saveToCloudTitle, saveToCloudDescription,
        saveToCloudButton);

    topSizer->Add(cloudSizer.release(), wxSizerFlags {}.Expand());

    auto computerSizer = SetupVerticalSizer(
        saveToComputerImage, saveToComputerTitle, saveToComputerDescription,
        saveToComputerButton);

    topSizer->Add(
        computerSizer.release(), wxSizerFlags {}.Expand().Border(wxLEFT, 8));

    sizer->Add(topSizer, wxSizerFlags {}.Expand().Border(wxALL, 16));

    sizer->Add(safenew wxStaticLine(this), wxSizerFlags {}.Expand());

    sizer->Add(rememberChoiceCheckbox, wxSizerFlags {}.Border(wxALL, 16));

    SetSizerAndFit(sizer);
    SetupAccessibility(this);

    Center();

    Bind(
        wxEVT_CHAR_HOOK,
        [this](auto& evt)
    {
        if (!IsEscapeKey(evt)) {
            evt.Skip();
            return;
        }

        EndModal(wxID_CANCEL);
    });

    rememberChoiceCheckbox->Bind(
        wxEVT_CHECKBOX, [this, rememberChoiceCheckbox](auto)
    { mDoNotShow = rememberChoiceCheckbox->GetValue(); });

    saveToCloudButton->Bind(wxEVT_BUTTON, [this](auto) { EndModal(wxID_SAVE); });
    saveToComputerButton->Bind(
        wxEVT_BUTTON, [this](auto) { EndModal(wxID_OK); });
}

CloudLocationDialog::~CloudLocationDialog()
{
}

LocationDialogResult CloudLocationDialog::ShowDialog()
{
    auto& description = mType == LocationDialogType::Save
                        ? SaveDialogDescription
                        : ExportDialogDescription;

    if (description.LocationMode.ReadEnum() != CloudLocationMode::Ask) {
        return description.LocationMode.ReadEnum() == CloudLocationMode::Cloud
               ? LocationDialogResult::Cloud
               : LocationDialogResult::Local;
    }

    const auto result = ShowModal();

    if (result == wxID_OK) {
        if (mDoNotShow) {
            description.LocationMode.WriteEnum(CloudLocationMode::Local);
            gPrefs->Flush();
        }
        return LocationDialogResult::Local;
    }

    if (result == wxID_SAVE) {
        if (mDoNotShow) {
            description.LocationMode.WriteEnum(
                description.RememberChoiceMode == ChoiceMode::User
                ? CloudLocationMode::Cloud
                : CloudLocationMode::Local);
            gPrefs->Flush();
        }

        return LocationDialogResult::Cloud;
    }

    return LocationDialogResult::Cancel;
}
} // namespace audacity::cloud::audiocom::sync
