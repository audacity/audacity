/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudioComPrefsPanel.h

  Dmitry Vedenko

**********************************************************************/

#include <cassert>

#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/radiobut.h>
#include <wx/textctrl.h>

#include "PrefsPanel.h"

#include "CloudLibrarySettings.h"
#include "CloudModuleSettings.h"
#include "ExportUtils.h"
#include "ShuttleGui.h"
#include "UserPanel.h"

#include "OAuthService.h"
#include "ServiceConfig.h"
#include "UserService.h"

#include "TempDirectory.h"

namespace {
using namespace audacity::cloud::audiocom;

class AudioComPrefsPanel final : public PrefsPanel
{
public:
    AudioComPrefsPanel(wxWindow* parent, wxWindowID winid)
        : PrefsPanel{parent, winid, XO("Cloud")}
    {
        ShuttleGui S(this, eIsCreatingFromPrefs);
        PopulateOrExchange(S);

        mSaveLocationMode   = sync::SaveLocationMode.ReadEnum();
        mExportLocationMode = sync::ExportLocationMode.ReadEnum();
    }

    bool Commit() override
    {
        ShuttleGui S(this, eIsSavingToPrefs);
        PopulateOrExchange(S);

        CloudProjectsSavePath.Invalidate();
        DaysToKeepFiles.Invalidate();

        // Enum settings are not cacheable, so we need to invalidate them
        // sync::SaveLocationMode.Invalidate();
        // sync::ExportLocationMode.Invalidate();
        sync::MixdownDialogShown.Invalidate();

        return true;
    }

    void Cancel() override
    {
        // ChoiceSetting ignores transactions, so we need to reset the values
        sync::SaveLocationMode.WriteEnum(mSaveLocationMode);
        sync::ExportLocationMode.WriteEnum(mExportLocationMode);
    }

    void PopulateOrExchange(ShuttleGui& S) override
    {
        S.SetBorder(2);
        S.StartScroller();
        {
            S.SetBorder(8);

            S.StartStatic(XO("Account"));
            {
                S.SetBorder(8);
                S.AddWindow(
                    safenew UserPanel { GetServiceConfig(), GetOAuthService(),
                                        GetUserService(), UserPanel::LinkMode::Link,
                                        AudiocomTrace::PrefsPanel, S.GetParent() },
                    wxEXPAND);
            }
            S.EndStatic();

            S.StartStatic(XO("Export behavior"));
            {
                S.SetBorder(8);
                auto checkBox = S.AddCheckBox(
                    XO("S&how 'How would you like to export?' dialog"),
                    sync::ExportLocationMode.ReadEnum()
                    == sync::CloudLocationMode::Ask);

                checkBox->Bind(
                    wxEVT_CHECKBOX,
                    [this](auto& event)
                {
                    sync::ExportLocationMode.WriteEnum(
                        event.IsChecked() ? sync::CloudLocationMode::Ask
                        : sync::CloudLocationMode::Local);
                });
            }
            S.EndStatic();

            S.StartStatic(XO("Save behavior"));
            {
                S.SetBorder(4);

                const auto initialMode
                    =static_cast<int>(sync::SaveLocationMode.ReadEnum());

                auto BindRadioButton = [](auto* button, auto mode)
                {
                    button->Bind(
                        wxEVT_RADIOBUTTON,
                        [mode](auto& event)
                    {
                        sync::SaveLocationMode.WriteEnum(
                            static_cast<sync::CloudLocationMode>(mode));
                    });
                };

                S.StartInvisiblePanel(4);
                {
                    BindRadioButton(S.AddRadioButton(
                                        XO("Always &ask"),
                                        static_cast<int>(sync::CloudLocationMode::Ask),
                                        initialMode),
                                    sync::CloudLocationMode::Ask);
                    BindRadioButton(S.AddRadioButtonToGroup(
                                        XO("Always &save to cloud"),
                                        static_cast<int>(sync::CloudLocationMode::Cloud),
                                        initialMode),
                                    sync::CloudLocationMode::Cloud);
                    BindRadioButton(S.AddRadioButtonToGroup(
                                        XO("Always save to the co&mputer"),
                                        static_cast<int>(sync::CloudLocationMode::Local),
                                        initialMode),
                                    sync::CloudLocationMode::Local);
                }
                S.EndInvisiblePanel();
            }
            S.EndStatic();

            S.StartStatic(XO("Temporary Cloud files directory"));
            {
                S.SetBorder(8);
                S.StartMultiColumn(3, wxEXPAND);
                {
                    S.SetStretchyCol(1);

                    mCloudProjectsSavePath = S.TieTextBox(XXO("&Location:"), CloudProjectsSavePath, 30);
                    S.AddButton(XXO("&Browse..."))
                    ->Bind(wxEVT_BUTTON, [this](auto&) { Browse(); });
                }
                S.EndMultiColumn();

                S.SetBorder(8);
                S.StartMultiColumn(3);
                {
                    S.NameSuffix(XO("days"))
                    .TieIntegerTextBox(
                        XXO("&Remove temporary files after:"), DaysToKeepFiles, 10);
                    S.AddFixedText(XO("days"), true);
                }
                S.EndMultiColumn();
            }
            S.EndStatic();
        }
        S.EndScroller();
    }

    ComponentInterfaceSymbol GetSymbol() const override
    {
        return ComponentInterfaceSymbol { XO("Cloud") };
    }

    TranslatableString GetDescription() const override
    {
        return XO("Preferences for Cloud");
    }

    void Browse()
    {
        const auto currentPath = CloudProjectsSavePath.Read();

        wxDirDialogWrapper dlog(
            this, XO("Choose a location to place the temporary directory"),
            currentPath);
        int retval = dlog.ShowModal();

        if (retval != wxID_CANCEL && !dlog.GetPath().empty()) {
            wxFileName tmpDirPath;
            tmpDirPath.AssignDir(dlog.GetPath());

            if (TempDirectory::FATFilesystemDenied(
                    tmpDirPath.GetFullPath(),
                    XO("Temporary files directory cannot be on a FAT drive."))) {
                return;
            }

            if (!FileNames::WritableLocationCheck(
                    dlog.GetPath(), XO("Cannot set the preference."))) {
                return;
            }

            mCloudProjectsSavePath->SetValue(
                tmpDirPath.GetPath(wxPATH_GET_VOLUME | wxPATH_GET_SEPARATOR));
        }
    }

private:
    wxTextCtrl* mCloudProjectsSavePath {};
    Observer::Subscription mFrequencySubscription;

    sync::CloudLocationMode mSaveLocationMode;
    sync::CloudLocationMode mExportLocationMode;
}; // class AudioComPrefsPanel

PrefsPanel::Registration sAttachment {
    "AudioComPrefsPanel",
    [](wxWindow* parent, wxWindowID winid, AudacityProject*) -> PrefsPanel*
    {
        assert(parent != nullptr);
        return parent != nullptr ? safenew AudioComPrefsPanel(parent, winid)
               : nullptr;
    },
    false,
    // Register with an explicit ordering hint because this one might be
    // absent
    { "", { Registry::OrderingHint::End, {} } }
};
} // namespace
