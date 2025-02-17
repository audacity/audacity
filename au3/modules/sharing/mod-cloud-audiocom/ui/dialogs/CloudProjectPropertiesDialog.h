/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectPropertiesDialog.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <string>
#include <utility>

#include "wxPanelWrapper.h"

#include "Observer.h"

class wxTextCtrl;
class wxChoice;
class wxButton;
class wxStaticText;
enum class AudiocomTrace;

namespace audacity::cloud::audiocom {
class ServiceConfig;
class OAuthService;
class UserService;
class UserPanel;
} // namespace audacity::cloud::audiocom

namespace audacity::cloud::audiocom::sync {
class CloudProjectPropertiesDialog final : public wxDialogWrapper
{
    CloudProjectPropertiesDialog(
        const ServiceConfig& serviceConfig, OAuthService& authService, UserService& userService, const wxString& projectName,
        wxWindow* parent, AudiocomTrace);
    ~CloudProjectPropertiesDialog() override;

public:
    enum class Action
    {
        Cancel,
        SaveToCloud,
        SaveLocally
    };

    static std::pair<Action, std::string> Show(
        const ServiceConfig& serviceConfig, OAuthService& authService, UserService& userService, const wxString& projectName,
        wxWindow* parent, bool allowLocalSave, AudiocomTrace);

private:
    bool OnSubmit();
    void LayoutControls();
    void SetupEvents();

    std::string GetProjectName() const;
    void OnUpdateCloudSaveState();

    UserPanel* mUserPanel {};
    wxTextCtrl* mProjectName {};

    wxButton* mSaveToCloud {};
    wxButton* mSaveLocally {};
    wxButton* mCancel {};
};
} // namespace audacity::cloud::audiocom::sync
