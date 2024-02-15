/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectPropertiesDialog.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <utility>

#include "wxPanelWrapper.h"

#include "Observer.h"

class wxTextCtrl;
class wxChoice;
class wxButton;

namespace cloud::audiocom
{
class ServiceConfig;
class OAuthService;
class UserService;
class UserPanel;
} // namespace cloud::audiocom

namespace cloud::audiocom::sync
{
   
class CloudProjectPropertiesDialog final : public wxDialogWrapper
{
   CloudProjectPropertiesDialog(
      const ServiceConfig& serviceConfig, OAuthService& authService,
      UserService& userService, const wxString& projectName, wxWindow* parent);
   ~CloudProjectPropertiesDialog() override;

public:
   enum class Action
   {
      Cancel,
      SaveToCloud,
      SaveLocally
   };

   static std::pair<Action, wxString> Show(
      const ServiceConfig& serviceConfig, OAuthService& authService,
      UserService& userService, const wxString& projectName, wxWindow* parent, bool allowLocalSave);

private:
   void LayoutControls();
   void SetupEvents();

   wxString GetProjectName() const;
   void OnUpdateCloudSaveState();

   UserPanel* mUserPanel {};
   wxTextCtrl* mProjectName {};

   wxButton* mSaveToCloud {};
   wxButton* mSaveLocally {};
   wxButton* mCancel {};

   Observer::Subscription mUserStateChangedSubscription {};
};
} // namespace cloud::audiocom::sync
