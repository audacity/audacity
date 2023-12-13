/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectPropertiesDialog.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "wxPanelWrapper.h"

#include "sync/CloudSyncUI.h"

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
   static FirstSaveResult Show(
      const ServiceConfig& serviceConfig, OAuthService& authService,
      UserService& userService, const wxString& projectName, wxWindow* parent);

private:
   void LayoutControls();
   void SetupEvents();

   wxString GetProjectName() const;
   void OnUpdateCloudSaveState();

   UserPanel* mUserPanel {};
   wxTextCtrl* mProjectName {};

   wxChoice* mProjectVisibility {};

   wxButton* mSaveToCloud {};
   wxButton* mSaveLocally {};
   wxButton* mCancel {};

   Observer::Subscription mUserStateChangedSubscription {};
};
} // namespace cloud::audiocom::sync
