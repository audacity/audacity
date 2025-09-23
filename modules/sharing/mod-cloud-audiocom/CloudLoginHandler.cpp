#include <wx/app.h>
#include <wx/event.h>
#include <wx/window.h>

#include "AppEvents.h"
#include "BasicUI.h"
#include "CodeConversions.h"
#include "ui/dialogs/LoginDialog.h"
#include "OAuthService.h"
#include "UserService.h"
#include "ServiceConfig.h"
#include "menus/CloudLoginHelper.h"

class CloudLoginHandler
{

public:
   CloudLoginHandler() {
      AppEvents::OnAppInitialized([this] {
         mSubscription = CloudLoginHelper::Get().Subscribe([this](const auto&) {
            HandleCloudLogin();
            return true;
         });
      });
   }

   bool HandleCloudLogin() {
      using namespace audacity::cloud::audiocom;

      auto& oauthService = GetOAuthService();
      if (!oauthService.HasRefreshToken() && !ShowLoginDialog())
         return false;

      if (!oauthService.HasAccessToken())
         oauthService.ValidateAuth({}, {}, true);

      auto& userService = GetUserService();
      userService.UpdateUserData();

      mUserServiceSubscription = userService.Subscribe([this, &userService, &oauthService](const auto&) {
         if (userService.GetUserSlug().empty() && !ShowLoginDialog())
            return;

         auto& serviceConfig = GetServiceConfig();

         const auto userId = audacity::ToUTF8(userService.GetUserId());
         const auto tourPage = serviceConfig.GetTourPage();
         const auto url = oauthService.MakeAudioComAuthorizeURL(userId, tourPage);
         BasicUI::OpenInDefaultBrowser(url);
         // One shot subscription
         mUserServiceSubscription.Reset();
      });

      return true;
   }

   wxWindow *GetParent() const {
      if (wxTopLevelWindows.IsEmpty())
         return nullptr;
      return wxTopLevelWindows.GetLast()->GetData();
   }

   bool ShowLoginDialog() {
      bool result = LoginDialog::SignIn(
         GetParent(),
         LoginDialog::Mode::Create);
      return result;
   }

private:
   Observer::Subscription mSubscription;
   Observer::Subscription mUserServiceSubscription;
};

static CloudLoginHandler sEventHandler;
