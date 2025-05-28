/**********************************************************************
Audacity: A Digital Audio Editor
**********************************************************************/

#include "GetEffectsHelper.h"

#include "AppEvents.h"
#include "BasicUI.h"


static const wxString GetEffectsDefaultUrl = "https://www.audacityteam.org/mh-effectmenu";

GetEffectsHelper& GetEffectsHelper::Get()
{
   static GetEffectsHelper instance;
   return instance;
}

void GetEffectsHelper::GetEffects()
{
   Publish({});
}

Observer::Subscription GetEffectsHelper::Subscribe(Callback callback, GetEffectsHandlerType handlerType)
{
   // If the custom handler is already set, do not allow overriding it
   if (mHandlerType == GetEffectsHandlerType::Custom)
   {
      return {};
   }
   mHandlerType = handlerType;
   return Publisher::Subscribe(callback);
}

// Default handler for the "Get Effects" action when no other handler is registered.
class DefaultGetEffectsHandler final
{
public:
   DefaultGetEffectsHandler() {
      AppEvents::OnAppInitialized([this] {
         mSubscription = GetEffectsHelper::Get().Subscribe(
            [this](const auto&) -> bool {
               BasicUI::OpenInDefaultBrowser(GetEffectsDefaultUrl);
               return true;
            });
      });
   }

private:
   Observer::Subscription mSubscription;

};

static DefaultGetEffectsHandler sDefaultGetEffectsHandler;
