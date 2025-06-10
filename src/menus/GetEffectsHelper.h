/**********************************************************************
Audacity: A Digital Audio Editor
**********************************************************************/

#pragma once

#include "Observer.h"

enum class GetEffectsHandlerType
{
   Default, // Default handler that opens the default browser to the "Get Effects" URL
   Custom   // Custom handler that overrides the default behavior
};

class AUDACITY_DLL_API GetEffectsHelper final : public Observer::Publisher<Observer::Message, false>
{
private:
   GetEffectsHelper() = default;

public:
   static GetEffectsHelper &Get();
   Observer::Subscription Subscribe(Callback callback, GetEffectsHandlerType handlerType = GetEffectsHandlerType::Default);
   void GetEffects();

private:
   GetEffectsHandlerType mHandlerType = GetEffectsHandlerType::Default;

};
