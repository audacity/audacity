/**********************************************************************
Audacity: A Digital Audio Editor
**********************************************************************/

#pragma once

#include "Observer.h"

class AUDACITY_DLL_API CloudLoginHelper final : public Observer::Publisher<Observer::Message, false>
{
private:
   CloudLoginHelper() = default;

public:
   static CloudLoginHelper &Get();
   Observer::Subscription Subscribe(Callback callback);
   void ShowLoginDialog();
};

