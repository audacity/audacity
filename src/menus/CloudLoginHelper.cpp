/**********************************************************************
Audacity: A Digital Audio Editor
**********************************************************************/

#include "CloudLoginHelper.h"

CloudLoginHelper& CloudLoginHelper::Get()
{
   static CloudLoginHelper instance;
   return instance;
}

void CloudLoginHelper::ShowLoginDialog()
{
   Publish({});
}

Observer::Subscription CloudLoginHelper::Subscribe(Callback callback)
{
   return Publisher::Subscribe(callback);
}
