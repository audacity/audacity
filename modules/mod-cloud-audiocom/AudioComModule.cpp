/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudioComModule.cpp

  Dmitry Vedenko

**********************************************************************/

#include "ModuleConstants.h"

#include "ui/MixdownPropertiesDialog.h"

#include "ServiceConfig.h"
#include "OAuthService.h"
#include "UserService.h"

DEFINE_VERSION_CHECK
extern "C" DLL_API int ModuleDispatch(ModuleDispatchTypes type)
{
   switch (type)
   {
   case ModuleInitialize:
   {
//       auto r = cloud::audiocom::sync::MixdownPropertiesDialog::Show(nullptr);
// 
//       exit(0);
   }
      break;
   default:
      break;
   }
   return 1;
}
