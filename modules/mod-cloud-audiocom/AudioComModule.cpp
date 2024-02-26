/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudioComModule.cpp

  Dmitry Vedenko

**********************************************************************/

#include "ModuleConstants.h"

#include "ui/images/CloudImages.hpp"

#include "ui/dialogs/UploadCanceledDialog.h"

#include "ServiceConfig.h"
#include "OAuthService.h"
#include "UserService.h"

DEFINE_VERSION_CHECK
extern "C" DLL_API int ModuleDispatch(ModuleDispatchTypes type)
{
   static auto cloudImages = []
   {
      bin2c_init_CLOUDIMAGES_HPP();
      return true;
   }();

   switch (type)
   {
   case ModuleInitialize:
   {

      //audacity::cloud::audiocom::sync::UploadCanceledDialog { nullptr }
      //   .ShowDialog();
     // exit(0);
   }
      break;
   default:
      break;
   }
   return 1;
}
