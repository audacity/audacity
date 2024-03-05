/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudioComModule.cpp

  Dmitry Vedenko

**********************************************************************/

#include "ModuleConstants.h"

DEFINE_VERSION_CHECK
extern "C" DLL_API int ModuleDispatch(ModuleDispatchTypes type)
{
   return 1;
}
