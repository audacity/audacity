/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudioComModule.cpp

  Dmitry Vedenko

**********************************************************************/

#include "ModuleConstants.h"
#include "PluginHost.h"

#include "ui/images/CloudImages.hpp"

DEFINE_VERSION_CHECK
extern "C" DLL_API int ModuleDispatch(ModuleDispatchTypes type)
{
    static auto cloudImages = []
    {
        if (!PluginHost::IsHostProcess()) {
            bin2c_init_CLOUDIMAGES_HPP();
        }
        return true;
    }();

    return 1;
}
