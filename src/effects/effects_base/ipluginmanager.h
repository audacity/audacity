/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/modularity/imoduleinterface.h"

namespace au::effects {
class IPluginManager : MODULE_GLOBAL_EXPORT_INTERFACE
{
    INTERFACE_ID(IPluginManager)
public:

    virtual ~IPluginManager() = default;

    virtual void initialize() = 0;
    virtual void terminate() = 0;
};
}
