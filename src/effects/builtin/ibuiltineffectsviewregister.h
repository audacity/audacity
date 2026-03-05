/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "global/types/string.h"

#include "modularity/imoduleinterface.h"

namespace au::effects {
class IBuiltinEffectsViewRegister : MODULE_GLOBAL_INTERFACE
{
    INTERFACE_ID(IBuiltinEffectsViewRegister)

public:
    virtual ~IBuiltinEffectsViewRegister() = default;

    virtual void setDefaultUrl(const muse::String& viewUrl) = 0;
    virtual void regUrl(const muse::String& effectName, const muse::String& viewUrl) = 0;

    virtual const muse::String& viewUrl(const muse::String& effectName) const = 0;
};
}
