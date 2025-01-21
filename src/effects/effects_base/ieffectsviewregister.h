/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "global/types/string.h"

#include "modularity/imoduleinterface.h"

namespace au::effects {
//! TODO Move to builtin module
class IEffectsViewRegister : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IEffectsViewRegister)

public:
    virtual ~IEffectsViewRegister() = default;

    virtual void setDefaultUrl(const muse::String& viewUrl) = 0;
    virtual void regUrl(const muse::String& effectName, const muse::String& viewUrl) = 0;

    virtual const muse::String& viewUrl(const muse::String& effectName) const = 0;
};
}
