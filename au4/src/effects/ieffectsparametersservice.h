/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "async/channel.h"

#include "modularity/imoduleinterface.h"

#include "effectstypes.h"

namespace au::effects {
class IEffectsParametersService : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IEffectsParametersService)

public:
    virtual ~IEffectsParametersService() = default;

    virtual void regEffectParameters(const muse::String& effectId, const EffectParameters& parameters) = 0;
    virtual EffectParameters& effectParameters(const muse::String& effectId) const = 0;
    virtual muse::async::Channel<EffectParameter> effectParameterChanged(const muse::String& effectId) const = 0;
};
}
