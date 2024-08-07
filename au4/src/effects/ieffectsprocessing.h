/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "global/types/string.h"

#include "modularity/imoduleinterface.h"

class Effect;

namespace au::effects {
class IEffectsProcessing : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IEffectsProcessing)
public:
    virtual void process(const muse::String& effectId) = 0;

    virtual void regEffect(const muse::String& effectId, Effect* effect) = 0;
    virtual void unregEffect(const muse::String& effectId) = 0;
};
}
