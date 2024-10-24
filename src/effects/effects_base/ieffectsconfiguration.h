/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imoduleinterface.h"

#include "global/types/secs.h"

namespace au::effects {
class IEffectsConfiguration : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IEffectsConfiguration)
public:

    virtual ~IEffectsConfiguration() = default;

    virtual muse::secs_t previewDuration() const = 0;
};
}
