/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imoduleinterface.h"

#include "global/io/path.h"

namespace au::effects {
class IEffectsConfiguration : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IEffectsConfiguration)
public:

    virtual ~IEffectsConfiguration() = default;

    virtual muse::io::path_t defaultPath() const = 0;
};
}
