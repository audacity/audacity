/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/global/modularity/imoduleinterface.h"

namespace au::effects {
class IMissingEffectChecker : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IMissingEffectChecker)

public:
    virtual ~IMissingEffectChecker() = default;

    virtual void warnIfEffectsMissing() = 0;
};
}
