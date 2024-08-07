/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imoduleinterface.h"

class QQmlEngine;
namespace au::effects {
class IEffectsUiEngine : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IEffectsUiEngine)

public:
    virtual ~IEffectsUiEngine() = default;

    virtual QQmlEngine* qmlEngine() const = 0;
};
}
