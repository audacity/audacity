#pragma once

#include "async/channel.h"
#include "modularity/imoduleinterface.h"
#include "types/projectscenetypes.h"

namespace au::projectscene {
class IProjectSceneConfiguration : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IProjectSceneConfiguration)
public:
    virtual ~IProjectSceneConfiguration() = default;

    virtual bool isVerticalRulersVisible() const = 0;
    virtual void setVerticalRulersVisible(bool visible) = 0;
    virtual muse::async::Channel<bool> isVerticalRulersVisibleChanged() const = 0;

    virtual double zoom() const = 0;

    virtual int mouseZoomPrecision() const = 0;
    virtual void setMouseZoomPrecision(int precision) = 0;
};
}
