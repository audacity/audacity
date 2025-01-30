#pragma once

#include "async/channel.h"
#include "modularity/imoduleinterface.h"
#include "types/projectscenetypes.h"
#include "types/retval.h"

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

    virtual trackedit::secs_t insertSilenceDuration() const = 0;
    virtual void setInsertSilenceDuration(const trackedit::secs_t duration) = 0;

    virtual std::string insertSilenceDurationFormat() const = 0;
    virtual void setInsertSilenceDurationFormat(const std::string& format) = 0;

    virtual int mouseZoomPrecision() const = 0;
    virtual void setMouseZoomPrecision(int precision) = 0;
    virtual TimelineRulerMode timelineRulerMode() const = 0;
    virtual void setTimelineRulerMode(const TimelineRulerMode mode) = 0;
    virtual muse::async::Channel<TimelineRulerMode> timelineRulerModeChanged() const = 0;

    virtual muse::ValCh<bool> isEffectsPanelVisible() const = 0;
    virtual void setIsEffectsPanelVisible(bool visible) = 0;

    virtual const std::vector<std::pair<std::string, std::string> >& clipColors() const = 0;
};
}
