/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../iprojectsceneconfiguration.h"

#include "modularity/ioc.h"
#include "ui/iuiconfiguration.h"

namespace au::projectscene {
class ProjectSceneConfiguration : public IProjectSceneConfiguration
{
public:
    muse::Inject<muse::ui::IUiConfiguration> uiConfiguration;

public:
    ProjectSceneConfiguration() = default;

    void init();

    bool isVerticalRulersVisible() const override;
    void setVerticalRulersVisible(bool visible) override;
    muse::async::Channel<bool> isVerticalRulersVisibleChanged() const override;

    double projectZoom() const override;

    int mouseZoomPrecision() const override;
    void setMouseZoomPrecision(int precision) override;
    virtual TimelineRulerMode timelineRulerMode() const override;
    virtual void setTimelineRulerMode(const TimelineRulerMode mode) override;
    virtual muse::async::Channel<TimelineRulerMode> timelineRulerModeChanged() const override;

private:
    muse::async::Channel<bool> m_isVerticalRulersVisibleChanged;
    muse::async::Channel<TimelineRulerMode> m_timelineRulerModeChanged;
};
}
