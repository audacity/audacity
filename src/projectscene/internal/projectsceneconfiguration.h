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

    trackedit::secs_t insertSilenceDuration() const override;
    void setInsertSilenceDuration(const trackedit::secs_t duration) override;

    std::string insertSilenceDurationFormat() const override;
    void setInsertSilenceDurationFormat(const std::string& format) override;

    double zoom() const override;

    int mouseZoomPrecision() const override;
    void setMouseZoomPrecision(int precision) override;
    virtual TimelineRulerMode timelineRulerMode() const override;
    virtual void setTimelineRulerMode(const TimelineRulerMode mode) override;
    virtual muse::async::Channel<TimelineRulerMode> timelineRulerModeChanged() const override;

    muse::ValCh<bool> isEffectsPanelVisible() const override;
    void setIsEffectsPanelVisible(bool visible) override;

    const std::vector<std::pair<std::string, std::string> >& clipColors() const override;

private:
    muse::async::Channel<bool> m_isVerticalRulersVisibleChanged;
    muse::async::Channel<TimelineRulerMode> m_timelineRulerModeChanged;

    muse::ValCh<bool> m_effectsPanelVisible;
};
}
