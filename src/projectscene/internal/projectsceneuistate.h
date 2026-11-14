/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../iprojectsceneuistate.h"

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"
#include "framework/ui/iuistate.h"

namespace au::projectscene {
class ProjectSceneUiState : public IProjectSceneUiState, public muse::Contextable, public muse::async::Asyncable
{
    muse::ContextInject<muse::ui::IUiState> uiState { this };

public:
    ProjectSceneUiState(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init();

    TimelineRulerMode timelineRulerMode() const override;
    void setTimelineRulerMode(TimelineRulerMode mode) override;
    muse::async::Notification timelineRulerModeChanged() const override;

private:
    muse::async::Notification m_timelineRulerModeChanged;
};
}
