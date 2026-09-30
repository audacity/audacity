/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"

#include "projectscene/iprojectsceneactionscontroller.h"
#include "projectscene/itimelineviewcontroller.h"

namespace au::projectscene {
class TimelineContext;
class TimelineViewController : public ITimelineViewController, public muse::Contextable
{
    muse::ContextInject<IProjectSceneActionsController> projectSceneActionsController{ this };

public:
    TimelineViewController(TimelineContext* context, const muse::modularity::ContextPtr& ctx);

    void init();
    void deinit();

    void zoomIn() override;
    void zoomOut() override;
    void zoomDefault() override;
    void zoomToggle() override;

    void fitSelectionToWidth() override;
    void fitProjectToWidth() override;

    void centerViewOnPlayhead(bool onlyIfPlayheadNotVisible) override;

private:
    TimelineContext* m_context = nullptr;
};
}
