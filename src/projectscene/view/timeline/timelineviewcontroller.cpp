/*
* Audacity: A Digital Audio Editor
*/
#include "timelineviewcontroller.h"

#include "timelinecontext.h"

using namespace au::projectscene;

TimelineViewController::TimelineViewController(TimelineContext* context, const muse::modularity::ContextPtr& ctx)
    : muse::Contextable(ctx), m_context(context)
{
}

void TimelineViewController::init()
{
    projectSceneActionsController()->setTimelineViewController(this);
}

void TimelineViewController::deinit()
{
    if (projectSceneActionsController()->timelineViewController() == this) {
        projectSceneActionsController()->setTimelineViewController(nullptr);
    }
}

void TimelineViewController::zoomIn()
{
    m_context->zoomIn();
}

void TimelineViewController::zoomOut()
{
    m_context->zoomOut();
}

void TimelineViewController::zoomDefault()
{
    m_context->zoomDefault();
}

void TimelineViewController::zoomToggle()
{
    m_context->zoomToggle();
}

void TimelineViewController::fitSelectionToWidth()
{
    m_context->fitSelectionToWidth();
}

void TimelineViewController::fitProjectToWidth()
{
    m_context->fitProjectToWidth();
}

void TimelineViewController::centerViewOnPlayhead(bool onlyIfPlayheadNotVisible)
{
    m_context->centerViewOnPlayhead(onlyIfPlayheadNotVisible);
}

void TimelineViewController::requestContextMenu()
{
    m_context->requestContextMenu();
}
