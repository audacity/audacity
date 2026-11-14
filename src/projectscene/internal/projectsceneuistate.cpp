/*
* Audacity: A Digital Audio Editor
*/
#include "projectsceneuistate.h"

using namespace au::projectscene;

static const QString TIMELINE_RULER_MODE_KEY("projectscene/timelineRulerMode");

void ProjectSceneUiState::init()
{
    uiState()->uiItemStateChanged(TIMELINE_RULER_MODE_KEY).onNotify(this, [this]() {
        m_timelineRulerModeChanged.notify();
    });
}

TimelineRulerMode ProjectSceneUiState::timelineRulerMode() const
{
    return uiState()->uiItemState(TIMELINE_RULER_MODE_KEY) == "1"
           ? TimelineRulerMode::BEATS_AND_MEASURES
           : TimelineRulerMode::MINUTES_AND_SECONDS;
}

void ProjectSceneUiState::setTimelineRulerMode(TimelineRulerMode mode)
{
    const QString value = mode == TimelineRulerMode::BEATS_AND_MEASURES ? "1" : "0";
    if (uiState()->uiItemState(TIMELINE_RULER_MODE_KEY) == value) {
        return;
    }
    uiState()->setUiItemState(TIMELINE_RULER_MODE_KEY, value);
}

muse::async::Notification ProjectSceneUiState::timelineRulerModeChanged() const
{
    return m_timelineRulerModeChanged;
}
