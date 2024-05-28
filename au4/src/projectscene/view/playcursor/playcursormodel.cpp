#include "playcursormodel.h"

using namespace au::projectscene;

PlayCursorModel::PlayCursorModel(QObject* parent)
    : QObject(parent)
{
}

void PlayCursorModel::init()
{
    playbackController()->playbackPositionChanged().onNotify(this, [this]() {
        emit positionXChanged();
    });
}

double PlayCursorModel::positionX() const
{
    if (!m_context) {
        return 0;
    }
    return m_context->timeToPosition(playbackController()->playbackPositionInSeconds());
}

TimelineContext* PlayCursorModel::timelineContext() const
{
    return m_context;
}

void PlayCursorModel::setTimelineContext(TimelineContext* newContext)
{
    if (m_context == newContext) {
        return;
    }
    m_context = newContext;
    emit timelineContextChanged();
}
