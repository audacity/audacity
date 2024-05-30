#include "playcursormodel.h"

using namespace au::projectscene;

PlayCursorModel::PlayCursorModel(QObject* parent)
    : QObject(parent)
{
}

au::playback::IPlayerPtr PlayCursorModel::player() const
{
    return globalContext()->player();
}

void PlayCursorModel::init()
{
    player()->playbackPositionChanged().onReceive(this, [this](audio::secs_t secs) {
        updatePositionX(secs);
    });
}

void PlayCursorModel::updatePositionX(audio::secs_t secs)
{
    m_positionX = m_context->timeToPosition(secs);
    emit positionXChanged();
}

double PlayCursorModel::positionX() const
{
    return m_positionX;
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
