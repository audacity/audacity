#include "playcursormodel.h"

using namespace au::projectscene;
using namespace muse::actions;

PlayCursorModel::PlayCursorModel(QObject* parent)
    : QObject(parent)
{
}

au::context::IPlaybackStatePtr PlayCursorModel::playbackState() const
{
    return globalContext()->playbackState();
}

void PlayCursorModel::init()
{
    playbackState()->playbackPositionChanged().onReceive(this, [this](audio::secs_t secs) {
        updatePositionX(secs);
    });
}

void PlayCursorModel::seekToX(double x)
{
    double secs = m_context->positionToTime(x);
    dispatcher()->dispatch("playback_seek", ActionData::make_arg1<double>(secs));
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
