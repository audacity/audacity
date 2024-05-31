#include "playcursorcontroller.h"

using namespace au::projectscene;
using namespace muse::actions;

PlayCursorController::PlayCursorController(QObject* parent)
    : QObject(parent)
{
}

au::context::IPlaybackStatePtr PlayCursorController::playbackState() const
{
    return globalContext()->playbackState();
}

void PlayCursorController::init()
{
    playbackState()->playbackPositionChanged().onReceive(this, [this](audio::secs_t secs) {
        updatePositionX(secs);
    });
}

void PlayCursorController::seekToX(double x)
{
    double secs = m_context->positionToTime(x);
    dispatcher()->dispatch("playback_seek", ActionData::make_arg1<double>(secs));
}

void PlayCursorController::updatePositionX(audio::secs_t secs)
{
    m_positionX = m_context->timeToPosition(secs);
    emit positionXChanged();
}

double PlayCursorController::positionX() const
{
    return m_positionX;
}

TimelineContext* PlayCursorController::timelineContext() const
{
    return m_context;
}

void PlayCursorController::setTimelineContext(TimelineContext* newContext)
{
    if (m_context == newContext) {
        return;
    }
    m_context = newContext;
    emit timelineContextChanged();
}
