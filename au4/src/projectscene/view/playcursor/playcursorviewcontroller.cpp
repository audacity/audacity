#include "playcursorviewcontroller.h"

using namespace au::projectscene;
using namespace muse::actions;

//! TODO Make better name
constexpr double INSURE_VISIBLE_GAP_PX(16);
constexpr double AUTO_SHIFT_PERCENT(0.75);

PlayCursorViewController::PlayCursorViewController(QObject* parent)
    : QObject(parent)
{
}

au::context::IPlaybackStatePtr PlayCursorViewController::playbackState() const
{
    return globalContext()->playbackState();
}

void PlayCursorViewController::init()
{
    playbackState()->playbackPositionChanged().onReceive(this, [this](audio::secs_t secs) {
        updatePositionX(secs);
    });
}

void PlayCursorViewController::seekToX(double x)
{
    double secs = m_context->positionToTime(x);
    dispatcher()->dispatch("playback_seek", ActionData::make_arg1<double>(secs));
}

void PlayCursorViewController::insureVisible(audio::secs_t pos)
{
    // move to play cursor position
    if (pos < m_context->frameStartTime()) {
        m_context->moveToFrameTime(pos);
    } else {
        // auto shift
        double endGapPx = m_context->zoom() * (m_context->frameEndTime() - pos);
        if (endGapPx < INSURE_VISIBLE_GAP_PX) {
            double frameTime = m_context->frameEndTime() - m_context->frameStartTime();
            m_context->shiftFrameTime(frameTime * AUTO_SHIFT_PERCENT);
        }
    }
}

void PlayCursorViewController::updatePositionX(audio::secs_t secs)
{
    insureVisible(secs);

    m_positionX = m_context->timeToPosition(secs);
    emit positionXChanged();

    playCursorController()->setTimePosition(m_context->positionToTime(m_positionX));
}

void PlayCursorViewController::onFrameTimeChanged()
{
    m_positionX = m_context->timeToPosition(playbackState()->playbackPosition());
    emit positionXChanged();

    playCursorController()->setTimePosition(playbackState()->playbackPosition());
}

double PlayCursorViewController::positionX() const
{
    return m_positionX;
}

double PlayCursorViewController::currentTime() const
{
    return m_context->positionToTime(m_positionX);
}

TimelineContext* PlayCursorViewController::timelineContext() const
{
    return m_context;
}

void PlayCursorViewController::setTimelineContext(TimelineContext* newContext)
{
    if (m_context == newContext) {
        return;
    }

    if (m_context) {
        disconnect(m_context, nullptr, this, nullptr);
    }

    m_context = newContext;

    if (m_context) {
        connect(m_context, &TimelineContext::frameTimeChanged, this, &PlayCursorViewController::onFrameTimeChanged);
    }

    emit timelineContextChanged();
}
