/*
* Audacity: A Digital Audio Editor
*/

#include "playregioncontroller.h"

#include <cmath>

#include "log.h"

namespace au::projectscene {
PlayRegionController::PlayRegionController(QObject* parent)
    : QObject(parent)
{
}

void PlayRegionController::mouseDown(double pos)
{
    m_lastPos = pos;
    m_dragStartPos = pos;
    m_dragStarted = false;

    if (playback()->player()->isLoopRegionClear()) {
        m_action = UserInputAction::CreateRegion;
        QGuiApplication::setOverrideCursor(QCursor(Qt::SizeHorCursor));
    } else if (std::abs(startTimePos() - pos) < RESIZE_AREA_WIDTH_PX) {
        m_action = UserInputAction::ResizeStart;
        QGuiApplication::setOverrideCursor(QCursor(Qt::SizeHorCursor));
    } else if (std::abs(endTimePos() - pos) < RESIZE_AREA_WIDTH_PX) {
        m_action = UserInputAction::ResizeEnd;
        QGuiApplication::setOverrideCursor(QCursor(Qt::SizeHorCursor));
    } else if (pos > startTimePos() && pos < endTimePos()) {
        m_action = UserInputAction::Drag;
        QGuiApplication::setOverrideCursor(QCursor(Qt::ClosedHandCursor));
    } else {
        m_action = UserInputAction::CreateRegion;
        QGuiApplication::setOverrideCursor(QCursor(Qt::SizeHorCursor));
    }

    if (m_action != UserInputAction::None) {
        playback()->player()->loopEditingBegin();
    }
}

void PlayRegionController::mouseMove(double pos)
{
    if (m_action == UserInputAction::None) {
        return;
    }

    if (!m_dragStarted) {
        if (std::abs(pos - m_dragStartPos) <= MINIMUM_DRAG_LENGTH_PX) {
            return;
        }
        m_dragStarted = true;
    }

    auto player = playback()->player();
    player->setLoopRegionActive(true);

    double visibleStartPos = context()->timeToPosition(context()->frameStartTime());
    double visibleEndPos = context()->timeToPosition(context()->frameEndTime());

    pos = std::clamp(pos, visibleStartPos, visibleEndPos);

    switch (m_action) {
    case UserInputAction::CreateRegion:
        // Have to clear the loop if we are replacing existing
        player->clearLoopRegion();
        // Clearing the loop automatically disables it, reactivating
        player->setLoopRegionActive(true);
        setStartTime(m_dragStartPos);
        setEndTime(pos);
        m_action = UserInputAction::ResizeEnd;
        break;
    case UserInputAction::ResizeStart:
        setStartTime(pos);
        break;
    case UserInputAction::ResizeEnd:
        setEndTime(pos);
        break;
    case UserInputAction::Drag: {
        auto pr = player->loopRegion();
        double deltaTime = context()->positionToTime(pos) - context()->positionToTime(m_lastPos);
        player->setLoopRegion({ pr.start + deltaTime, pr.end + deltaTime });
        break;
    }
    case UserInputAction::None:
        break;
    }

    m_lastPos = pos;
}

void PlayRegionController::mouseUp(double pos)
{
    UNUSED(pos);

    auto player = playback()->player();
    auto region = player->loopRegion();

    if (region.end < region.start) {
        player->setLoopRegion({ region.end, region.start });
    }

    // Toggle region active if it was a simple click without drag
    if (m_action == UserInputAction::Drag && !m_dragStarted) {
        player->setLoopRegionActive(!player->isLoopRegionActive());
    }

    QGuiApplication::restoreOverrideCursor();
    player->loopEditingEnd();
    m_action = UserInputAction::None;
}

TimelineContext* PlayRegionController::context() const
{
    return m_context;
}

double PlayRegionController::startTimePos() const
{
    return context()->timeToPosition(playback()->player()->loopRegion().start);
}

double PlayRegionController::endTimePos() const
{
    return context()->timeToPosition(playback()->player()->loopRegion().end);
}

void PlayRegionController::setStartTime(double pos)
{
    playback()->player()->setLoopRegionStart(context()->positionToTime(pos));
}

void PlayRegionController::setEndTime(double pos)
{
    playback()->player()->setLoopRegionEnd(context()->positionToTime(pos));
}

void PlayRegionController::beginPreview()
{
    m_initialRegion = playback()->player()->loopRegion();
    m_initialState = playback()->player()->isLoopRegionActive();
    playback()->player()->setLoopRegionActive(true);
}

void PlayRegionController::setPreviewStartTime(double time)
{
    playback()->player()->setLoopRegionStart(time);
}

void PlayRegionController::setPreviewEndTime(double time)
{
    playback()->player()->setLoopRegionEnd(time);
}

void PlayRegionController::endPreview()
{
    playback()->player()->setLoopRegion(m_initialRegion);
    playback()->player()->setLoopRegionActive(m_initialState);
}

void PlayRegionController::setContext(TimelineContext* newContext)
{
    m_context = newContext;
}
}
