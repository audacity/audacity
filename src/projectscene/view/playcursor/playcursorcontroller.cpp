/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * A Digital Audio Editor
 *
 * Copyright (C) 2024 Audacity BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#include "playcursorcontroller.h"

#include <algorithm>
#include <cmath>

#include "global/realfn.h"

using namespace au::projectscene;
using namespace muse::actions;

static const ActionQuery PLAYBACK_SEEK_QUERY("action://playback/seek");
static const ActionQuery PLAYBACK_CHANGE_PLAY_REGION_QUERY("action://playback/play-region-change");

static constexpr int SCROLL_SUPPRESSION_TIMEOUT_MS = 3000;
static constexpr double ANIMATION_DURATION_SEC = TimelineContext::ANIMATION_DURATION_MS / 1000.0;
static constexpr double SEEK_GESTURE_DRAG_THRESHOLD_PX = 5.0;

PlayCursorController::PlayCursorController(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
    m_scrollSuppressionTimer.setSingleShot(true);
    m_scrollSuppressionTimer.setInterval(SCROLL_SUPPRESSION_TIMEOUT_MS);
    connect(&m_scrollSuppressionTimer, &QTimer::timeout, this, &PlayCursorController::onScrollSuppressionTimeout);
}

void PlayCursorController::init()
{
    playbackState()->playbackPositionChanged().onReceive(this, [this](muse::secs_t secs) {
        updatePositionX(secs);
    });

    playbackState()->playbackStatusChanged().onReceive(this, [this](playback::PlaybackStatus status) {
        if (status == playback::PlaybackStatus::Running) {
            clearScrollSuppression(); // so that the cursor is immediately updated when playback starts
        }
    });
}

void PlayCursorController::seekToTime(double secs, bool triggerPlay)
{
    if (playbackState()->isPlaying() && !triggerPlay) {
        //! NOTE: Ignore all seeks in play mode unless it is an activation of play or resume from a new position
        return;
    }

    secs = snapTime(secs);

    muse::actions::ActionQuery q(PLAYBACK_SEEK_QUERY);
    q.addParam("triggerPlay", muse::Val(triggerPlay));
    if (muse::RealIsEqualOrMore(secs, 0.0)) {
        q.addParam("seekTime", muse::Val(secs));
        dispatcher()->dispatch(q);
    } else if (!muse::RealIsEqual(playbackState()->playbackPosition(), 0.0)) {
        q.addParam("seekTime", muse::Val(0.0));
        dispatcher()->dispatch(q);
    }
}

void PlayCursorController::animatedSeekToTime(double secs)
{
    if (!playbackState()->isPlaying()) {
        m_seekAnimated = true;
    }

    seekToTime(secs);
}

void PlayCursorController::beginSeekGesture(double time, double x, double y)
{
    m_seekGestureActive = true;
    m_seekGestureDragged = false;
    m_seekGestureTime = time;
    m_seekGesturePressPos = QPointF(x, y);
}

void PlayCursorController::updateSeekGesture(double x, double y)
{
    if (!m_seekGestureActive || m_seekGestureDragged) {
        return;
    }

    if (std::abs(x - m_seekGesturePressPos.x()) >= SEEK_GESTURE_DRAG_THRESHOLD_PX
        || std::abs(y - m_seekGesturePressPos.y()) >= SEEK_GESTURE_DRAG_THRESHOLD_PX) {
        m_seekGestureDragged = true;
    }
}

bool PlayCursorController::endSeekGesture()
{
    if (!m_seekGestureActive) {
        return false;
    }

    m_seekGestureActive = false;

    if (m_seekGestureDragged) {
        return false;
    }

    if (playbackState()->isPlaying()) {
        //! NOTE: while playing the playhead is not moved, but the click position
        //! is remembered so that stopping returns there. Snap it here: seekToTime
        //! bails out before snapping while playing, so it would be stored raw and
        //! stopping would land off the boundary a normal click snaps to.
        playbackController()->setLastPlaybackSeekTime(std::max(0.0, snapTime(m_seekGestureTime)));
    }
    seekToTime(m_seekGestureTime);

    return true;
}

void PlayCursorController::cancelSeekGesture()
{
    m_seekGestureActive = false;
    m_seekGestureDragged = false;
}

void PlayCursorController::setPlaybackRegionByTime(double t1, double t2)
{
    const double start = std::max(0.0, t1);
    const double end = std::max(0.0, t2);

    muse::actions::ActionQuery q(PLAYBACK_CHANGE_PLAY_REGION_QUERY);
    q.addParam("start", muse::Val(start));
    q.addParam("end", muse::Val(end));
    dispatcher()->dispatch(q);
}

double PlayCursorController::snapTime(double time) const
{
    return m_context ? m_context->applyDetectedSnap(time) : time;
}

au::context::IPlaybackStatePtr PlayCursorController::playbackState() const
{
    return globalContext()->playbackState();
}

void PlayCursorController::updatePositionX(muse::secs_t secs)
{
    const bool seekAnimated = std::exchange(m_seekAnimated, false);

    if (m_positionX == m_context->timeToPosition(secs)) {
        return;
    }

    const bool isPlayingOrRecording = playbackState()->isPlaying() || globalContext()->isRecording();

    if (isPlayingOrRecording) {
        const bool updateDisplayWhilePlaying = m_context->updateDisplayWhilePlayingEnabled();
        const bool pinnedPlayHead = m_context->pinnedPlayHeadEnabled();

        if (updateDisplayWhilePlaying && !m_viewUpdatesSuppressed && !m_context->isAnimating()) {
            const double halfFrameDuration = (m_context->frameEndTime() - m_context->frameStartTime()) * 0.5;
            const bool zoomTooClose = halfFrameDuration < ANIMATION_DURATION_SEC;

            if (pinnedPlayHead) {
                if (secs < m_context->frameStartTime() || secs > m_context->frameEndTime()) {
                    if (zoomTooClose) {
                        ensureCursorAtCenter(secs);
                    } else {
                        double predictedSecs = secs + ANIMATION_DURATION_SEC;
                        m_context->animatedCenterOnTime(predictedSecs);
                    }
                } else {
                    ensureCursorAtCenter(secs);
                }
            } else {
                double predictedSecs = secs + ANIMATION_DURATION_SEC;
                if (predictedSecs < m_context->frameStartTime() || predictedSecs > m_context->frameEndTime()) {
                    if (zoomTooClose) {
                        m_context->insureVisible(secs);
                    } else {
                        m_context->animatedInsureVisible(predictedSecs);
                    }
                }
            }
        }
    } else {
        const double halfFrameDuration = (m_context->frameEndTime() - m_context->frameStartTime()) * 0.5;
        const bool zoomTooClose = halfFrameDuration < ANIMATION_DURATION_SEC;

        if (seekAnimated && !zoomTooClose) {
            m_context->animatedInsureVisible(secs);
        } else {
            m_context->insureVisible(secs);
        }
    }

    m_positionX = m_context->timeToPosition(secs);
    emit positionXChanged();
}

void PlayCursorController::onFrameTimeChanged()
{
    double newPosition = m_context->timeToPosition(playbackState()->playbackPosition());

    if (m_positionX != newPosition) {
        m_positionX = newPosition;
        emit positionXChanged();
    }
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

    if (m_context) {
        disconnect(m_context, nullptr, this, nullptr);
    }

    m_context = newContext;

    if (m_context) {
        connect(m_context, &TimelineContext::frameTimeChanged, this, &PlayCursorController::onFrameTimeChanged);
        connect(m_context, &TimelineContext::userHorizontalScrolled, this, &PlayCursorController::onUserHorizontalScroll);
        connect(m_context, &TimelineContext::updateDisplayWhilePlayingEnabledChanged, this, [this]() {
            if (m_context->updateDisplayWhilePlayingEnabled()) {
                clearScrollSuppression();
            }
        });
    }

    emit timelineContextChanged();
}

void PlayCursorController::ensureCursorAtCenter(muse::secs_t secs) const
{
    m_context->centerOnTime(secs);
}

void PlayCursorController::onUserHorizontalScroll()
{
    const bool isPlayingOrRecording = playbackState()->isPlaying() || globalContext()->isRecording();
    if (!isPlayingOrRecording || !m_context->updateDisplayWhilePlayingEnabled()) {
        return;
    }

    m_viewUpdatesSuppressed = true;
    m_scrollSuppressionTimer.start();
}

void PlayCursorController::onScrollSuppressionTimeout()
{
    m_viewUpdatesSuppressed = false;
}

void PlayCursorController::clearScrollSuppression()
{
    m_scrollSuppressionTimer.stop();
    m_viewUpdatesSuppressed = false;
}
