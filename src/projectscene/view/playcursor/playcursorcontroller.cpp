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

#include "global/realfn.h"

using namespace au::projectscene;
using namespace muse::actions;

static const ActionQuery PLAYBACK_SEEK_QUERY("action://playback/seek");
static const ActionQuery PLAYBACK_CHANGE_PLAY_REGION_QUERY("action://playback/play-region-change");

static constexpr int SCROLL_SUPPRESSION_TIMEOUT_MS = 3000;

PlayCursorController::PlayCursorController(QObject* parent)
    : QObject(parent), muse::Injectable(muse::iocCtxForQmlObject(this))
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

    globalContext()->recordPositionChanged().onReceive(this, [this](muse::secs_t secs){
        updatePositionX(secs);
    });

    playbackState()->playbackStatusChanged().onReceive(this, [this](playback::PlaybackStatus status) {
        if (status == playback::PlaybackStatus::Running) {
            clearScrollSuppression(); // so that the cursor is immediately updated when playback starts
        }
    });
}

void PlayCursorController::seekToX(double x, bool triggerPlay)
{
    if (playbackState()->isPlaying() && !triggerPlay) {
        //! NOTE: Ignore all seeks in play mode unless it is an activation of play or resume from a new position
        return;
    }

    const IProjectViewStatePtr viewState = projectViewState();
    const bool snapEnabled = viewState ? viewState->isSnapEnabled() : false;

    const double secs = m_context->positionToTime(x, snapEnabled);
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

void PlayCursorController::setPlaybackRegion(double x1, double x2)
{
    const IProjectViewStatePtr viewState = projectViewState();
    const bool snapEnabled = viewState ? viewState->isSnapEnabled() : false;

    const double start = std::max(0.0, m_context->positionToTime(x1, snapEnabled));
    const double end = std::max(0.0, m_context->positionToTime(x2, snapEnabled));

    muse::actions::ActionQuery q(PLAYBACK_CHANGE_PLAY_REGION_QUERY);
    q.addParam("start", muse::Val(start));
    q.addParam("end", muse::Val(end));
    dispatcher()->dispatch(q);
}

au::context::IPlaybackStatePtr PlayCursorController::playbackState() const
{
    return globalContext()->playbackState();
}

IProjectViewStatePtr PlayCursorController::projectViewState() const
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    return project ? project->viewState() : nullptr;
}

void PlayCursorController::updatePositionX(muse::secs_t secs)
{
    if (m_positionX == m_context->timeToPosition(secs)) {
        return;
    }

    const bool isPlayingOrRecording = playbackState()->isPlaying() || globalContext()->isRecording();

    if (isPlayingOrRecording) {
        const bool updateDisplayWhilePlaying = m_context->updateDisplayWhilePlayingEnabled();
        const bool pinnedPlayHead = m_context->pinnedPlayHeadEnabled();

        if (updateDisplayWhilePlaying && !m_viewUpdatesSuppressed) {
            if (pinnedPlayHead) {
                ensureCursorAtCenter(secs);
            } else {
                m_context->insureVisible(secs);
            }
        }
    }

    m_positionX = m_context->timeToPosition(secs);
    emit positionXChanged();
}

void PlayCursorController::onFrameTimeChanged()
{
    double newPosition;
    if (globalContext()->isRecording()) {
        newPosition = m_context->timeToPosition(globalContext()->recordPosition());
    } else {
        newPosition = m_context->timeToPosition(playbackState()->playbackPosition());
    }

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
