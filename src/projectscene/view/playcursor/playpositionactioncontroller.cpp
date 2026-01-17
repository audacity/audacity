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

#include "playpositionactioncontroller.h"

#include "global/realfn.h"

using namespace au::projectscene;
using namespace muse::actions;

static const ActionCode PLAY_POSITION_DECREASE("play-position-decrease");
static const ActionCode PLAY_POSITION_INCREASE("play-position-increase");
static const ActionQuery PLAYBACK_SEEK_QUERY("action://playback/seek");

au::projectscene::PlayPositionActionController::PlayPositionActionController(QObject* parent)
    : QObject(parent), muse::Injectable(muse::iocCtxForQmlObject(this))
{
}

void PlayPositionActionController::init()
{
    dispatcher()->reg(this, PLAY_POSITION_DECREASE, this, &PlayPositionActionController::playPositionDecrease);
    dispatcher()->reg(this, PLAY_POSITION_INCREASE, this, &PlayPositionActionController::playPositionIncrease);

    globalContext()->currentProjectChanged().onNotify(this, [this](){
        onProjectChanged();
    });

    onProjectChanged();
}

TimelineContext* PlayPositionActionController::timelineContext() const
{
    return m_context;
}

void PlayPositionActionController::setTimelineContext(TimelineContext* newContext)
{
    if (m_context == newContext) {
        return;
    }

    m_context = newContext;
    emit timelineContextChanged();
}

void PlayPositionActionController::playPositionDecrease()
{
    applySingleStep(Direction::Left);
}

void PlayPositionActionController::playPositionIncrease()
{
    applySingleStep(Direction::Right);
}

void PlayPositionActionController::snapCurrentPosition()
{
    const muse::secs_t currentPlaybackPosition = playbackState()->playbackPosition();
    const double currentXPosition = m_context->timeToPosition(currentPlaybackPosition);
    const muse::secs_t secs = m_context->positionToTime(currentXPosition, true);
    if (muse::RealIsEqualOrMore(secs, 0.0) || !muse::RealIsEqual(secs, currentPlaybackPosition)) {
        muse::actions::ActionQuery q(PLAYBACK_SEEK_QUERY);
        q.addParam("seekTime", muse::Val(secs));
        q.addParam("triggerPlay", muse::Val(false));
        dispatcher()->dispatch(q);
    }
}

void PlayPositionActionController::applySingleStep(Direction direction)
{
    IProjectViewStatePtr viewState = globalContext()->currentProject()->viewState();
    bool snapEnabled = viewState->isSnapEnabled();

    const muse::secs_t currentPlaybackPosition = playbackState()->playbackPosition();
    muse::secs_t secs = 0;

    if (snapEnabled) {
        const double currentXPosition = m_context->timeToPosition(currentPlaybackPosition);
        secs = m_context->singleStepToTime(currentXPosition, direction, viewState->snap().val);
    } else {
        double newXPosition = 0.0;
        if (direction == Direction::Left) {
            newXPosition = m_context->timeToPosition(currentPlaybackPosition) - 1;
        } else {
            newXPosition = m_context->timeToPosition(currentPlaybackPosition) + 1;
        }

        secs = m_context->positionToTime(newXPosition);
    }

    if (muse::RealIsEqualOrMore(secs, 0.0)) {
        muse::actions::ActionQuery q(PLAYBACK_SEEK_QUERY);
        q.addParam("seekTime", muse::Val(secs));
        q.addParam("triggerPlay", muse::Val(false));
        dispatcher()->dispatch(q);
    }
}

void PlayPositionActionController::onProjectChanged()
{
    auto currentProject = globalContext()->currentProject();
    if (!currentProject) {
        return;
    }

    IProjectViewStatePtr viewState = currentProject->viewState();

    viewState->snap().ch.onReceive(this, [this](const Snap& snap){
        if (snap.enabled) {
            snapCurrentPosition();
        }
    });
}

au::context::IPlaybackStatePtr PlayPositionActionController::playbackState() const
{
    return globalContext()->playbackState();
}
