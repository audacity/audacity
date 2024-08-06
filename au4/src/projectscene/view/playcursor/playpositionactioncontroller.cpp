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

using namespace au::projectscene;
using namespace muse::actions;

static const ActionCode PLAY_POSITION_DECREASE("play-position-decrease");
static const ActionCode PLAY_POSITION_INCREASE("play-position-increase");

au::projectscene::PlayPositionActionController::PlayPositionActionController(QObject* parent)
    : QObject(parent)
{
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

void PlayPositionActionController::init()
{
    dispatcher()->reg(this, PLAY_POSITION_DECREASE, this, &PlayPositionActionController::playPositionDecrease);
    dispatcher()->reg(this, PLAY_POSITION_INCREASE, this, &PlayPositionActionController::playPositionIncrease);
}

void PlayPositionActionController::applySingleStep(Direction direction)
{
    IProjectViewStatePtr viewState = globalContext()->currentProject()->viewState();
    bool snapEnabled = viewState->isSnapEnabled().val;

    audio::secs_t currentPlaybackPosition = globalContext()->playbackState()->playbackPosition();
    double newXPosition;
    if (direction == Direction::Left) {
        newXPosition = m_context->timeToPosition(currentPlaybackPosition) - 1;
    } else {
        newXPosition = m_context->timeToPosition(currentPlaybackPosition) + 1;
    }

    //! TODO AU4 implement snapping behaviour
    Q_UNUSED(snapEnabled);
    audio::secs_t secs = m_context->positionToTime(newXPosition /*, snapEnabled*/);

    if (muse::RealIsEqualOrMore(secs, 0.0)) {
        dispatcher()->dispatch("playback_seek", ActionData::make_arg1<double>(secs));
    }
}
