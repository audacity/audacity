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
static const ActionCode SEL_EXT_LEFT("sel-ext-left");
static const ActionCode SEL_EXT_RIGHT("sel-ext-right");
static const ActionCode SEL_CNTR_LEFT("sel-cntr-left");
static const ActionCode SEL_CNTR_RIGHT("sel-cntr-right");
static const ActionQuery PLAYBACK_SEEK_QUERY("action://playback/seek");

au::projectscene::PlayPositionActionController::PlayPositionActionController(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void PlayPositionActionController::init()
{
    dispatcher()->reg(this, PLAY_POSITION_DECREASE, this, &PlayPositionActionController::playPositionDecrease);
    dispatcher()->reg(this, PLAY_POSITION_INCREASE, this, &PlayPositionActionController::playPositionIncrease);
    dispatcher()->reg(this, SEL_EXT_LEFT, this, &PlayPositionActionController::selectionExtendLeft);
    dispatcher()->reg(this, SEL_EXT_RIGHT, this, &PlayPositionActionController::selectionExtendRight);
    dispatcher()->reg(this, SEL_CNTR_LEFT, this, &PlayPositionActionController::selectionContractLeft);
    dispatcher()->reg(this, SEL_CNTR_RIGHT, this, &PlayPositionActionController::selectionContractRight);

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
    const muse::secs_t currentPlaybackPosition = playbackState()->playbackPosition();
    const muse::secs_t secs = stepFromTime(currentPlaybackPosition, direction);

    if (muse::RealIsEqualOrMore(secs, 0.0)) {
        muse::actions::ActionQuery q(PLAYBACK_SEEK_QUERY);
        q.addParam("seekTime", muse::Val(secs));
        q.addParam("triggerPlay", muse::Val(false));
        dispatcher()->dispatch(q);

        m_context->animatedInsureVisible(secs);
    }
}

muse::secs_t PlayPositionActionController::stepFromTime(muse::secs_t from, Direction direction) const
{
    auto currentProject = globalContext()->currentProject();
    if (!currentProject) {
        return from;
    }

    IProjectViewStatePtr viewState = currentProject->viewState();
    const bool snapEnabled = viewState->isSnapEnabled();

    if (snapEnabled) {
        const double currentXPosition = m_context->timeToPosition(from);
        return m_context->singleStepToTime(currentXPosition, direction, viewState->snap().val);
    }

    const double newXPosition = m_context->timeToPosition(from) + (direction == Direction::Left ? -1 : 1);
    return m_context->positionToTime(newXPosition);
}

void PlayPositionActionController::selectionExtendLeft()
{
    if (selectionController()->timeSelectionIsEmpty()) {
        selectionController()->initSelectionAtPlayhead();
    }
    const muse::secs_t from = selectionController()->dataSelectedStartTime();
    const muse::secs_t newStart = stepFromTime(from, Direction::Left);
    if (muse::RealIsEqualOrMore(newStart, 0.0)) {
        selectionController()->setDataSelectedStartTime(newStart, true);
    }
}

void PlayPositionActionController::selectionExtendRight()
{
    if (selectionController()->timeSelectionIsEmpty()) {
        selectionController()->initSelectionAtPlayhead();
    }
    const muse::secs_t from = selectionController()->dataSelectedEndTime();
    selectionController()->setDataSelectedEndTime(stepFromTime(from, Direction::Right), true);
}

void PlayPositionActionController::selectionContractLeft()
{
    if (selectionController()->timeSelectionIsEmpty()) {
        return;
    }
    const muse::secs_t end = selectionController()->dataSelectedEndTime();
    muse::secs_t newStart = stepFromTime(selectionController()->dataSelectedStartTime(), Direction::Right);
    if (muse::RealIsEqualOrMore(newStart, end)) {
        newStart = end;
    }
    selectionController()->setDataSelectedStartTime(newStart, true);
}

void PlayPositionActionController::selectionContractRight()
{
    if (selectionController()->timeSelectionIsEmpty()) {
        return;
    }
    const muse::secs_t start = selectionController()->dataSelectedStartTime();
    muse::secs_t newEnd = stepFromTime(selectionController()->dataSelectedEndTime(), Direction::Left);
    if (muse::RealIsEqualOrLess(newEnd, start)) {
        newEnd = start;
    }
    selectionController()->setDataSelectedEndTime(newEnd, true);
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
