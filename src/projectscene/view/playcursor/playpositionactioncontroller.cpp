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

static const ActionQuery PLAYBACK_SEEK_QUERY("action://playback/seek");

PlayPositionActionController::PlayPositionActionController(TimelineContext* context, const muse::modularity::ContextPtr& ctx)
    : muse::Contextable(ctx), m_context(context)
{
}

void PlayPositionActionController::init()
{
    projectSceneActionsController()->setPlayPositionViewController(this);

    globalContext()->currentProjectChanged().onNotify(this, [this](){
        onProjectChanged();
    });

    onProjectChanged();
}

void PlayPositionActionController::deinit()
{
    if (projectSceneActionsController()->playPositionViewController() == this) {
        projectSceneActionsController()->setPlayPositionViewController(nullptr);
    }
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
        movePlayPositionTo(secs);

        if (!playbackState()->isPlaying()) {
            selectionController()->setDataSelectedStartTime(secs, true);
            selectionController()->setDataSelectedEndTime(secs, true);
        }
    }
}

void PlayPositionActionController::movePlayPositionTo(muse::secs_t secs)
{
    muse::actions::ActionQuery q(PLAYBACK_SEEK_QUERY);
    q.addParam("seekTime", muse::Val(secs));
    q.addParam("triggerPlay", muse::Val(false));
    dispatcher()->dispatch(q);

    m_context->animatedInsureVisible(secs);
}

void PlayPositionActionController::cursorToSelectionStart()
{
    //! NOTE: unlike stepping the cursor, this keeps the selection intact —
    //! it only moves the play position to one of its edges
    if (selectionController()->timeSelectionIsEmpty()) {
        return;
    }

    const muse::secs_t secs = selectionController()->dataSelectedStartTime();
    if (muse::RealIsEqualOrMore(secs, 0.0)) {
        movePlayPositionTo(secs);
    }
}

void PlayPositionActionController::cursorToSelectionEnd()
{
    if (selectionController()->timeSelectionIsEmpty()) {
        return;
    }

    const muse::secs_t secs = selectionController()->dataSelectedEndTime();
    if (muse::RealIsEqualOrMore(secs, 0.0)) {
        movePlayPositionTo(secs);
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
