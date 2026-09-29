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
#pragma once

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "actions/iactionsdispatcher.h"
#include "trackedit/iselectioncontroller.h"

#include "projectscene/iplaypositionviewcontroller.h"
#include "projectscene/iprojectsceneactionscontroller.h"
#include "../timeline/timelinecontext.h"

namespace au::projectscene {
class PlayPositionActionController : public IPlayPositionViewController, public muse::async::Asyncable, public muse::Contextable
{
    muse::ContextInject<context::IGlobalContext> globalContext{ this };
    muse::ContextInject<muse::actions::IActionsDispatcher> dispatcher{ this };
    muse::ContextInject<trackedit::ISelectionController> selectionController{ this };
    muse::ContextInject<IProjectSceneActionsController> projectSceneActionsController{ this };

public:
    PlayPositionActionController(TimelineContext* context, const muse::modularity::ContextPtr& ctx);

    void init();
    void deinit();

    void playPositionDecrease() override;
    void playPositionIncrease() override;

    void selectionExtendLeft() override;
    void selectionExtendRight() override;
    void selectionContractLeft() override;
    void selectionContractRight() override;

    void cursorToSelectionStart() override;
    void cursorToSelectionEnd() override;

private:
    void onProjectChanged();

    void snapCurrentPosition();
    void applySingleStep(Direction direction);
    void movePlayPositionTo(muse::secs_t secs);

    muse::secs_t stepFromTime(muse::secs_t from, Direction direction) const;

    context::IPlaybackStatePtr playbackState() const;

    TimelineContext* m_context = nullptr;

    friend struct SnapTestAccess;
};
}
