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

#include <QObject>

#include "actions/actionable.h"
#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "actions/iactionsdispatcher.h"
#include "trackedit/iselectioncontroller.h"

#include "../timeline/timelinecontext.h"

namespace au::projectscene {
class PlayPositionActionController : public QObject, public muse::actions::Actionable, public muse::async::Asyncable,
    public muse::Contextable
{
    Q_OBJECT

    Q_PROPERTY(TimelineContext * context READ timelineContext WRITE setTimelineContext NOTIFY timelineContextChanged FINAL)

    muse::ContextInject<context::IGlobalContext> globalContext{ this };
    muse::ContextInject<muse::actions::IActionsDispatcher> dispatcher{ this };
    muse::ContextInject<trackedit::ISelectionController> selectionController{ this };

public:
    PlayPositionActionController(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    TimelineContext* timelineContext() const;
    void setTimelineContext(TimelineContext* newContext);

    void playPositionDecrease();
    void playPositionIncrease();

    void selectionExtendLeft();
    void selectionExtendRight();
    void selectionContractLeft();
    void selectionContractRight();

signals:
    void timelineContextChanged();

private:
    void onProjectChanged();

    void snapCurrentPosition();
    void applySingleStep(Direction direction);

    muse::secs_t stepFromTime(muse::secs_t from, Direction direction) const;

    context::IPlaybackStatePtr playbackState() const;

    TimelineContext* m_context = nullptr;
};
}
