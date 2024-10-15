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

#include "../timeline/timelinecontext.h"

namespace au::projectscene {
class PlayPositionActionController : public QObject, public muse::actions::Actionable, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(TimelineContext * context READ timelineContext WRITE setTimelineContext NOTIFY timelineContextChanged FINAL)

    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;

public:
    PlayPositionActionController(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    TimelineContext* timelineContext() const;
    void setTimelineContext(TimelineContext* newContext);

    void playPositionDecrease();
    void playPositionIncrease();

signals:
    void timelineContextChanged();

private:
    void onProjectChanged();

    void snapCurrentPosition();
    void applySingleStep(Direction direction);

    TimelineContext* m_context = nullptr;
};
}
