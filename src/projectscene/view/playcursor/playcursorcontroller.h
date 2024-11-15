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

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "actions/iactionsdispatcher.h"

#include "../timeline/timelinecontext.h"

namespace au::projectscene {
class PlayCursorController : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(TimelineContext * context READ timelineContext WRITE setTimelineContext NOTIFY timelineContextChanged FINAL)

    Q_PROPERTY(double positionX READ positionX NOTIFY positionXChanged FINAL)

    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;

public:
    PlayCursorController(QObject* parent = nullptr);

    TimelineContext* timelineContext() const;
    void setTimelineContext(TimelineContext* newContext);
    double positionX() const;

    Q_INVOKABLE void init();
    Q_INVOKABLE void seekToX(double x, bool triggerPlay = false);
    Q_INVOKABLE void setPlaybackRegion(double x1, double x2);

signals:
    void timelineContextChanged();
    void positionXChanged();

private slots:
    void onFrameTimeChanged();

private:
    context::IPlaybackStatePtr playbackState() const;
    projectscene::IProjectViewStatePtr projectViewState() const;

    void updatePositionX(muse::secs_t secs);

    TimelineContext* m_context = nullptr;
    double m_positionX = 0.0;
};
}
