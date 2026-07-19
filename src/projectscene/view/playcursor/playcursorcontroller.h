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
#include <QPointF>
#include <QTimer>

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "playback/iplaybackcontroller.h"
#include "record/irecordcontroller.h"
#include "actions/iactionsdispatcher.h"

#include "../timeline/timelinecontext.h"

namespace au::projectscene {
class PlayCursorController : public QObject, public muse::async::Asyncable, public muse::Contextable
{
    Q_OBJECT

    Q_PROPERTY(TimelineContext * context READ timelineContext WRITE setTimelineContext NOTIFY timelineContextChanged FINAL)

    Q_PROPERTY(double positionX READ positionX NOTIFY positionXChanged FINAL)

    muse::ContextInject<context::IGlobalContext> globalContext{ this };
    muse::ContextInject<playback::IPlaybackController> playbackController{ this };
    muse::ContextInject<record::IRecordController> recordController{ this };
    muse::ContextInject<muse::actions::IActionsDispatcher> dispatcher{ this };

public:
    PlayCursorController(QObject* parent = nullptr);

    TimelineContext* timelineContext() const;
    void setTimelineContext(TimelineContext* newContext);
    double positionX() const;

    Q_INVOKABLE void init();
    Q_INVOKABLE void seekToTime(double secs, bool triggerPlay = false);
    Q_INVOKABLE void animatedSeekToTime(double secs);
    Q_INVOKABLE void setPlaybackRegionByTime(double time1, double time2);

    //! NOTE Deferred seek: a press in the track area must not move the playhead
    //! immediately, otherwise the viewport can shift mid-gesture (e.g. while
    //! dragging a clip or a selection). The target is remembered on press and
    //! the seek is dispatched on release, and only when the gesture stayed a
    //! plain click — dragging anything other than the play cursor itself never
    //! moves the playhead.
    Q_INVOKABLE void beginSeekGesture(double time, double x, double y);
    Q_INVOKABLE void updateSeekGesture(double x, double y);
    Q_INVOKABLE bool endSeekGesture();
    Q_INVOKABLE void cancelSeekGesture();

signals:
    void timelineContextChanged();
    void positionXChanged();

private slots:
    void onFrameTimeChanged();

private:
    context::IPlaybackStatePtr playbackState() const;

    double snapTime(double time) const;
    void updatePositionX(muse::secs_t secs);
    void ensureCursorAtCenter(muse::secs_t secs) const;

    void onUserHorizontalScroll();
    void onScrollSuppressionTimeout();
    void clearScrollSuppression();

    TimelineContext* m_context = nullptr;
    double m_positionX = 0.0;

    QTimer m_scrollSuppressionTimer;
    bool m_viewUpdatesSuppressed = false;
    bool m_seekAnimated = false;

    bool m_seekGestureActive = false;
    bool m_seekGestureDragged = false;
    double m_seekGestureTime = 0.0;
    QPointF m_seekGesturePressPos;

    friend struct SnapTestAccess;
};
}
