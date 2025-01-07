/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "trackedit/iselectioncontroller.h"

#include "../timeline/timelinecontext.h"

namespace au::projectscene {
class SelectionViewController : public QObject
{
    Q_OBJECT
    Q_PROPERTY(TimelineContext * context READ timelineContext WRITE setTimelineContext NOTIFY timelineContextChanged FINAL)

    Q_PROPERTY(bool selectionActive READ selectionActive NOTIFY selectionActiveChanged FINAL)

    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<trackedit::ISelectionController> selectionController;

public:
    SelectionViewController(QObject* parent = nullptr);

    TimelineContext* timelineContext() const;
    void setTimelineContext(TimelineContext* newContext);

    //! NOTE The x coordinates must match the timeline.
    //! The y coordinates must match the track view
    //! If this is not the case, then appropriate adjustments must be made.
    Q_INVOKABLE void onPressed(double x, double y);
    Q_INVOKABLE void onPositionChanged(double x, double y);
    Q_INVOKABLE void onReleased(double x, double y);

    Q_INVOKABLE void onSelectionDraged(double x, double x2, bool completed);

    Q_INVOKABLE void selectTrackAudioData(double y);
    Q_INVOKABLE void selectClipAudioData(const ClipKey& clipKey);

    Q_INVOKABLE void resetSelectedClip();
    Q_INVOKABLE void resetDataSelection();
    Q_INVOKABLE bool isLeftSelection(double x);

    bool selectionActive() const;
    void setSelectionActive(bool newSelectionActive);

signals:
    void timelineContextChanged();
    void selectionActiveChanged();

    void selectionStarted();
    void selectionChanged(QPointF p1, QPointF p2);
    void selectionEnded(QPointF p1, QPointF p2);

private:

    IProjectViewStatePtr viewState() const;
    trackedit::TrackIdList trackIdList() const;
    void setSelection(double x1, double x2, bool complete);

    Qt::KeyboardModifiers keyboardModifiers() const;

    bool isProjectOpened() const;

    TimelineContext* m_context = nullptr;
    QMetaObject::Connection m_autoScrollConnection;

    bool m_selectionStarted = false;
    bool m_selectionActive = false;
    QPointF m_startPoint;
    QPointF m_lastPoint;
};
}
