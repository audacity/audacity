/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "processing/iprocessingselectioncontroller.h"

#include "../timeline/timelinecontext.h"

namespace au::projectscene {
class SelectionViewController : public QObject
{
    Q_OBJECT
    Q_PROPERTY(TimelineContext * context READ timelineContext WRITE setTimelineContext NOTIFY timelineContextChanged FINAL)

    Q_PROPERTY(bool selectionActive READ selectionActive NOTIFY selectionActiveChanged FINAL)

    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<processing::IProcessingSelectionController> selectionController;

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
    std::vector<processing::TrackId> trackIdList() const;

    std::vector<processing::TrackId> determinateTracks(double y1, double y2) const;

    TimelineContext* m_context = nullptr;

    bool m_selectionStarted = false;
    bool m_selectionActive = false;
    QPointF m_startPoint;
};
}
