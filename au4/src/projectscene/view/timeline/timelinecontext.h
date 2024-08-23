#pragma once

#include <QObject>

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "trackedit/iselectioncontroller.h"
#include "iprojectsceneconfiguration.h"

//! NOTE This class does two things:
//! 1. This is a context that is passed to other classes
//! 2. This is a controller that interprets mouse and view resize events into context values
//!
//! If this class becomes more complex,
//! or we notice that its "controller" methods are being called in unexpected places,
//! then we should split it into two separate classes.

namespace au::projectscene {
class SnapTimeFormatter;
class TimelineContext : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    //  0 sec     visible frame          end
    //          | ~~~~~ ~~~~ ~~~|
    Q_PROPERTY(double frameStartTime READ frameStartTime NOTIFY frameStartTimeChanged FINAL)
    Q_PROPERTY(double frameEndTime READ frameEndTime NOTIFY frameEndTimeChanged FINAL)
    Q_PROPERTY(double zoom READ zoom NOTIFY zoomChanged FINAL)
    Q_PROPERTY(int BPM READ BPM WRITE setBPM NOTIFY BPMChanged FINAL)

    Q_PROPERTY(double selectionStartTime READ selectionStartTime NOTIFY selectionStartTimeChanged FINAL)
    Q_PROPERTY(double selectionEndTime READ selectionEndTime NOTIFY selectionEndTimeChanged FINAL)
    Q_PROPERTY(bool selectionActive READ selectionActive NOTIFY selectionActiveChanged FINAL)

    Q_PROPERTY(qreal startHorizontalScrollPosition READ startHorizontalScrollPosition NOTIFY horizontalScrollChanged)
    Q_PROPERTY(qreal horizontalScrollbarSize READ horizontalScrollbarSize NOTIFY horizontalScrollChanged)
    Q_PROPERTY(
        qreal startVerticalScrollPosition READ startVerticalScrollPosition WRITE setStartVerticalScrollPosition NOTIFY verticalScrollChanged)
    Q_PROPERTY(qreal verticalScrollbarSize READ verticalScrollbarSize NOTIFY verticalScrollChanged)

    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<trackedit::ISelectionController> selectionController;
    muse::Inject<IProjectSceneConfiguration> configuration;

public:

    TimelineContext(QObject* parent = nullptr);

    double frameStartTime() const;
    double frameEndTime() const;

    double zoom() const;
    void setZoom(double zoom, double mouseX);

    int BPM() const;
    void setBPM(int BPM);

    int timeSigUpper() const;
    void setTimeSigUpper(int timeSigUpper);

    int timeSigLower() const;
    void setTimeSigLower(int timeSigLower);

    double selectionStartTime() const;
    double selectionEndTime() const;
    bool selectionActive() const;

    Q_INVOKABLE void init(double frameWidth);

    Q_INVOKABLE void onResizeFrameWidth(double frameWidth);
    Q_INVOKABLE void onResizeFrameHeight(double frameHeight);
    Q_INVOKABLE void onResizeFrameContentHeight(double frameHeight);

    Q_INVOKABLE void onWheel(double mouseX, const QPoint& pixelDelta, const QPoint& angleDelta);
    Q_INVOKABLE void pinchToZoom(qreal scaleFactor, const QPointF& pos);
    Q_INVOKABLE void scrollHorizontal(qreal newPos);
    Q_INVOKABLE void scrollVertical(qreal newPos);

    Q_INVOKABLE double timeToPosition(double time) const;
    Q_INVOKABLE double positionToTime(double position, bool withSnap = false) const;
    double singleStepToTime(double position, Direction direction, const Snap& snap) const;

    void moveToFrameTime(double startTime);
    void shiftFrameTime(double secs);

    qreal startHorizontalScrollPosition() const;
    qreal horizontalScrollbarSize() const;

    qreal startVerticalScrollPosition() const;
    void setStartVerticalScrollPosition(qreal position);

    qreal verticalScrollbarSize() const;

signals:

    void frameStartTimeChanged();
    void frameEndTimeChanged();
    void frameTimeChanged(); // any or both together

    void zoomChanged();
    void BPMChanged();
    void timeSigUpperChanged();
    void timeSigLowerChanged();

    void selectionStartTimeChanged();
    void selectionEndTimeChanged();
    void selectionActiveChanged();

    void viewContentYChangeRequested(double contentY);
    void shiftViewByY(double dy);

    void horizontalScrollChanged();
    void verticalScrollChanged();

private:
    trackedit::ITrackeditProjectPtr trackEditProject() const;
    IProjectViewStatePtr viewState() const;
    void onProjectChanged();

    void shiftFrameTimeOnStep(int direction);
    void setFrameStartTime(double newFrameStartTime);
    void setFrameEndTime(double newFrameEndTime);
    void updateFrameTime();

    void setSelectionStartTime(double time);
    void setSelectionEndTime(double time);
    void updateSelectionActive();

    void updateTimeSignature();

    qreal horizontalScrollableSize() const;
    qreal verticalScrollableSize() const;

    double timeToContentPosition(double time) const;

    double m_frameWidth = 0.0;
    double m_frameHeight = 0.0;
    double m_frameContentHeight = 0.0;

    double m_frameStartTime = 0.0;
    double m_frameEndTime = 0.0;

    double m_lastZoomEndTime = 0.0;

    double m_zoom = 1.0; // see init
    int m_BPM = 120;
    // time signature
    int m_timeSigUpper = 4;
    int m_timeSigLower = 4;

    trackedit::secs_t m_selecitonStartTime = -1.0;
    trackedit::secs_t m_selectionEndTime = -1.0;
    bool m_selectionActive = false;

    std::shared_ptr<SnapTimeFormatter> m_snapTimeFormatter;

    qreal m_previousVerticalScrollPosition = 0.0;
    qreal m_previousHorizontalScrollPosition = 0.0;

    qreal m_startVerticalScrollPosition = 0.0;
};
}
