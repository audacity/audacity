#pragma once

#include <QObject>

#include "global/async/asyncable.h"
#include "actions/actionable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "actions/iactionsdispatcher.h"
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
class TimelineContext : public QObject, public muse::async::Asyncable, public muse::actions::Actionable
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
    Q_PROPERTY(double selectionStartPosition READ selectionStartPosition NOTIFY selectionStartPositionChanged FINAL)
    Q_PROPERTY(double selectionEndPosition READ selectionEndPosition NOTIFY selectionEndPositionChanged FINAL)
    Q_PROPERTY(bool selectionActive READ selectionActive NOTIFY selectionActiveChanged FINAL)

    Q_PROPERTY(double selectedClipStartTime READ selectedClipStartTime NOTIFY selectedClipStartTimeChanged FINAL)
    Q_PROPERTY(double selectedClipEndTime READ selectedClipEndTime NOTIFY selectedClipEndTimeChanged FINAL)
    Q_PROPERTY(double selectedClipStartPosition READ selectedClipStartPosition NOTIFY selectedClipStartPositionChanged FINAL)
    Q_PROPERTY(double selectedClipEndPosition READ selectedClipEndPosition NOTIFY selectedClipEndPositionChanged FINAL)
    Q_PROPERTY(bool singleClipSelected READ singleClipSelected NOTIFY singleClipSelectedChanged FINAL)

    Q_PROPERTY(qreal startHorizontalScrollPosition READ startHorizontalScrollPosition NOTIFY horizontalScrollChanged)
    Q_PROPERTY(qreal horizontalScrollbarSize READ horizontalScrollbarSize NOTIFY horizontalScrollChanged)
    Q_PROPERTY(
        qreal startVerticalScrollPosition READ startVerticalScrollPosition WRITE setStartVerticalScrollPosition NOTIFY verticalScrollChanged)
    Q_PROPERTY(qreal verticalScrollbarSize READ verticalScrollbarSize NOTIFY verticalScrollChanged)

    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;
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
    double selectionStartPosition() const;
    double selectionEndPosition() const;
    bool selectionActive() const;

    double selectedClipStartTime() const;
    double selectedClipEndTime() const;
    double selectedClipStartPosition() const;
    double selectedClipEndPosition() const;
    bool singleClipSelected() const;

    Q_INVOKABLE void init(double frameWidth);

    Q_INVOKABLE void onResizeFrameWidth(double frameWidth);
    Q_INVOKABLE void onResizeFrameHeight(double frameHeight);
    Q_INVOKABLE void onResizeFrameContentHeight(double frameHeight);

    Q_INVOKABLE void onWheel(double mouseX, const QPoint& pixelDelta, const QPoint& angleDelta);
    Q_INVOKABLE void pinchToZoom(qreal scaleFactor, const QPointF& pos);
    Q_INVOKABLE void scrollHorizontal(qreal newPos);
    Q_INVOKABLE void scrollVertical(qreal newPos);

    Q_INVOKABLE void insureVisible(double posSec);
    Q_INVOKABLE void startAutoScroll(double posSec);
    Q_INVOKABLE void stopAutoScroll();

    Q_INVOKABLE double timeToPosition(double time) const;
    Q_INVOKABLE double positionToTime(double position, bool withSnap = false) const;
    double singleStepToTime(double position, Direction direction, const Snap& snap) const;
    double applySnapToTime(double time) const;

    Q_INVOKABLE void updateMousePositionTime(double mouseX);
    Q_INVOKABLE double mousePositionTime() const;

    void moveToFrameTime(double startTime);
    void shiftFrameTime(double secs);

    qreal startHorizontalScrollPosition() const;
    qreal horizontalScrollbarSize() const;

    qreal startVerticalScrollPosition() const;
    void setStartVerticalScrollPosition(qreal position);

    qreal verticalScrollbarSize() const;

    void updateSelectedClipTime();

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
    void selectionStartPositionChanged();
    void selectionEndPositionChanged();
    void selectionActiveChanged();

    void selectedClipStartTimeChanged();
    void selectedClipEndTimeChanged();
    void selectedClipStartPositionChanged();
    void selectedClipEndPositionChanged();
    void singleClipSelectedChanged();

    void viewContentYChangeRequested(double contentY);

    void horizontalScrollChanged();
    void verticalScrollChanged();

private:
    trackedit::ITrackeditProjectPtr trackEditProject() const;
    IProjectViewStatePtr viewState() const;
    void onProjectChanged();

    void zoomIn();
    void zoomOut();

    qreal frameCenterPosition() const;
    qreal selectionCenterPosition() const;
    qreal findZoomFocusPosition() const;

    void fitSelectionToWidth();
    void fitProjectToWidth();
    void updateViewOnProjectTempoChange(double ratio);

    bool hasSelection() const;

    void shiftFrameTimeOnStep(int direction);
    void setFrameStartTime(double newFrameStartTime);
    void setFrameEndTime(double newFrameEndTime);
    void updateFrameTime();
    void autoScrollView(double scrollStep);

    void setSelectionStartTime(double time);
    void setSelectionEndTime(double time);
    void updateSelectionActive();

    void setClipStartTime(double time);
    void setClipEndTime(double time);
    void updateSingleClipSelected();

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

    trackedit::secs_t m_selectionStartTime = -1.0;
    trackedit::secs_t m_selectionEndTime = -1.0;
    bool m_selectionActive = false;

    trackedit::secs_t m_selectedClipStartTime = -1.0;
    trackedit::secs_t m_selectedClipEndTime = -1.0;
    bool m_singleClipSelected = false;

    std::shared_ptr<SnapTimeFormatter> m_snapTimeFormatter;

    qreal m_previousVerticalScrollPosition = 0.0;
    qreal m_previousHorizontalScrollPosition = 0.0;

    qreal m_startVerticalScrollPosition = 0.0;

    double m_mousePositionTime = 0.0;

    QTimer m_scrollTimer;
    double m_autoScrollStep = 0.0;
};
}
