#pragma once

#include <QObject>

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "processing/iselectioncontroller.h"

//! NOTE This class does two things:
//! 1. This is a context that is passed to other classes
//! 2. This is a controller that interprets mouse and view resize events into context values
//!
//! If this class becomes more complex,
//! or we notice that its "controller" methods are being called in unexpected places,
//! then we should split it into two separate classes.

namespace au::projectscene {
class TimelineContext : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    //  0 sec     visible frame          end
    //          | ~~~~~ ~~~~ ~~~|
    Q_PROPERTY(double frameStartTime READ frameStartTime NOTIFY frameStartTimeChanged FINAL)
    Q_PROPERTY(double frameEndTime READ frameEndTime NOTIFY frameEndTimeChanged FINAL)
    Q_PROPERTY(double zoom READ zoom WRITE setZoom NOTIFY zoomChanged FINAL)
    Q_PROPERTY(int BPM READ BPM WRITE setBPM NOTIFY BPMChanged FINAL)

    Q_PROPERTY(double selectionStartTime READ selectionStartTime NOTIFY selectionStartTimeChanged FINAL)
    Q_PROPERTY(double selectionEndTime READ selectionEndTime NOTIFY selectionEndTimeChanged FINAL)
    Q_PROPERTY(bool selectionActive READ selectionActive NOTIFY selectionActiveChanged FINAL)

    muse::Inject<processing::ISelectionController> selectionController;

public:

    TimelineContext(QObject* parent = nullptr);

    double frameStartTime() const;
    double frameEndTime() const;

    double zoom() const;
    void setZoom(double zoom);

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
    Q_INVOKABLE bool onWheel(double y);

    Q_INVOKABLE double timeToPosition(double time) const;
    Q_INVOKABLE double positionToTime(double position) const;

    void moveToFrameTime(double startTime);
    void shiftFrameTime(double secs);

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

private:

    void shiftFrameTimeOnStep(int direction);
    void setFrameStartTime(double newFrameStartTime);
    void setFrameEndTime(double newFrameEndTime);
    void updateFrameTime();

    void changeZoom(int direction);

    void setSelectionStartTime(double time);
    void setSelectionEndTime(double time);
    void updateSelectionActive();

    double m_frameWidth = 0.0;
    double m_frameStartTime = 0.0;
    double m_frameEndTime = 0.0;

    double m_zoom = 1.0; // see init
    int m_BPM = 120;
    // time signature
    int m_timeSigUpper = 4;
    int m_timeSigLower = 4;

    processing::secs_t m_selecitonStartTime = -1.0;
    processing::secs_t m_selectionEndTime = -1.0;
    bool m_selectionActive = false;
};
}
