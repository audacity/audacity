#pragma once

#include <QObject>

//! NOTE This class does two things:
//! 1. This is a context that is passed to other classes
//! 2. This is a controller that interprets mouse and view resize events into context values
//!
//! If this class becomes more complex,
//! or we notice that its "controller" methods are being called in unexpected places,
//! then we should split it into two separate classes.

class TimelineContext : public QObject
{
    Q_OBJECT

    //  0 sec     visible frame          end
    //          | ~~~~~ ~~~~ ~~~|
    Q_PROPERTY(double frameStartTime READ frameStartTime NOTIFY frameStartTimeChanged FINAL)
    Q_PROPERTY(double frameEndTime READ frameEndTime NOTIFY frameEndTimeChanged FINAL)
    Q_PROPERTY(double zoom READ zoom WRITE setZoom NOTIFY zoomChanged FINAL)

    Q_PROPERTY(double selectionStartTime READ selectionStartTime NOTIFY selectionStartTimeChanged FINAL)
    Q_PROPERTY(double selectionEndTime READ selectionEndTime NOTIFY selectionEndTimeChanged FINAL)
    Q_PROPERTY(bool selectionActive READ selectionActive NOTIFY selectionActiveChanged FINAL)

public:

    TimelineContext(QObject* parent = nullptr);

    double frameStartTime() const;
    double frameEndTime() const;

    double zoom() const;
    void setZoom(double zoom);

    double selectionStartTime() const;
    void setSelectionStartTime(double time);
    double selectionEndTime() const;
    void setSelectionEndTime(double time);
    bool selectionActive() const;

    Q_INVOKABLE void init(double frameWidth);

    Q_INVOKABLE void onResizeFrameWidth(double frameWidth);
    Q_INVOKABLE void onWheel(double y);

    Q_INVOKABLE void onSelection(double x1, double x2);
    Q_INVOKABLE void resetSelection();

    Q_INVOKABLE double timeToPosition(double time) const;
    Q_INVOKABLE double positionToTime(double position) const;

signals:

    void frameStartTimeChanged();
    void frameEndTimeChanged();
    void frameTimeChanged(); // any or both together

    void zoomChanged();

    void selectionStartTimeChanged();
    void selectionEndTimeChanged();
    void selectionActiveChanged();

private:

    void shiftFrameTime(int direction);
    void setFrameStartTime(double newFrameStartTime);
    void setFrameEndTime(double newFrameEndTime);
    void updateFrameTime();

    void changeZoom(int direction);

    void onSelectionTime(double t1, double t2);
    void updateSelectionActive();
    void setSelectionActive(bool newSelectionActive);

    double m_frameWidth = 0.0;
    double m_frameStartTime = 0.0;
    double m_frameEndTime = 0.0;

    double m_zoom = 1.0; // see init

    double m_selecitonStartTime = 0.0;
    double m_selectionEndTime = 0.0;
    bool m_selectionActive = false;
};
