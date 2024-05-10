#pragma once

#include <QQuickItem>

class TimelineContext : public QQuickItem
{
    Q_OBJECT

    Q_PROPERTY(double offset READ offset WRITE setOffset NOTIFY offsetChanged FINAL)
    Q_PROPERTY(double zoom READ zoom WRITE setZoom NOTIFY zoomChanged FINAL)

    Q_PROPERTY(double selectionStartTime READ selectionStartTime NOTIFY selectionStartTimeChanged FINAL)
    Q_PROPERTY(double selectionEndTime READ selectionEndTime NOTIFY selectionEndTimeChanged FINAL)
    Q_PROPERTY(bool selectionActive READ selectionActive NOTIFY selectionActiveChanged FINAL)

public:

    TimelineContext(QQuickItem* parent = nullptr);

    Q_INVOKABLE void onWheel(double y);
    Q_INVOKABLE void onSelection(double x1, double x2);
    Q_INVOKABLE void resetSelection();

    Q_INVOKABLE qint64 timeToPosition(double time) const;
    Q_INVOKABLE double positionToTime(qint64 position) const;

    double offset() const;
    void setOffset(double newOffset);
    double zoom() const;
    void setZoom(double zoom);

    double selectionStartTime() const;
    void setSelectionStartTime(double time);
    double selectionEndTime() const;
    void setSelectionEndTime(double time);
    bool selectionActive() const;

signals:

    void offsetChanged();
    void zoomChanged();

    void selectionStartTimeChanged();
    void selectionEndTimeChanged();
    void selectionActiveChanged();

private:

    void changeZoom(int direction);
    void changeOffset(int direction);

    void onSelectionTime(double t1, double t2);
    void updateSelectionActive();
    void setSelectionActive(bool newSelectionActive);

    double m_offset = 0.0;
    double m_zoom = 2.0;//{ 44100.0 / 512.0 };

    double m_selecitonStartTime = 0.0;
    double m_selectionEndTime = 0.0;
    bool m_selectionActive = false;
};
