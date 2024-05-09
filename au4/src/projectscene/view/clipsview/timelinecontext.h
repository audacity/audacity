#pragma once

#include <QQuickItem>

class TimelineContext : public QQuickItem
{
    Q_OBJECT

    Q_PROPERTY(double offset READ offset WRITE setOffset NOTIFY offsetChanged)
    Q_PROPERTY(double zoom READ zoom WRITE setZoom NOTIFY zoomChanged)

    Q_PROPERTY(double selectionStartTime READ selectionStartTime WRITE setSelectionStartTime NOTIFY selectionStartTimeChanged)
    Q_PROPERTY(double selectionEndTime READ selectionEndTime WRITE setSelectionEndTime NOTIFY selectionEndTimeChanged)

    Q_PROPERTY(int tracksOriginOffset READ tracksOriginOffset WRITE setTracksOriginOffset NOTIFY tracksOriginOffsetChanged)

public:

    TimelineContext(QQuickItem* parent = nullptr);

    Q_INVOKABLE void onWheel(double y);

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

    int tracksOriginOffset() const;
    void setTracksOriginOffset(int offset);

signals:

    void offsetChanged();
    void zoomChanged();
    void selectionStartTimeChanged();
    void selectionEndTimeChanged();
    void tracksOriginOffsetChanged();

private:

    void changeZoom(int direction);

    double mOffset = 0.0;
    double mZoom = 2.0;//{ 44100.0 / 512.0 };
    double mSelecitonStartTime{};
    double mSelectionEndTime{};

    int mTracksOriginOffset{};
};
