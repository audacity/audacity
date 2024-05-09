#include "timelinecontext.h"

#include <QApplication>

#include "log.h"

static constexpr double ZOOM_MIN = 0.1;

TimelineContext::TimelineContext(QQuickItem* parent)
    : QQuickItem(parent)
{
}

void TimelineContext::onWheel(double y)
{
    LOGD() << y;

    Qt::KeyboardModifiers modifiers = QApplication::keyboardModifiers();
    if (modifiers.testFlag(Qt::ControlModifier)) {
        changeZoom(y < 0 ? -1 : 1);
    } else if (modifiers.testFlag(Qt::ShiftModifier)) {
        changeOffset(y < 0 ? -1 : 1);
    }
}

void TimelineContext::changeZoom(int direction)
{
    double step = std::max(zoom() / 2.0, ZOOM_MIN);

    double _zoom = zoom() + (step * direction);
    _zoom = std::floor(_zoom * 10.0) / 10.0;
    if (_zoom > 4) {
        _zoom = std::floor(_zoom);
    }

    _zoom = std::max(_zoom, ZOOM_MIN);

    setZoom(_zoom);
}

void TimelineContext::changeOffset(int direction)
{
    double step = 10;
    setOffset(offset() + (step * direction));
}

qint64 TimelineContext::timeToPosition(double time) const
{
    double t = 0.5 + mZoom * (time - mOffset);
    if (t < INT64_MIN) {
        return INT64_MIN;
    }
    if (t > INT64_MAX) {
        return INT64_MAX;
    }
    t = floor(t);
    return static_cast<qint64>(t);
}

double TimelineContext::positionToTime(qint64 position) const
{
    return mOffset + position / mZoom;
}

double TimelineContext::offset() const
{
    return mOffset;
}

void TimelineContext::setOffset(double newOffset)
{
    if (mOffset != newOffset) {
        mOffset = newOffset;
        emit offsetChanged();
    }
}

double TimelineContext::zoom() const
{
    return mZoom;
}

void TimelineContext::setZoom(double zoom)
{
    if (mZoom != zoom) {
        mZoom = zoom;
        emit zoomChanged();
    }
}

double TimelineContext::selectionStartTime() const
{
    return mSelecitonStartTime;
}

void TimelineContext::setSelectionStartTime(double time)
{
    if (mSelecitonStartTime != time) {
        mSelecitonStartTime = time;
        emit selectionEndTimeChanged();
    }
}

double TimelineContext::selectionEndTime() const
{
    return mSelectionEndTime;
}

void TimelineContext::setSelectionEndTime(double time)
{
    if (mSelectionEndTime != time) {
        mSelectionEndTime = time;
        emit selectionEndTimeChanged();
    }
}

int TimelineContext::tracksOriginOffset() const
{
    return mTracksOriginOffset;
}

void TimelineContext::setTracksOriginOffset(int offset)
{
    if (mTracksOriginOffset != offset) {
        mTracksOriginOffset = offset;
        emit tracksOriginOffsetChanged();
    }
}
