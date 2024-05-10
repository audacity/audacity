#include "timelinecontext.h"

#include <QApplication>

#include "global/types/number.h"

#include "log.h"

static constexpr double ZOOM_MIN = 0.1;

TimelineContext::TimelineContext(QQuickItem* parent)
    : QQuickItem(parent)
{
}

void TimelineContext::onWheel(double y)
{
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

void TimelineContext::onSelection(double x1, double x2)
{
    onSelectionTime(positionToTime(x1), positionToTime(x2));
}

void TimelineContext::resetSelection()
{
    onSelectionTime(0.0, 0.0);
}

void TimelineContext::onSelectionTime(double startTime, double endTime)
{
    if (startTime > endTime) {
        std::swap(startTime, endTime);
    }

    setSelectionStartTime(startTime);
    setSelectionEndTime(endTime);
    setSelectionActive(!muse::is_zero(startTime) && !muse::is_zero(endTime));
}

qint64 TimelineContext::timeToPosition(double time) const
{
    double t = 0.5 + m_zoom * (time - m_offset);
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
    return m_offset + position / m_zoom;
}

double TimelineContext::offset() const
{
    return m_offset;
}

void TimelineContext::setOffset(double newOffset)
{
    if (m_offset != newOffset) {
        m_offset = newOffset;
        emit offsetChanged();
    }
}

double TimelineContext::zoom() const
{
    return m_zoom;
}

void TimelineContext::setZoom(double zoom)
{
    if (m_zoom != zoom) {
        m_zoom = zoom;
        emit zoomChanged();
    }
}

double TimelineContext::selectionStartTime() const
{
    return m_selecitonStartTime;
}

void TimelineContext::setSelectionStartTime(double time)
{
    if (m_selecitonStartTime != time) {
        m_selecitonStartTime = time;
        emit selectionEndTimeChanged();
    }
}

double TimelineContext::selectionEndTime() const
{
    return m_selectionEndTime;
}

void TimelineContext::setSelectionEndTime(double time)
{
    if (m_selectionEndTime != time) {
        m_selectionEndTime = time;
        emit selectionEndTimeChanged();
    }
}

bool TimelineContext::selectionActive() const
{
    return m_selectionActive;
}

void TimelineContext::setSelectionActive(bool newSelectionActive)
{
    if (m_selectionActive == newSelectionActive) {
        return;
    }
    m_selectionActive = newSelectionActive;
    emit selectionActiveChanged();
}
