#include "timelinecontext.h"

#include <QApplication>

#include "global/types/number.h"

#include "log.h"

static constexpr double ZOOM_MIN = 0.1;

TimelineContext::TimelineContext(QObject* parent)
    : QObject(parent)
{
}

void TimelineContext::init(double frameWidth)
{
    m_zoom = 2.0;//{ 44100.0 / 512.0 };
    emit zoomChanged();

    m_frameStartTime = 0.0;
    emit frameStartTimeChanged();
    m_frameEndTime = positionToTime(frameWidth);
    emit frameEndTimeChanged();
    emit frameTimeChanged();
}

void TimelineContext::onWheel(double y)
{
    Qt::KeyboardModifiers modifiers = QApplication::keyboardModifiers();
    if (modifiers.testFlag(Qt::ControlModifier)) {
        changeZoom(y < 0 ? -1 : 1);
    } else if (modifiers.testFlag(Qt::ShiftModifier)) {
        shiftFrameTime(y < 0 ? -1 : 1);
    }
}

void TimelineContext::changeZoom(int direction)
{
    double step = std::round(std::max(zoom() / 2.0, ZOOM_MIN) * 10) / 10;

    double _zoom = zoom() + (step * direction);
    _zoom = std::floor(_zoom * 10.0) / 10.0;
    if (_zoom > 4) {
        _zoom = std::floor(_zoom);
    }

    _zoom = std::max(_zoom, ZOOM_MIN);

    setZoom(_zoom);
}

void TimelineContext::onResizeFrameWidth(double frameWidth)
{
    m_frameWidth = frameWidth;
    updateFrameTime();
}

void TimelineContext::shiftFrameTime(int direction)
{
    double step = 10.0;
    double shift = step * direction;

    setFrameStartTime(m_frameStartTime + shift);
    setFrameEndTime(m_frameEndTime + shift);

    emit frameTimeChanged();
}

void TimelineContext::updateFrameTime()
{
    setFrameEndTime(m_frameStartTime + positionToTime(m_frameWidth));
    emit frameTimeChanged();
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

double TimelineContext::timeToPosition(double time) const
{
    double p = 0.5 + m_zoom * (time - m_frameStartTime);
    p = std::floor(p);
    return p;
}

double TimelineContext::positionToTime(double position) const
{
    return m_frameStartTime + position / m_zoom;
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
        updateFrameTime();
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

double TimelineContext::frameStartTime() const
{
    return m_frameStartTime;
}

void TimelineContext::setFrameStartTime(double newFrameStartTime)
{
    if (qFuzzyCompare(m_frameStartTime, newFrameStartTime)) {
        return;
    }
    m_frameStartTime = newFrameStartTime;
    emit frameStartTimeChanged();
}

double TimelineContext::frameEndTime() const
{
    return m_frameEndTime;
}

void TimelineContext::setFrameEndTime(double newFrameEndTime)
{
    if (qFuzzyCompare(m_frameEndTime, newFrameEndTime)) {
        return;
    }
    m_frameEndTime = newFrameEndTime;
    emit frameEndTimeChanged();
}
