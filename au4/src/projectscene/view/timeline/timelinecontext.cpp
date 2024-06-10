#include "timelinecontext.h"

#include <QApplication>

#include "global/types/number.h"

#include "log.h"

static constexpr double ZOOM_MIN = 0.1;

using namespace au::projectscene;

TimelineContext::TimelineContext(QObject* parent)
    : QObject(parent)
{
}

void TimelineContext::init(double frameWidth)
{
    m_zoom = 2.0; //{ 44100.0 / 512.0 };
    emit zoomChanged();

    m_frameStartTime = 0.0;
    emit frameStartTimeChanged();
    m_frameEndTime = positionToTime(frameWidth);
    emit frameEndTimeChanged();
    emit frameTimeChanged();

    muse::ValCh<processing::secs_t> selectedStartTime = processingSelectionController()->dataSelectedStartTime();
    m_selecitonStartTime = selectedStartTime.val;
    selectedStartTime.ch.onReceive(this, [this](processing::secs_t time) {
        m_selecitonStartTime = time;
        emit selectionStartTimeChanged();
        updateSelectionActive();
    });

    muse::ValCh<processing::secs_t> selectedEndTime = processingSelectionController()->dataSelectedEndTime();
    m_selectionEndTime = selectedEndTime.val;
    selectedEndTime.ch.onReceive(this, [this](processing::secs_t time) {
        m_selectionEndTime = time;
        emit selectionEndTimeChanged();
        updateSelectionActive();
    });
}

bool TimelineContext::onWheel(double y)
{
    Qt::KeyboardModifiers modifiers = QApplication::keyboardModifiers();
    if (modifiers.testFlag(Qt::ControlModifier)) {
        changeZoom(y < 0 ? -1 : 1);
        return true;
    } else if (modifiers.testFlag(Qt::ShiftModifier)) {
        shiftFrameTimeOnStep(y < 0 ? -1 : 1);
        return true;
    }

    return false;
}

void TimelineContext::changeZoom(int direction)
{
    double step = (muse::is_equal(m_zoom, 1.0) && direction < 0) ? 0.1 : 1.0;

    double zoom = m_zoom + (step * direction);
    zoom = std::max(zoom, ZOOM_MIN);

    setZoom(zoom);
}

void TimelineContext::onResizeFrameWidth(double frameWidth)
{
    m_frameWidth = frameWidth;
    updateFrameTime();
}

void TimelineContext::moveToFrameTime(double startTime)
{
    setFrameStartTime(startTime);
    updateFrameTime();
}

void TimelineContext::shiftFrameTime(double shift)
{
    // do not shift to negative time values
    if (m_frameStartTime + shift < 0) {
        return;
    }
    setFrameStartTime(m_frameStartTime + shift);
    setFrameEndTime(m_frameEndTime + shift);

    emit frameTimeChanged();
}

void TimelineContext::shiftFrameTimeOnStep(int direction)
{
    double step = 10.0;
    double shift = step * direction;
    shiftFrameTime(shift);
}

void TimelineContext::updateFrameTime()
{
    setFrameEndTime(positionToTime(m_frameWidth));
    emit frameTimeChanged();
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

double TimelineContext::selectionStartTime() const
{
    return m_selecitonStartTime.raw();
}

double TimelineContext::selectionEndTime() const
{
    return m_selectionEndTime.raw();
}

bool TimelineContext::selectionActive() const
{
    return m_selectionActive;
}

void TimelineContext::updateSelectionActive()
{
    bool isActive = m_selecitonStartTime >= 0.0
                    && m_selectionEndTime > 0.0
                    && m_selectionEndTime > m_selecitonStartTime;

    if (m_selectionActive == isActive) {
        return;
    }
    m_selectionActive = isActive;
    emit selectionActiveChanged();
}
