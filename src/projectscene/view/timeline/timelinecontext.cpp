#include "timelinecontext.h"

#include <QApplication>
#include <QWheelEvent>

#include "global/types/number.h"

#include "snaptimeformatter.h"

#include "log.h"

static constexpr double ZOOM_MIN = 0.001;
static constexpr double ZOOM_MAX = 6000000.0;

// on wheel event factor
static constexpr int PIXELSSTEPSFACTOR = 5;

// horizontal auto-scroll setup
static constexpr double SCROLL_MARGIN_PX = 40.0;
static constexpr double SCROLL_MIN_SPEED = 0.00001;
static constexpr double SCROLL_MAX_SPEED = 0.025;

using namespace au::projectscene;

namespace {
// calculate auto-scroll speed based on where the mouse cursor is
// it's non linear range mapping:
// from (0px, SCROLL_MARGIN_PX) to (SCROLL_MIN_SPEED, SCROLL_MAX_SPEED)
double calculateScrollSpeed(double value, double inMin, double inMax, double outMin, double outMax)
{
    double sign = (muse::RealIsEqualOrMore(value, 0.0)) ? 1.0 : -1.0;
    double absValue = std::abs(value);
    double normalized = std::clamp((absValue - inMin) / (inMax - inMin), 0.0, 1.0);

    double scaled = std::pow(normalized, 4);
    double result = scaled * (outMax - outMin) + outMin;

    return sign * result;
}
}

TimelineContext::TimelineContext(QObject* parent)
    : QObject(parent)
{
    m_snapTimeFormatter = std::make_shared<SnapTimeFormatter>();

    m_scrollTimer.setInterval(16); // scroll at ~60 FPS
    connect(&m_scrollTimer, &QTimer::timeout, [this](){ autoScrollView(m_autoScrollStep); });
}

void TimelineContext::init(double frameWidth)
{
    double initialTimeRange = trackEditProject() ? trackEditProject()->totalTime().to_double() * 2 : 0.0;
    if (muse::is_zero(initialTimeRange)) {
        m_zoom = configuration()->zoom();
    } else {
        m_zoom = m_frameWidth / initialTimeRange;
    }
    emit zoomChanged();

    m_frameWidth = frameWidth;
    m_frameStartTime = 0.0;
    emit frameStartTimeChanged();
    m_frameEndTime = positionToTime(frameWidth);

    m_lastZoomEndTime = m_frameEndTime;

    emit frameEndTimeChanged();
    emit frameTimeChanged();

    selectionController()->clipsSelected().onReceive(this, [this](const trackedit::ClipKeyList&) {
        updateSingleClipSelected();
        updateSelectedClipTime();
    });

    m_selectionStartTime = selectionController()->dataSelectedStartTime();
    selectionController()->dataSelectedStartTimeChanged().onReceive(this, [this](trackedit::secs_t time) {
        setSelectionStartTime(time);
    });

    m_selectionEndTime = selectionController()->dataSelectedEndTime();
    selectionController()->dataSelectedEndTimeChanged().onReceive(this, [this](trackedit::secs_t time) {
        setSelectionEndTime(time);
    });

    globalContext()->currentTrackeditProjectChanged().onNotify(this, [this]() {
        onProjectChanged();
    });

    connect(this, &TimelineContext::horizontalScrollChanged, [this]() {
        m_previousHorizontalScrollPosition = startHorizontalScrollPosition();
    });

    connect(this, &TimelineContext::verticalScrollChanged, [this]() {
        m_previousVerticalScrollPosition = startVerticalScrollPosition();
    });

    connect(this, &TimelineContext::frameTimeChanged, [this]() {
        emit horizontalScrollChanged();
        emit selectionStartTimeChanged();
        emit selectionEndTimeChanged();
        emit selectionStartPositionChanged();
        emit selectionEndPositionChanged();
        if (singleClipSelected()) {
            emit selectedClipStartPositionChanged();
            emit selectedClipEndPositionChanged();
        }
    });

    dispatcher()->reg(this, "zoom-in", this, &TimelineContext::zoomIn);
    dispatcher()->reg(this, "zoom-out", this, &TimelineContext::zoomOut);
    dispatcher()->reg(this, "fit-selection", this, &TimelineContext::fitSelectionToWidth);
    dispatcher()->reg(this, "fit-project", this, &TimelineContext::fitProjectToWidth);

    onProjectChanged();

    emit horizontalScrollChanged();
    emit verticalScrollChanged();
}

void TimelineContext::onWheel(double mouseX, const QPoint& pixelDelta, const QPoint& angleDelta)
{
    QPoint pixelsScrolled = pixelDelta;
    QPoint stepsScrolled = angleDelta;

    int dx = 0;
    int dy = 0;
    qreal stepsX = 0.0;
    qreal stepsY = 0.0;

// pixelDelta is unreliable on X11
#ifdef Q_OS_LINUX
    if (std::getenv("WAYLAND_DISPLAY") == NULL) {
        // Ignore pixelsScrolled unless Wayland is used
        pixelsScrolled.setX(0);
        pixelsScrolled.setY(0);
    }
#endif

    if (!pixelsScrolled.isNull()) {
        dx = pixelsScrolled.x();
        dy = pixelsScrolled.y();
        stepsX = dx / static_cast<qreal>(PIXELSSTEPSFACTOR);
        stepsY = dy / static_cast<qreal>(PIXELSSTEPSFACTOR);
    } else if (!stepsScrolled.isNull()) {
        dx = (stepsScrolled.x() * qMax(2.0, m_frameWidth / 10.0)) / QWheelEvent::DefaultDeltasPerStep;
        dy = (stepsScrolled.y() * qMax(2.0, m_frameHeight / 10.0)) / QWheelEvent::DefaultDeltasPerStep;
        stepsX = static_cast<qreal>(stepsScrolled.x()) / static_cast<qreal>(QWheelEvent::DefaultDeltasPerStep);
        stepsY = static_cast<qreal>(stepsScrolled.y()) / static_cast<qreal>(QWheelEvent::DefaultDeltasPerStep);
    }

    Qt::KeyboardModifiers modifiers = QApplication::keyboardModifiers();

    if (modifiers.testFlag(Qt::ControlModifier)) {
        double zoomSpeed = qPow(2.0, 1.0 / configuration()->mouseZoomPrecision());
        qreal absSteps = sqrt(stepsX * stepsX + stepsY * stepsY) * (stepsY > -stepsX ? 1 : -1);
        double newZoom = zoom() * qPow(zoomSpeed, absSteps);

        setZoom(newZoom, mouseX);
    } else {
        qreal correction = 1.0 / zoom();

        if (modifiers.testFlag(Qt::ShiftModifier)) {
            int abs = sqrt(dx * dx + dy * dy) * (dy > -dx ? -1 : 1);
            shiftFrameTime(abs * correction);
        } else {
            shiftFrameTime(-dx * correction);
            emit viewContentYChangeRequested(-dy);
        }
    }
}

void TimelineContext::pinchToZoom(qreal scaleFactor, const QPointF& pos)
{
    double newZoom = zoom() * scaleFactor;
    setZoom(newZoom, pos.x());
}

void TimelineContext::scrollHorizontal(qreal newPos)
{
    TRACEFUNC;

    qreal scrollStep = newPos - m_previousHorizontalScrollPosition;
    if (qFuzzyIsNull(scrollStep)) {
        return;
    }

    qreal correction = 1.0 / zoom();
    qreal dx = horizontalScrollableSize() * scrollStep;

    shiftFrameTime(dx * correction);
}

void TimelineContext::scrollVertical(qreal newPos)
{
    TRACEFUNC;

    qreal scrollStep = newPos - m_previousVerticalScrollPosition;
    if (qFuzzyIsNull(scrollStep)) {
        return;
    }

    static constexpr qreal correction = 100.0;
    emit viewContentYChangeRequested(scrollStep * correction);
}

void TimelineContext::insureVisible(double posSec)
{
    double newPosition = timeToContentPosition(posSec);
    double frameStartPosition = timeToContentPosition(m_frameStartTime);
    double frameEndPosition = timeToContentPosition(m_frameEndTime);

    if (muse::RealIsEqualOrMore(newPosition, frameStartPosition)
        && muse::RealIsEqualOrLess(newPosition, frameEndPosition)) {
        return;
    }

    double frameTime = m_frameEndTime - m_frameStartTime;
    double newFrameTime = m_frameStartTime + (posSec - m_frameEndTime) + (frameTime / 3.0);
    moveToFrameTime(newFrameTime);
}

void TimelineContext::autoScrollView(double scrollStep)
{
    if (!muse::RealIsEqualOrMore(scrollStep, 0.0)) {
        scrollStep = 0.0;
    }

    trackedit::secs_t frameStartBeforeShift = frameStartTime();

    double frameTime = m_frameEndTime - m_frameStartTime;
    double scrollFactor = calculateScrollSpeed(m_autoScrollStep, 0, SCROLL_MARGIN_PX, SCROLL_MIN_SPEED, SCROLL_MAX_SPEED);
    moveToFrameTime(m_frameStartTime + (frameTime * scrollFactor));

    trackedit::secs_t frameStartAfterShift = frameStartTime();

    updateMousePositionTime(timeToPosition(m_mousePositionTime + (frameStartAfterShift - frameStartBeforeShift)));
    emit frameTimeChanged();
}

void TimelineContext::startAutoScroll(double posSec)
{
    if (globalContext()->playbackState()->playbackStatus() == playback::PlaybackStatus::Running
        || globalContext()->isRecording()) {
        return;
    }

    double newPosition = timeToContentPosition(posSec);
    double frameStartPosition = timeToContentPosition(m_frameStartTime);
    double frameEndPosition = timeToContentPosition(m_frameEndTime);

    if (muse::RealIsEqualOrMore(newPosition, frameStartPosition + SCROLL_MARGIN_PX)
        && muse::RealIsEqualOrLess(newPosition, frameEndPosition - SCROLL_MARGIN_PX)) {
        stopAutoScroll();
        return;
    }

    // update scroll step
    if (muse::RealIsEqualOrLess(newPosition, frameStartPosition + SCROLL_MARGIN_PX)) {
        if (muse::RealIsEqualOrLess(frameStartPosition, 0.0)) {
            stopAutoScroll();
            return;
        }

        // left view edge, m_autoScrollStep should be negative number
        m_autoScrollStep = newPosition - (frameStartPosition + SCROLL_MARGIN_PX);
    } else {
        // right view edge, m_autoScrollStep should be positive number
        m_autoScrollStep = newPosition - (frameEndPosition - SCROLL_MARGIN_PX);
    }

    if (!m_scrollTimer.isActive()) {
        m_scrollTimer.start();
    }
}

void TimelineContext::stopAutoScroll()
{
    if (m_scrollTimer.isActive()) {
        m_scrollTimer.stop();
    }
}

void TimelineContext::onResizeFrameWidth(double frameWidth)
{
    m_frameWidth = frameWidth;
    updateFrameTime();
}

void TimelineContext::onResizeFrameHeight(double frameHeight)
{
    m_frameHeight = frameHeight;
}

void TimelineContext::onResizeFrameContentHeight(double frameHeight)
{
    m_frameContentHeight = frameHeight;

    emit verticalScrollChanged();
}

void TimelineContext::moveToFrameTime(double startTime)
{
    setFrameStartTime(std::max(startTime, 0.0));
    updateFrameTime();
}

void TimelineContext::shiftFrameTime(double shift)
{
    if (muse::is_zero(shift)) {
        return;
    }

    double timeShift = shift;
    double endTimeShift = shift;

    double minStartTime = 0.0;
    double totalTime = trackEditProject()->totalTime().to_double();
    double maxEndTime = std::max(m_lastZoomEndTime, totalTime + (m_frameEndTime - m_frameStartTime) * 3 / 4);

    // do not shift to negative time values
    if (m_frameStartTime + timeShift < minStartTime) {
        if (muse::is_equal(m_frameStartTime, minStartTime)) {
            return;
        }
        //! NOTE If we haven't reached the limit yet, then it shifts as much as possible
        else {
            timeShift = minStartTime - m_frameStartTime;
        }
    }

    if (m_frameEndTime + endTimeShift > maxEndTime) {
        if (muse::is_equal(m_frameEndTime, maxEndTime)) {
            return;
        }
        //! NOTE If we haven't reached the limit yet, then it shifts as much as possible
        else {
            timeShift = std::min(timeShift, maxEndTime - m_frameEndTime);
        }
    }

    setFrameStartTime(m_frameStartTime + timeShift);
    setFrameEndTime(m_frameEndTime + timeShift);

    emit frameTimeChanged();
}

au::trackedit::ITrackeditProjectPtr TimelineContext::trackEditProject() const
{
    project::IAudacityProjectPtr prj = globalContext()->currentProject();
    return prj ? prj->trackeditProject() : nullptr;
}

IProjectViewStatePtr TimelineContext::viewState() const
{
    project::IAudacityProjectPtr prj = globalContext()->currentProject();
    return prj ? prj->viewState() : nullptr;
}

void TimelineContext::onProjectChanged()
{
    auto project = globalContext()->currentTrackeditProject();
    if (!project) {
        return;
    }

    project->timeSignatureChanged().onReceive(this, [this](const trackedit::TimeSignature&) {
        updateTimeSignature();
    });

    updateTimeSignature();
}

void TimelineContext::zoomIn()
{
    double newZoom = zoom() * 2.0;
    double zoomPosition = findZoomFocusPosition();

    setZoom(newZoom, zoomPosition);

    double centerPosition = frameCenterPosition();
    double centerTime = positionToTime(centerPosition);

    //! update values after zooming
    zoomPosition = findZoomFocusPosition();
    double zoomTime = positionToTime(zoomPosition);

    //! position center to zoom position
    shiftFrameTime(zoomTime - centerTime);
}

void TimelineContext::zoomOut()
{
    double newZoom = zoom() / 2.0;
    setZoom(newZoom, findZoomFocusPosition());
}

qreal TimelineContext::frameCenterPosition() const
{
    return m_frameWidth / 2.0;
}

qreal TimelineContext::selectionCenterPosition() const
{
    double centerTime = m_selectionStartTime + (m_selectionEndTime - m_selectionStartTime) / 2.0;
    return timeToPosition(centerTime);
}

qreal TimelineContext::findZoomFocusPosition() const
{
    double result = 0.0;

    if (!hasSelection()) {
        // No selection: zoom at the current playback position
        result = timeToPosition(globalContext()->playbackState()->playbackPosition());
    } else {
        // Selection: zoom at the center of the selection
        result = selectionCenterPosition();
    }

    return std::clamp(result, 0.0, m_frameWidth);
}

void TimelineContext::fitSelectionToWidth()
{
    if (!hasSelection()) {
        return;
    }

    double zoomPosition = findZoomFocusPosition();

    double newZoom = m_frameWidth / (m_selectionEndTime - m_selectionStartTime);
    setZoom(newZoom, zoomPosition);

    double centerPosition = frameCenterPosition();
    double centerTime = positionToTime(centerPosition);

    //! update values after zooming
    zoomPosition = selectionCenterPosition();
    double zoomTime = positionToTime(zoomPosition);

    //! position center to zoom position
    shiftFrameTime(zoomTime - centerTime);
}

void TimelineContext::fitProjectToWidth()
{
    double totalTimeRange = trackEditProject()->totalTime();
    if (muse::is_zero(totalTimeRange)) {
        return;
    }

    double newZoom = m_frameWidth / totalTimeRange;
    setZoom(newZoom, 0.0);

    //! position view to begin
    shiftFrameTime(0.0 - m_frameStartTime);
}

void TimelineContext::updateViewOnProjectTempoChange(double ratio)
{
    m_zoom *= ratio;
    emit zoomChanged();

    setFrameStartTime(m_frameStartTime / ratio);
    setFrameEndTime(m_frameEndTime / ratio);

    dispatcher()->dispatch("playback-seek", muse::actions::ActionData::make_arg1<double>(
                               globalContext()->playbackState()->playbackPosition() / ratio));
}

void TimelineContext::shiftFrameTimeOnStep(int direction)
{
    double step = 30.0 / m_zoom;
    double shift = step * direction;
    shiftFrameTime(shift);
}

void TimelineContext::updateFrameTime()
{
    setFrameEndTime(positionToTime(m_frameWidth));
    emit frameTimeChanged();
}

bool TimelineContext::hasSelection() const
{
    return !muse::RealIsEqualOrLess(m_selectionStartTime, 0.0) && !muse::RealIsEqualOrLess(m_selectionEndTime, 0.0);
}

double TimelineContext::timeToPosition(double time) const
{
    return m_zoom * (time - m_frameStartTime);
}

double TimelineContext::positionToTime(double position, bool withSnap) const
{
    double result = m_frameStartTime + position / m_zoom;

    if (withSnap) {
        result = applySnapToTime(result);
    }

    return result;
}

double TimelineContext::singleStepToTime(double position, Direction direction, const Snap& snap) const
{
    double result = m_frameStartTime + position / m_zoom;
    auto viewState = this->viewState();

    if (viewState && snap.enabled) {
        auto project = globalContext()->currentTrackeditProject();
        if (!project) {
            return 0.0;
        }

        trackedit::TimeSignature timeSig = project->timeSignature();
        result = m_snapTimeFormatter->singleStep(result, viewState->snap().val, direction, timeSig);
    }

    return result;
}

double TimelineContext::applySnapToTime(double time) const
{
    auto viewState = this->viewState();
    if (!viewState || !viewState->isSnapEnabled()) {
        return time;
    }

    auto project = globalContext()->currentTrackeditProject();
    if (!project) {
        return time;
    }

    trackedit::TimeSignature timeSig = project->timeSignature();
    return m_snapTimeFormatter->snapTime(time, viewState->snap().val, timeSig);
}

void TimelineContext::updateMousePositionTime(double mouseX)
{
    m_mousePositionTime = positionToTime(mouseX);
}

double TimelineContext::mousePositionTime() const
{
    return m_mousePositionTime;
}

double TimelineContext::zoom() const
{
    return m_zoom;
}

void TimelineContext::setZoom(double zoom, double mouseX)
{
    double newZoom = std::max(ZOOM_MIN, std::min(ZOOM_MAX, zoom));

    if (muse::RealIsEqual(m_zoom, newZoom)) {
        return;
    }

    double timeRange = m_frameEndTime - m_frameStartTime;
    double mouseTime = m_frameStartTime + (mouseX / m_frameWidth) * timeRange;

    //we limit zoom to the total time range *2, but at least 4 minutes
    double totalTimeRange = std::max(trackEditProject()->totalTime().to_double() * 2.0, 4 * 60.0);
    double newTimeRange = (m_frameStartTime + m_frameWidth / newZoom) - m_frameStartTime;

    if (!muse::is_zero(totalTimeRange) && muse::RealIsEqualOrMore(newTimeRange, totalTimeRange)) {
        newTimeRange = totalTimeRange;
        newZoom = m_frameWidth / totalTimeRange;
    }

    m_zoom = newZoom;
    emit zoomChanged();

    double newStartTime = mouseTime - (mouseX / m_frameWidth) * newTimeRange;
    setFrameStartTime(std::max(newStartTime, 0.0));

    double newEndTime = mouseTime + ((m_frameWidth - mouseX) / m_frameWidth) * newTimeRange;
    m_lastZoomEndTime = newEndTime;
    setFrameEndTime(newEndTime);

    emit verticalScrollChanged();
    emit frameTimeChanged();
}

int TimelineContext::BPM() const
{
    return m_BPM;
}

void TimelineContext::setBPM(int BPM)
{
    if (m_BPM != BPM) {
        m_BPM = BPM;
        emit BPMChanged();
    }
}

int TimelineContext::timeSigUpper() const
{
    return m_timeSigUpper;
}

void TimelineContext::setTimeSigUpper(int timeSigUpper)
{
    if (m_timeSigUpper != timeSigUpper) {
        m_timeSigUpper = timeSigUpper;
        emit timeSigUpperChanged();
    }
}

int TimelineContext::timeSigLower() const
{
    return m_timeSigLower;
}

void TimelineContext::setTimeSigLower(int timeSigLower)
{
    if (m_timeSigLower != timeSigLower) {
        m_timeSigLower = timeSigLower;
        emit timeSigLowerChanged();
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
    return m_selectionStartTime.raw();
}

void TimelineContext::setSelectionStartTime(double time)
{
    if (m_selectionStartTime != time) {
        m_selectionStartTime = time;
        emit selectionStartTimeChanged();
        emit selectionStartPositionChanged();
        updateSelectionActive();
    }
}

double TimelineContext::selectionEndTime() const
{
    return m_selectionEndTime.raw();
}

double TimelineContext::selectionStartPosition() const
{
    return timeToPosition(m_selectionStartTime);
}

double TimelineContext::selectionEndPosition() const
{
    return timeToPosition(m_selectionEndTime);
}

void TimelineContext::setSelectionEndTime(double time)
{
    if (m_selectionEndTime != time) {
        m_selectionEndTime = time;
        emit selectionEndTimeChanged();
        emit selectionEndPositionChanged();
        updateSelectionActive();
    }
}

bool TimelineContext::selectionActive() const
{
    return m_selectionActive;
}

double TimelineContext::selectedClipStartTime() const
{
    return m_selectedClipStartTime.raw();
}

double TimelineContext::selectedClipEndTime() const
{
    return m_selectedClipEndTime.raw();
}

double TimelineContext::selectedClipStartPosition() const
{
    return timeToPosition(m_selectedClipStartTime);
}

double TimelineContext::selectedClipEndPosition() const
{
    return timeToPosition(m_selectedClipEndTime);
}

bool TimelineContext::singleClipSelected() const
{
    return m_singleClipSelected;
}

void TimelineContext::updateSelectionActive()
{
    bool isActive = m_selectionStartTime >= 0.0
                    && m_selectionEndTime > 0.0
                    && m_selectionEndTime > m_selectionStartTime;

    if (m_selectionActive == isActive) {
        return;
    }
    m_selectionActive = isActive;
    emit selectionActiveChanged();
}

void TimelineContext::setClipStartTime(double time)
{
    if (m_selectedClipStartTime != time) {
        m_selectedClipStartTime = time;
        emit selectedClipStartTimeChanged();
        emit selectedClipStartPositionChanged();
    }
}

void TimelineContext::setClipEndTime(double time)
{
    if (m_selectedClipEndTime != time) {
        m_selectedClipEndTime = time;
        emit selectedClipEndTimeChanged();
        emit selectedClipEndPositionChanged();
    }
}

void TimelineContext::updateSingleClipSelected()
{
    bool selected = selectionController()->selectedClips().size() == 1;

    if (m_singleClipSelected == selected) {
        return;
    }
    m_singleClipSelected = selected;
    emit singleClipSelectedChanged();
}

void TimelineContext::updateSelectedClipTime()
{
    if (singleClipSelected()) {
        setClipStartTime(selectionController()->selectedClipStartTime());
        setClipEndTime(selectionController()->selectedClipEndTime());
    }
}

void TimelineContext::updateTimeSignature()
{
    auto project = globalContext()->currentTrackeditProject();
    if (!project) {
        return;
    }

    trackedit::TimeSignature timeSignature = project->timeSignature();

    m_timeSigUpper = timeSignature.upper;
    emit timeSigUpperChanged();

    m_timeSigLower = timeSignature.lower;
    emit timeSigLowerChanged();

    if (m_BPM != timeSignature.tempo) {
        double ratio = timeSignature.tempo / m_BPM;
        m_BPM = timeSignature.tempo;
        updateViewOnProjectTempoChange(ratio);

        emit BPMChanged();
    }

    updateFrameTime();
}

qreal TimelineContext::horizontalScrollableSize() const
{
    auto project = trackEditProject();
    if (!project) {
        return 0.0;
    }

    double maxEndTime = std::max(m_lastZoomEndTime, project->totalTime().to_double());
    return timeToContentPosition(maxEndTime);
}

qreal TimelineContext::verticalScrollableSize() const
{
    return m_frameContentHeight;
}

double TimelineContext::timeToContentPosition(double time) const
{
    return std::floor(0.5 + m_zoom * time);
}

qreal TimelineContext::startHorizontalScrollPosition() const
{
    qreal scrollableWidth = horizontalScrollableSize();
    if (qFuzzyIsNull(scrollableWidth)) {
        return 0;
    }

    double left = timeToContentPosition(m_frameStartTime);

    return left / scrollableWidth;
}

qreal TimelineContext::horizontalScrollbarSize() const
{
    qreal scrollableWidth = horizontalScrollableSize();
    if (qFuzzyIsNull(scrollableWidth)) {
        return 0;
    }

    double viewportWidth = timeToPosition(m_frameEndTime);

    return viewportWidth / scrollableWidth;
}

qreal TimelineContext::startVerticalScrollPosition() const
{
    qreal scrollableWidth = verticalScrollableSize();
    if (qFuzzyIsNull(scrollableWidth)) {
        return 0;
    }

    return m_startVerticalScrollPosition / scrollableWidth;
}

qreal TimelineContext::verticalScrollbarSize() const
{
    qreal scrollableWidth = verticalScrollableSize();
    if (qFuzzyIsNull(scrollableWidth)) {
        return 0;
    }

    return m_frameHeight / scrollableWidth;
}

void TimelineContext::setStartVerticalScrollPosition(qreal position)
{
    if (qFuzzyCompare(m_startVerticalScrollPosition, position)) {
        return;
    }

    m_startVerticalScrollPosition = position;
    emit verticalScrollChanged();
}
