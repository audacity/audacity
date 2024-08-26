#include "timelinecontext.h"

#include <QApplication>
#include <QWheelEvent>

#include "global/types/number.h"

#include "snaptimeformatter.h"

#include "log.h"

static constexpr double ZOOM_MIN = 0.001;
static constexpr double ZOOM_MAX = 6000000.0;
static constexpr int PIXELSSTEPSFACTOR = 5;

using namespace au::projectscene;

TimelineContext::TimelineContext(QObject* parent)
    : QObject(parent)
{
    m_snapTimeFormatter = std::make_shared<SnapTimeFormatter>();
}

void TimelineContext::init(double frameWidth)
{
    m_zoom = configuration()->zoom();
    emit zoomChanged();

    m_frameWidth = frameWidth;
    m_frameStartTime = 0.0;
    emit frameStartTimeChanged();
    m_frameEndTime = positionToTime(frameWidth);

    m_lastZoomEndTime = m_frameEndTime;

    emit frameEndTimeChanged();
    emit frameTimeChanged();

    m_selecitonStartTime = selectionController()->dataSelectedStartTime();
    selectionController()->dataSelectedStartTimeChanged().onReceive(this, [this](trackedit::secs_t time) {
        setSelectionStartTime(time);
    });

    m_selectionEndTime = selectionController()->dataSelectedEndTime();
    selectionController()->dataSelectedEndTimeChanged().onReceive(this, [this](trackedit::secs_t time) {
        setSelectionEndTime(time);
    });

    globalContext()->currentTrackeditProjectChanged().onNotify(this, [this](){
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
    });

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
            emit shiftViewByY(dy* correction);
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
    emit viewContentYChangeRequested(scrollStep* correction);
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
    setFrameStartTime(startTime);
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
    double maxEndTime = std::max(m_lastZoomEndTime, trackEditProject()->totalTime().to_double());

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

double TimelineContext::timeToPosition(double time) const
{
    double p = 0.5 + m_zoom * (time - m_frameStartTime);
    p = std::floor(p);
    return p;
}

double TimelineContext::positionToTime(double position, bool withSnap) const
{
    double result = m_frameStartTime + position / m_zoom;

    if (withSnap) {
        auto viewState = this->viewState();
        if (viewState && viewState->isSnapEnabled()) {
            auto project = globalContext()->currentTrackeditProject();
            if (!project) {
                return 0.0;
            }

            trackedit::TimeSignature timeSig = project->timeSignature();
            result = m_snapTimeFormatter->snapTime(result, viewState->snap().val, timeSig);
        }
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

double TimelineContext::zoom() const
{
    return m_zoom;
}

void TimelineContext::setZoom(double zoom, double mouseX)
{
    zoom = std::max(ZOOM_MIN, std::min(ZOOM_MAX, zoom));

    if (m_zoom != zoom) {
        m_zoom = zoom;
        emit zoomChanged();

        double timeRange = m_frameEndTime - m_frameStartTime;
        double mouseTime = m_frameStartTime + (mouseX / m_frameWidth) * timeRange;
        double newTimeRange = positionToTime(m_frameWidth) - m_frameStartTime;

        double newStartTime = mouseTime - (mouseX / m_frameWidth) * newTimeRange;
        setFrameStartTime(std::max(newStartTime, 0.0));

        double newEndTime = mouseTime + ((m_frameWidth - mouseX) / m_frameWidth) * newTimeRange;
        m_lastZoomEndTime = newEndTime;
        setFrameEndTime(newEndTime);

        emit verticalScrollChanged();
        emit frameTimeChanged();
    }
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
    return m_selecitonStartTime.raw();
}

void TimelineContext::setSelectionStartTime(double time)
{
    if (m_selecitonStartTime != time) {
        m_selecitonStartTime = time;
        emit selectionStartTimeChanged();
        updateSelectionActive();
    }
}

double TimelineContext::selectionEndTime() const
{
    return m_selectionEndTime.raw();
}

void TimelineContext::setSelectionEndTime(double time)
{
    if (m_selectionEndTime != time) {
        m_selectionEndTime = time;
        emit selectionEndTimeChanged();
        updateSelectionActive();
    }
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

    m_BPM = timeSignature.tempo;
    emit BPMChanged();

    updateFrameTime();
}

qreal TimelineContext::horizontalScrollableSize() const
{
    double maxEndTime = std::max(m_lastZoomEndTime, trackEditProject()->totalTime().to_double());
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
