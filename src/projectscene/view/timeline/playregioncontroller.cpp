/*
* Audacity: A Digital Audio Editor
*/
#include "playregioncontroller.h"

#include <cmath>

#include <QApplication>

#include "framework/global/log.h"

namespace au::projectscene {
PlayRegionController::PlayRegionController(QObject* parent)
    : QObject(parent)
{
}

void PlayRegionController::init()
{
    globalContext()->currentProjectChanged().onNotify(this, [this]() {
        updateIsActive();
    });

    connect(qApp, &QApplication::applicationStateChanged, this, [this](Qt::ApplicationState state){
        if (state != Qt::ApplicationActive) {
            finishInteraction(m_dragStartPos);
        }
    });

    uicontextResolver()->currentUiContextChanged().onNotify(this, [this]() {
        finishInteraction(m_dragStartPos);
    });

    updateIsActive();
}

void PlayRegionController::startInteraction(double pos, bool ctrlPressed)
{
    if (!m_isActive) {
        return;
    }

    m_initialRegion = playbackController()->loopRegion();
    m_dragStartPos = calculateSnappedPosition(pos);
    m_dragStarted = false;
    m_lastPos = m_dragStartPos;
    updateSnapGuideline(m_dragStartPos);

    if (playbackController()->isLoopRegionClear()) {
        m_action = ctrlPressed ? UserInputAction::CreateRegion : UserInputAction::None;
    } else if (std::abs(startPos() - pos) < RESIZE_AREA_WIDTH_PX) {
        m_action = UserInputAction::ResizeStart;
    } else if (std::abs(endPos() - pos) < RESIZE_AREA_WIDTH_PX) {
        m_action = UserInputAction::ResizeEnd;
    } else if (pos > startPos() && pos < endPos()) {
        m_action = UserInputAction::Drag;
    }

    if (m_action == UserInputAction::CreateRegion
        || m_action == UserInputAction::ResizeStart
        || m_action == UserInputAction::ResizeEnd) {
        QGuiApplication::setOverrideCursor(QCursor(Qt::SizeHorCursor));
    } else if (m_action == UserInputAction::Drag) {
        QGuiApplication::setOverrideCursor(QCursor(Qt::ClosedHandCursor));
    }

    if (m_action != UserInputAction::None) {
        playbackController()->loopEditingBegin();
    }
}

void PlayRegionController::updatePosition(double pos)
{
    if (!m_isActive) {
        return;
    }

    if (m_action == UserInputAction::None) {
        return;
    }

    if (!m_dragStarted) {
        if (std::abs(pos - m_dragStartPos) <= MINIMUM_DRAG_LENGTH_PX) {
            return;
        }
        m_dragStarted = true;
    }

    double visibleStartPos = context()->timeToPosition(context()->frameStartTime());
    double visibleEndPos = context()->timeToPosition(context()->frameEndTime());

    pos = std::clamp(pos, visibleStartPos, visibleEndPos);

    double snappedPos = calculateSnappedPosition(pos);
    updateSnapGuideline(pos);

    switch (m_action) {
    case UserInputAction::CreateRegion:
        // Have to clear the loop if we are replacing existing
        playbackController()->clearLoopRegion();
        // Clearing the loop automatically disables it, reactivating
        playbackController()->setLoopRegionActive(true);
        setStartPos(m_dragStartPos);
        setEndPos(snappedPos);
        m_action = UserInputAction::ResizeEnd;
        break;
    case UserInputAction::ResizeStart:
        setStartPos(snappedPos);
        break;
    case UserInputAction::ResizeEnd:
        setEndPos(snappedPos);
        break;
    case UserInputAction::Drag:
        // Passing raw pos here, when dragging we snap region start or region end
        handleDrag(pos);
        break;
    case UserInputAction::None:
        break;
    }

    m_lastPos = pos;
}

void au::projectscene::PlayRegionController::handleDrag(double pos)
{
    if (!m_isActive) {
        return;
    }

    const auto& pr = m_initialRegion;
    const auto& ctx = context();
    double deltaTime = ctx->positionToTime(pos) - ctx->positionToTime(m_dragStartPos);

    // Prevent dragging before time 0
    if (muse::RealIsEqualOrLess(pr.start + deltaTime, 0.0)) {
        deltaTime = -pr.start;
    }

    const double newStartTime = pr.start + deltaTime;
    const double newEndTime = pr.end + deltaTime;

    double startPos = ctx->timeToPosition(newStartTime);
    double endPos = ctx->timeToPosition(newEndTime);

    double snappedStart = calculateSnappedPosition(startPos);
    double snappedEnd = calculateSnappedPosition(endPos);

    const double regionLength = pr.end - pr.start;

    if (!muse::is_equal(snappedStart, startPos)) {
        snappedEnd = ctx->timeToPosition(ctx->positionToTime(snappedStart) + regionLength);
        updateSnapGuideline(snappedStart);
    } else if (!muse::is_equal(snappedEnd, endPos)) {
        snappedStart = ctx->timeToPosition(ctx->positionToTime(snappedEnd) - regionLength);
        updateSnapGuideline(snappedEnd);
    } else {
        resetSnapGuideline();
    }

    playbackController()->setLoopRegion({ ctx->positionToTime(snappedStart), ctx->positionToTime(snappedEnd) });
}

void PlayRegionController::finishInteraction(double pos)
{
    UNUSED(pos);

    if (!m_isActive) {
        return;
    }

    if (m_action == UserInputAction::None) {
        return;
    }

    auto region = playbackController()->loopRegion();

    if (region.end < region.start) {
        playbackController()->setLoopRegion({ region.end, region.start });
    }

    // Toggle region active if it was a simple click without drag
    if (m_action == UserInputAction::Drag && !m_dragStarted) {
        playbackController()->toggleLoopPlayback();
    }

    QGuiApplication::restoreOverrideCursor();
    resetSnapGuideline();
    playbackController()->loopEditingEnd();
    m_action = UserInputAction::None;
}

TimelineContext* PlayRegionController::context() const
{
    return m_context;
}

double PlayRegionController::startPos() const
{
    return context()->timeToPosition(playbackController()->loopRegion().start);
}

double PlayRegionController::endPos() const
{
    return context()->timeToPosition(playbackController()->loopRegion().end);
}

void PlayRegionController::setStartPos(double pos)
{
    playbackController()->setLoopRegionStart(context()->positionToTime(pos));
}

void PlayRegionController::setEndPos(double pos)
{
    playbackController()->setLoopRegionEnd(context()->positionToTime(pos));
}

void PlayRegionController::beginPreview()
{
    m_initialRegion = playbackController()->loopRegion();
    m_initialState = playbackController()->isLoopRegionActive();
    playbackController()->setLoopRegionActive(true);
}

void PlayRegionController::setPreviewStartTime(double time)
{
    playbackController()->setLoopRegionStart(time);
}

void PlayRegionController::setPreviewEndTime(double time)
{
    playbackController()->setLoopRegionEnd(time);
}

void PlayRegionController::endPreview()
{
    playbackController()->setLoopRegion(m_initialRegion);
    playbackController()->setLoopRegionActive(m_initialState);
}

void PlayRegionController::setContext(TimelineContext* newContext)
{
    m_context = newContext;
    emit contextChanged();

    resetSnapGuideline();
}

void PlayRegionController::updateSnapGuideline(double pos)
{
    bool snapEnabled = true;
    double guideline = context()->findGuideline(context()->positionToTime(pos, snapEnabled));

    double newPos = context()->timeToPosition(guideline);
    if (muse::is_equal(newPos, m_snapGuidelinePos)) {
        return;
    }
    bool wasVisible = guidelineVisible();

    m_snapGuidelinePos = newPos;
    emit guidelinePositionChanged();

    if (wasVisible != guidelineVisible()) {
        emit guidelineVisibleChanged();
    }
}

void PlayRegionController::resetSnapGuideline()
{
    updateSnapGuideline(-1e9);
}

double PlayRegionController::calculateSnappedPosition(double pos) const
{
    bool snapEnabled = true;
    double guideline = context()->findGuideline(context()->positionToTime(pos, snapEnabled));

    if (muse::RealIsEqualOrMore(guideline, 0)) {
        return context()->timeToPosition(guideline);
    }
    return pos;
}

double PlayRegionController::guidelinePosition() const
{
    return m_snapGuidelinePos;
}

bool PlayRegionController::guidelineVisible() const
{
    return muse::RealIsEqualOrMore(m_snapGuidelinePos, 0);
}

void PlayRegionController::updateIsActive()
{
    auto currentProject = globalContext()->currentProject();
    m_isActive = currentProject != nullptr;
}
}
