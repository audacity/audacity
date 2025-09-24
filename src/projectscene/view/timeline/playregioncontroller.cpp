/*
* Audacity: A Digital Audio Editor
*/

#include "playregioncontroller.h"

#include <cmath>

#include "log.h"

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

    updateIsActive();
}

void PlayRegionController::mouseDown(double pos)
{
    if (!m_isActive) {
        return;
    }

    m_initialRegion = playback()->player()->loopRegion();
    m_dragStartPos = calculateSnappedPosition(pos);
    m_dragStarted = false;
    m_lastPos = m_dragStartPos;
    updateSnapGuideline(m_dragStartPos);

    if (playback()->player()->isLoopRegionClear()) {
        m_action = UserInputAction::CreateRegion;
        QGuiApplication::setOverrideCursor(QCursor(Qt::SizeHorCursor));
    } else if (std::abs(startPos() - pos) < RESIZE_AREA_WIDTH_PX) {
        m_action = UserInputAction::ResizeStart;
        QGuiApplication::setOverrideCursor(QCursor(Qt::SizeHorCursor));
    } else if (std::abs(endPos() - pos) < RESIZE_AREA_WIDTH_PX) {
        m_action = UserInputAction::ResizeEnd;
        QGuiApplication::setOverrideCursor(QCursor(Qt::SizeHorCursor));
    } else if (pos > startPos() && pos < endPos()) {
        m_action = UserInputAction::Drag;
        QGuiApplication::setOverrideCursor(QCursor(Qt::ClosedHandCursor));
    } else {
        m_action = UserInputAction::CreateRegion;
        QGuiApplication::setOverrideCursor(QCursor(Qt::SizeHorCursor));
    }

    if (m_action != UserInputAction::None) {
        playback()->player()->loopEditingBegin();
    }

    connect(qApp, &QApplication::applicationStateChanged, this, [this](Qt::ApplicationState state){
        if (state != Qt::ApplicationActive) {
            mouseUp(m_dragStartPos);
        }
    });

    uicontextResolver()->currentUiContextChanged().onNotify(this, [this]() {
        mouseUp(m_dragStartPos);
    });
}

void PlayRegionController::mouseMove(double pos)
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

    auto player = playback()->player();
    player->setLoopRegionActive(true);

    double visibleStartPos = context()->timeToPosition(context()->frameStartTime());
    double visibleEndPos = context()->timeToPosition(context()->frameEndTime());

    pos = std::clamp(pos, visibleStartPos, visibleEndPos);

    double snappedPos = calculateSnappedPosition(pos);
    updateSnapGuideline(pos);

    switch (m_action) {
    case UserInputAction::CreateRegion:
        // Have to clear the loop if we are replacing existing
        player->clearLoopRegion();
        // Clearing the loop automatically disables it, reactivating
        player->setLoopRegionActive(true);
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

    auto player = playback()->player();
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

    player->setLoopRegion({ ctx->positionToTime(snappedStart), ctx->positionToTime(snappedEnd) });
}

void PlayRegionController::mouseUp(double pos)
{
    UNUSED(pos);

    if (!m_isActive) {
        return;
    }

    auto player = playback()->player();
    auto region = player->loopRegion();

    if (region.end < region.start) {
        player->setLoopRegion({ region.end, region.start });
    }

    // Toggle region active if it was a simple click without drag
    if (m_action == UserInputAction::Drag && !m_dragStarted) {
        player->setLoopRegionActive(!player->isLoopRegionActive());
    }

    QGuiApplication::restoreOverrideCursor();
    resetSnapGuideline();
    player->loopEditingEnd();
    m_action = UserInputAction::None;
}

TimelineContext* PlayRegionController::context() const
{
    return m_context;
}

double PlayRegionController::startPos() const
{
    return context()->timeToPosition(playback()->player()->loopRegion().start);
}

double PlayRegionController::endPos() const
{
    return context()->timeToPosition(playback()->player()->loopRegion().end);
}

void PlayRegionController::setStartPos(double pos)
{
    playback()->player()->setLoopRegionStart(context()->positionToTime(pos));
}

void PlayRegionController::setEndPos(double pos)
{
    playback()->player()->setLoopRegionEnd(context()->positionToTime(pos));
}

void PlayRegionController::beginPreview()
{
    m_initialRegion = playback()->player()->loopRegion();
    m_initialState = playback()->player()->isLoopRegionActive();
    playback()->player()->setLoopRegionActive(true);
}

void PlayRegionController::setPreviewStartTime(double time)
{
    playback()->player()->setLoopRegionStart(time);
}

void PlayRegionController::setPreviewEndTime(double time)
{
    playback()->player()->setLoopRegionEnd(time);
}

void PlayRegionController::endPreview()
{
    playback()->player()->setLoopRegion(m_initialRegion);
    playback()->player()->setLoopRegionActive(m_initialState);
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
