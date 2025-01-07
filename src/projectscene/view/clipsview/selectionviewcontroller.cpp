/*
* Audacity: A Digital Audio Editor
*/
#include "selectionviewcontroller.h"

#include "log.h"

using namespace au::projectscene;
using namespace au::project;
using namespace au::trackedit;

//! NOTE: sync with ClipsSelection.qml minSelection
constexpr double MIN_SELECTION_PX = 1.0;

SelectionViewController::SelectionViewController(QObject* parent)
    : QObject(parent)
{
}

void SelectionViewController::onPressed(double x, double y)
{
    if (!isProjectOpened()) {
        return;
    }

    m_lastPoint = QPointF(x, y);

    Qt::KeyboardModifiers modifiers = keyboardModifiers();

    m_selectionStarted = true;
    //! NOTE: do not update start point when user holds Shift or Ctrl
    if (!(modifiers.testFlag(Qt::ShiftModifier) || modifiers.testFlag(Qt::ControlModifier))) {
        m_startPoint = QPointF(x, y);
        selectionController()->setSelectionStartTime(m_context->positionToTime(m_startPoint.x()));
    }
    emit selectionStarted();
    resetDataSelection();

    TrackIdList tracks;
    if (modifiers.testFlag(Qt::ControlModifier)) {
        tracks = selectionController()->selectedTracks();
        TrackIdList newTracks = selectionController()->determinateTracks(y, y);
        if (!newTracks.empty()) {
            if (!muse::contains(tracks, newTracks.at(0))) {
                tracks.push_back(newTracks.at(0));
            }
        }
    } else {
        tracks = selectionController()->determinateTracks(m_startPoint.y(), y);
    }

    selectionController()->setSelectedTracks(tracks, true);

    if (modifiers.testFlag(Qt::ShiftModifier) || modifiers.testFlag(Qt::ControlModifier)) {
        double x1 = m_startPoint.x();
        double x2 = x;
        if (x1 > x2) {
            std::swap(x1, x2);
        }

        setSelectionActive(true);

        selectionController()->setDataSelectedStartTime(m_context->positionToTime(x1, true /*withSnap*/), false);
        selectionController()->setDataSelectedEndTime(m_context->positionToTime(x2, true /*withSnap*/), false);
    }

    m_autoScrollConnection = connect(m_context, &TimelineContext::frameTimeChanged, [this](){
        onPositionChanged(m_lastPoint.x(), m_lastPoint.y());
    });
}

void SelectionViewController::onPositionChanged(double x, double y)
{
    if (!isProjectOpened()) {
        return;
    }

    if (!m_selectionStarted) {
        return;
    }

    Qt::KeyboardModifiers modifiers = keyboardModifiers();

    x = std::max(x, 0.0);
    m_lastPoint = QPointF(x, y);
    m_context->startAutoScroll(m_context->positionToTime(x));

    //! NOTE: update m_startPoint in case frameTime changed
    m_startPoint.setX(m_context->timeToPosition(selectionController()->selectionStartTime()));

    // point
    emit selectionChanged(m_startPoint, QPointF(x, y));

    // tracks
    TrackIdList tracks;
    if (modifiers.testFlag(Qt::ControlModifier)) {
        tracks = selectionController()->selectedTracks();
    } else {
        tracks = selectionController()->determinateTracks(m_startPoint.y(), y);
    }
    selectionController()->setSelectedTracks(tracks, true);

    // time
    double x1 = m_startPoint.x();
    double x2 = x;
    if (x1 > x2) {
        std::swap(x1, x2);
    }

    setSelection(x1, x2, false);
}

void SelectionViewController::onReleased(double x, double y)
{
    if (!isProjectOpened()) {
        return;
    }

    if (!m_selectionStarted) {
        return;
    }

    Qt::KeyboardModifiers modifiers = keyboardModifiers();

    m_selectionStarted = false;

    x = std::max(x, 0.0);
    m_lastPoint = QPointF(x, y);
    m_context->stopAutoScroll();
    disconnect(m_autoScrollConnection);

    // point
    emit selectionEnded(m_startPoint, QPointF(x, y));

    double x1 = m_startPoint.x();
    double x2 = x;
    if (x1 > x2) {
        std::swap(x1, x2);
    }

    TrackIdList tracks;
    if (modifiers.testFlag(Qt::ControlModifier)) {
        tracks = selectionController()->selectedTracks();
    } else {
        tracks = selectionController()->determinateTracks(m_startPoint.y(), y);
    }

    if ((x2 - x1) < MIN_SELECTION_PX) {
        // Click without drag
        if (!tracks.empty()) {
            selectionController()->setSelectedTracks(tracks);
        } else {
            selectionController()->resetSelectedTracks();
        }
        setSelection(x1, x1, true);
        return;
    }

    setSelectionActive(true);

    if (!tracks.empty()) {
        selectionController()->setSelectedTracks(tracks);
    }
    selectionController()->setSelectedTracks(tracks, true);

    // time
    setSelection(x1, x2, true);
}

void SelectionViewController::onSelectionDraged(double x1, double x2, bool completed)
{
    if (!isProjectOpened()) {
        return;
    }

    // time
    if (x1 > x2) {
        std::swap(x1, x2);
    }

    setSelection(x1, x2, completed);
}

void SelectionViewController::selectTrackAudioData(double y)
{
    if (!isProjectOpened()) {
        return;
    }

    const std::vector<TrackId> tracks = selectionController()->determinateTracks(m_startPoint.y(), y);
    if (tracks.empty()) {
        return;
    }

    selectionController()->setSelectedTrackAudioData(tracks.at(0));
}

void SelectionViewController::selectClipAudioData(const ClipKey& clipKey)
{
    if (!isProjectOpened()) {
        return;
    }

    selectionController()->setSelectedClips(ClipKeyList({ clipKey.key }));
}

void SelectionViewController::resetSelectedClip()
{
    if (!isProjectOpened()) {
        return;
    }

    selectionController()->resetSelectedClips();
}

void SelectionViewController::resetDataSelection()
{
    if (!isProjectOpened()) {
        return;
    }
    setSelectionActive(false);
    selectionController()->resetDataSelection();
}

bool SelectionViewController::isLeftSelection(double x)
{
    if (!isProjectOpened()) {
        return false;
    }

    return m_startPoint.x() > x;
}

IProjectViewStatePtr SelectionViewController::viewState() const
{
    IAudacityProjectPtr prj = globalContext()->currentProject();
    return prj ? prj->viewState() : nullptr;
}

TrackIdList SelectionViewController::trackIdList() const
{
    ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    return prj ? prj->trackIdList() : TrackIdList();
}

Qt::KeyboardModifiers SelectionViewController::keyboardModifiers() const
{
    Qt::KeyboardModifiers modifiers = QApplication::keyboardModifiers();

    //! NOTE: always treat simultaneously pressed Ctrl and Shift as Ctrl
    if (modifiers.testFlag(Qt::ShiftModifier) && modifiers.testFlag(Qt::ControlModifier)) {
        modifiers = Qt::ControlModifier;
    }

    return modifiers;
}

bool SelectionViewController::isProjectOpened() const
{
    return globalContext()->currentProject() != nullptr;
}

TimelineContext* SelectionViewController::timelineContext() const
{
    return m_context;
}

void SelectionViewController::setTimelineContext(TimelineContext* newContext)
{
    if (m_context == newContext) {
        return;
    }
    m_context = newContext;
    emit timelineContextChanged();
}

bool SelectionViewController::selectionActive() const
{
    return m_selectionActive;
}

void SelectionViewController::setSelectionActive(bool newSelectionActive)
{
    if (m_selectionActive == newSelectionActive) {
        return;
    }
    m_selectionActive = newSelectionActive;
    emit selectionActiveChanged();
}

void SelectionViewController::setSelection(double x1, double x2, bool complete)
{
    selectionController()->setDataSelectedStartTime(m_context->positionToTime(x1, true /*withSnap*/), complete);
    selectionController()->setDataSelectedEndTime(m_context->positionToTime(x2, true /*withSnap*/), complete);
}
