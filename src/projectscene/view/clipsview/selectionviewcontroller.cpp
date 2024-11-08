/*
* Audacity: A Digital Audio Editor
*/
#include "selectionviewcontroller.h"

#include "log.h"

using namespace au::projectscene;
using namespace au::project;
using namespace au::trackedit;

constexpr double MIN_SELECTION_PX = 12.0;

SelectionViewController::SelectionViewController(QObject* parent)
    : QObject(parent)
{
}

void SelectionViewController::onPressed(double x, double y)
{
    if (!isProjectOpened()) {
        return;
    }

    Qt::KeyboardModifiers modifiers = keyboardModifiers();

    m_selectionStarted = true;
    //! NOTE: do not update start point when user holds Shift or Ctrl
    if (!(modifiers.testFlag(Qt::ShiftModifier) || modifiers.testFlag(Qt::ControlModifier))) {
        m_startPoint = QPointF(x, y);
    }
    emit selectionStarted();
    resetDataSelection();

    TrackIdList tracks;
    if (modifiers.testFlag(Qt::ControlModifier)) {
        tracks = selectionController()->selectedTracks();
        TrackIdList newTracks = determinateTracks(y, y);
        if (!newTracks.empty()) {
            if (!muse::contains(tracks, newTracks.at(0))) {
                tracks.push_back(newTracks.at(0));
            }
        }
    } else {
        tracks = determinateTracks(m_startPoint.y(), y);
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

    // point
    emit selectionChanged(m_startPoint, QPointF(x, y));

    // tracks
    TrackIdList tracks;
    if (modifiers.testFlag(Qt::ControlModifier)) {
        tracks = selectionController()->selectedTracks();
    } else {
        tracks = determinateTracks(m_startPoint.y(), y);
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
        tracks = determinateTracks(m_startPoint.y(), y);
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

    const std::vector<TrackId> tracks = determinateTracks(m_startPoint.y(), y);
    selectionController()->setSelectedTrackAudioData(tracks.at(0));
}

void SelectionViewController::selectClipAudioData(const ClipKey& clipKey)
{
    if (!isProjectOpened()) {
        return;
    }

    selectionController()->setSelectedClip(clipKey.key);
}

void SelectionViewController::resetSelectedClip()
{
    if (!isProjectOpened()) {
        return;
    }

    selectionController()->resetSelectedClip();
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

TrackIdList SelectionViewController::determinateTracks(double y1, double y2) const
{
    IProjectViewStatePtr vs = viewState();
    if (!vs) {
        return { -1, -1 };
    }

    if (y1 < 0 && y2 < 0) {
        return { -1, -1 };
    }

    if (y1 > y2) {
        std::swap(y1, y2);
    }

    if (y1 < 1) {
        y1 = 1;
    }

    TrackIdList tracks = trackIdList();
    if (tracks.empty()) {
        return { -1, -1 };
    }

    TrackIdList ret;

    int tracksVericalY = vs->tracksVericalY().val;
    int trackTop = -tracksVericalY;
    int trackBottom = trackTop;

    for (TrackId trackId : tracks) {
        trackTop = trackBottom;
        trackBottom = trackTop + vs->trackHeight(trackId).val;

        if (y1 > trackTop && y1 < trackBottom) {
            ret.push_back(trackId);
        }

        if (y2 > trackTop && y2 < trackBottom) {
            if (!ret.empty() && ret.back() != trackId) {
                ret.push_back(trackId);
            }
            break;
        }

        if (!ret.empty() && ret.back() != trackId) {
            ret.push_back(trackId);
            continue;
        }
    }

    return ret;
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
