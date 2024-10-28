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
    m_selectionStarted = true;
    m_startPoint = QPointF(x, y);
    emit selectionStarted();

    resetDataSelection();
    TrackIdList tracks = determinateTracks(y, y);
    selectionController()->setSelectedTracks(tracks, true);
}

void SelectionViewController::onPositionChanged(double x, double y)
{
    if (!m_selectionStarted) {
        return;
    }

    // point
    emit selectionChanged(m_startPoint, QPointF(x, y));

    // tracks
    TrackIdList tracks = determinateTracks(m_startPoint.y(), y);
    selectionController()->setSelectedTracks(tracks, true);

    // time
    double x1 = m_startPoint.x();
    double x2 = x;
    if (x1 > x2) {
        std::swap(x1, x2);
    }

    selectionController()->setDataSelectedStartTime(m_context->positionToTime(x1, true /*withSnap*/), false);
    selectionController()->setDataSelectedEndTime(m_context->positionToTime(x2, true /*withSnap*/), false);
}

void SelectionViewController::onReleased(double x, double y)
{
    if (!m_selectionStarted) {
        return;
    }

    m_selectionStarted = false;

    // point
    emit selectionEnded(m_startPoint, QPointF(x, y));

    double x1 = m_startPoint.x();
    double x2 = x;
    if (x1 > x2) {
        std::swap(x1, x2);
    }

    const TrackIdList tracks = determinateTracks(m_startPoint.y(), y);

    if ((x2 - x1) < MIN_SELECTION_PX) {
        // Click without drag
        if (!tracks.empty()) {
            selectionController()->setSelectedTracks(tracks);
        } else {
            selectionController()->resetSelectedTracks();
        }
        return;
    }

    setSelectionActive(true);

    if (!tracks.empty()) {
        selectionController()->setSelectedTracks(tracks);
    }
    selectionController()->setSelectedTracks(tracks, true);

    // time
    selectionController()->setDataSelectedStartTime(m_context->positionToTime(x1, true /*withSnap*/), true);
    selectionController()->setDataSelectedEndTime(m_context->positionToTime(x2, true /*withSnap*/), true);
}

void SelectionViewController::onSelectionDraged(double x1, double x2, bool completed)
{
    // time
    if (x1 > x2) {
        std::swap(x1, x2);
    }

    selectionController()->setDataSelectedStartTime(m_context->positionToTime(x1, true /*withSnap*/), completed);
    selectionController()->setDataSelectedEndTime(m_context->positionToTime(x2, true /*withSnap*/), completed);
}

void SelectionViewController::resetSelectedClip()
{
    selectionController()->resetSelectedClip();
}

void SelectionViewController::resetDataSelection()
{
    setSelectionActive(false);
    selectionController()->resetDataSelection();
}

bool SelectionViewController::isLeftSelection(double x)
{
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
