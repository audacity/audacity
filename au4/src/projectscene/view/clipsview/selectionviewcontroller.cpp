#include "selectionviewcontroller.h"

#include "log.h"

using namespace au::projectscene;
using namespace au::project;
using namespace au::processing;

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

    setSelectionActive(false);
    processingSelectionController()->resetDataSelection();
}

void SelectionViewController::onPositionChanged(double x, double y)
{
    if (!m_selectionStarted) {
        return;
    }

    // point
    QPointF p(x, y);
    emit selectionChanged(m_startPoint, p);

    // tracks
    QList<int> tracks = determinateTracks(m_startPoint.y(), y);
    setSelectedTracks(tracks);

    // time
    double x1 = m_startPoint.x();
    double x2 = x;
    if (x1 > x2) {
        std::swap(x1, x2);
    }

    m_context->setSelectionStartTime(m_context->positionToTime(x1));
    m_context->setSelectionEndTime(m_context->positionToTime(x2));
}

void SelectionViewController::onReleased(double x, double y)
{
    if (!m_selectionStarted) {
        return;
    }

    m_selectionStarted = false;

    // point

    QPointF p(x, y);

    emit selectionEnded(m_startPoint, p);

    double x1 = m_startPoint.x();
    double x2 = x;
    if (x1 > x2) {
        std::swap(x1, x2);
    }

    if ((x2 - x1) < MIN_SELECTION_PX) {
        return;
    }

    setSelectionActive(true);

    // tracks
    QList<int> tracks = determinateTracks(m_startPoint.y(), y);
    setSelectedTracks(tracks);
    std::vector<TrackId> ids = { tracks.cbegin(), tracks.cend() };
    processingSelectionController()->setDataSelectedOnTracks(ids);

    // time
    processingSelectionController()->setDataSelectedStartTime(m_context->positionToTime(x1));
    processingSelectionController()->setDataSelectedEndTime(m_context->positionToTime(x2));
}

void SelectionViewController::onSelectionDraged(double x1, double x2)
{
    // time
    if (x1 > x2) {
        std::swap(x1, x2);
    }

    m_context->setSelectionStartTime(m_context->positionToTime(x1));
    m_context->setSelectionEndTime(m_context->positionToTime(x2));
}

IProjectViewStatePtr SelectionViewController::viewState() const
{
    IAudacityProjectPtr prj = globalContext()->currentProject();
    return prj ? prj->viewState() : nullptr;
}

std::vector<TrackId> SelectionViewController::trackIdList() const
{
    ProcessingProjectPtr prj = globalContext()->currentProcessingProject();
    return prj ? prj->trackIdList() : std::vector<TrackId>();
}

QList<int> SelectionViewController::determinateTracks(double y1, double y2) const
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

    std::vector<TrackId> tracks = trackIdList();
    if (tracks.empty()) {
        return { -1, -1 };
    }

    QList<int> ret;

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

QList<int> SelectionViewController::selectedTracks() const
{
    return m_selectedTracks;
}

void SelectionViewController::setSelectedTracks(const QList<int>& newSelectedTracks)
{
    if (m_selectedTracks == newSelectedTracks) {
        return;
    }
    m_selectedTracks = newSelectedTracks;
    emit selectedTracksChanged();
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
