#include "selectionviewcontroller.h"

#include "log.h"

using namespace au::projectscene;
using namespace au::project;
using namespace au::processing;

SelectionViewController::SelectionViewController(QObject* parent)
    : QObject(parent)
{
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

void SelectionViewController::onSelectedCoords(double x1, double y1, double x2, double y2)
{
    LOGDA() << "x1: " << x1 << " y1: " << y1 << " x2: " << x2 << " y2: " << y2;

    // tracks
    std::vector<TrackId> tracks = determinateTracks(y1, y2);
    processingSelectionController()->setSelectedTrackIds(tracks);

    // time
    if (x1 > x2) {
        std::swap(x1, x2);
    }

    processingSelectionController()->setSelectedStartTime(m_context->positionToTime(x1));
    processingSelectionController()->setSelectedEndTime(m_context->positionToTime(x2));
}

void SelectionViewController::resetSelection()
{
    processingSelectionController()->resetSelection();
}

std::vector<TrackId> SelectionViewController::determinateTracks(double y1, double y2) const
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

    std::vector<TrackId> ret;

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
            if (ret.back() != trackId) {
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
