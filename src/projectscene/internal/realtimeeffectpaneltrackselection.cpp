#include "realtimeeffectpaneltrackselection.h"

namespace au::projectscene {
void RealtimeEffectPanelTrackSelection::init()
{
    selectionController()->tracksSelected().onReceive(this, [this](const au::trackedit::TrackIdList& tracks) {
        if (!tracks.empty()) {
            setTrackId(tracks.back());
        }
    });
    globalContext()->currentTrackeditProjectChanged().onNotify(this, [this] {
        // Show effects of top-most track if there isn't one selected already and the project isn't empty.
        const trackedit::ITrackeditProjectPtr project = globalContext()->currentTrackeditProject();
        std::optional<trackedit::TrackId> trackId;
        if (project && !m_trackId.has_value()) {
            const std::vector<trackedit::TrackId> list = project->trackIdList();
            if (!list.empty()) {
                trackId = list.front();
            }
        }
        setTrackId(trackId);
    });
}

std::optional<au::trackedit::TrackId> RealtimeEffectPanelTrackSelection::selectedTrackId() const
{
    return m_trackId;
}

muse::async::Notification RealtimeEffectPanelTrackSelection::selectedTrackIdChanged() const
{
    return m_selectedTrackIdChanged;
}

void RealtimeEffectPanelTrackSelection::setTrackId(std::optional<au::trackedit::TrackId> trackId)
{
    if (m_trackId == trackId) {
        return;
    }
    m_trackId = trackId;
    m_selectedTrackIdChanged.notify();
}
}
