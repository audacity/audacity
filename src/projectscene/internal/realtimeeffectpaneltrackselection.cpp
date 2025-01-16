#include "realtimeeffectpaneltrackselection.h"

namespace au::projectscene {
void RealtimeEffectPanelTrackSelection::init()
{
    selectionController()->tracksSelected().onReceive(this, [this](const au::trackedit::TrackIdList& tracks) {
        if (tracks.empty()) {
            setTrackId(std::nullopt);
        } else {
            setTrackId(tracks.back());
        }
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
