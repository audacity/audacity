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
        if (!globalContext()->currentProject()) {
            setTrackId({});
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
