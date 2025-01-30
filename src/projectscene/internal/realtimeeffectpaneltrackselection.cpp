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
        if (project) {
            setupCallbacks(*project);
            if (!m_trackId.has_value()) {
                const std::vector<trackedit::TrackId> list = project->trackIdList();
                if (!list.empty()) {
                    trackId = list.front();
                }
            }
        }
        setTrackId(trackId);
    });
    if (const trackedit::ITrackeditProjectPtr project = globalContext()->currentTrackeditProject()) {
        setupCallbacks(*project);
    }
}

void RealtimeEffectPanelTrackSelection::setupCallbacks(trackedit::ITrackeditProject& project)
{
    project.trackAdded().onReceive(this, [this](trackedit::Track track) { onTrackAdded(track); });
    project.trackRemoved().onReceive(this, [this](trackedit::Track track) { onTrackRemoved(track.id); });
    project.tracksChanged().onReceive(this, [this](std::vector<trackedit::Track> tracks) {
        onTracksChanged(tracks);
    });
}

void RealtimeEffectPanelTrackSelection::onTrackAdded(const trackedit::Track& track)
{
    if (m_trackId.has_value()) {
        return;
    }
    setTrackId(track.id);
}

void RealtimeEffectPanelTrackSelection::onTrackRemoved(const trackedit::TrackId& trackId)
{
    const trackedit::ITrackeditProjectPtr project = globalContext()->currentTrackeditProject();
    IF_ASSERT_FAILED(project) {
        return;
    }
    const std::vector<trackedit::TrackId> tracks = project->trackIdList();
    if (tracks.empty()) {
        setTrackId(std::nullopt);
    } else if (m_trackId == trackId) {
        setTrackId(tracks.front());
    }
}

void RealtimeEffectPanelTrackSelection::onTracksChanged(const std::vector<trackedit::Track>& tracks)
{
    if (tracks.empty()) {
        setTrackId(std::nullopt);
    } else if (!m_trackId.has_value()) {
        setTrackId(tracks.front().id);
    }
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

    // Rule of thumb: no track is selected for the realtime-effect panel if and only if the project is empty.
    assert(
        m_trackId.has_value() || !globalContext()->currentTrackeditProject()
        || globalContext()->currentTrackeditProject()->trackIdList().empty());

    m_selectedTrackIdChanged.notify();
}
}
