#include "projectviewstate.h"

using namespace au::projectscene;

constexpr int DEFAULT_HEIGHT = 144;
constexpr int MIN_HEIGHT = 52;
constexpr int COLLAPSE_HEIGHT = 72;

muse::ValCh<int> ProjectViewState::tracksVericalY() const
{
    return m_tracksVericalY;
}

void ProjectViewState::changeTracksVericalY(int deltaY)
{
    m_tracksVericalY.set(deltaY);
}

ProjectViewState::TrackData& ProjectViewState::makeTrackData(const processing::TrackId& trackId) const
{
    TrackData d;
    d.height.val = DEFAULT_HEIGHT;
    d.collapsed.val = false;
    return m_tracks.insert({ trackId, d }).first->second;
}

muse::ValCh<int> ProjectViewState::trackHeight(const processing::TrackId& trackId) const
{
    auto it = m_tracks.find(trackId);
    if (it != m_tracks.end()) {
        return it->second.height;
    }

    const ProjectViewState::TrackData& d = makeTrackData(trackId);
    return d.height;
}

muse::ValCh<bool> ProjectViewState::isTrackCollapsed(const processing::TrackId& trackId) const
{
    auto it = m_tracks.find(trackId);
    if (it != m_tracks.end()) {
        return it->second.collapsed;
    }

    const ProjectViewState::TrackData& d = makeTrackData(trackId);
    return d.collapsed;
}

void ProjectViewState::changeTrackHeight(const processing::TrackId& trackId, int deltaY)
{
    TrackData* d = nullptr;
    auto it = m_tracks.find(trackId);
    if (it != m_tracks.end()) {
        d = &it->second;
    } else {
        d = &makeTrackData(trackId);
    }

    int newVal = std::max(d->height.val + deltaY, MIN_HEIGHT);
    d->height.set(newVal);
    d->collapsed.set(newVal < COLLAPSE_HEIGHT);
}
