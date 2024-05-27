#include "projectviewstate.h"

using namespace au::projectscene;

constexpr int DEFAULT_HEIGHT = 144;
constexpr int MIN_HEIGHT = 52;

muse::ValCh<int> ProjectViewState::tracksVericalY() const
{
    return m_tracksVericalY;
}

void ProjectViewState::changeTracksVericalY(int deltaY)
{
    m_tracksVericalY.set(deltaY);
}

muse::ValCh<int> ProjectViewState::trackHeight(const processing::TrackId& trackId) const
{
    auto it = m_tracks.find(trackId);
    if (it != m_tracks.end()) {
        return it->second;
    }

    muse::ValCh<int> val;
    val.val = DEFAULT_HEIGHT;
    m_tracks.insert({ trackId, val });

    return val;
}

void ProjectViewState::changeTrackHeight(const processing::TrackId& trackId, int deltaY)
{
    auto it = m_tracks.find(trackId);
    if (it == m_tracks.end()) {
        muse::ValCh<int> val;
        val.val = DEFAULT_HEIGHT + deltaY;
        m_tracks.insert({ trackId, val });
    } else {
        int newVal = std::max(it->second.val + deltaY, MIN_HEIGHT);
        it->second.set(newVal);
    }
}
