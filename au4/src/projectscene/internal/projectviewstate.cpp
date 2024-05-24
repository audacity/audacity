#include "projectviewstate.h"

using namespace au::projectscene;

constexpr int DEFAULT_HEIGHT = 144;
constexpr int MIN_HEIGHT = 52;

muse::ValCh<int> ProjectViewState::trackHeight(const processing::TrackId& trackId) const
{
    auto it = m_data.find(trackId);
    if (it != m_data.end()) {
        return it->second;
    }

    muse::ValCh<int> val;
    val.val = DEFAULT_HEIGHT;
    m_data.insert({ trackId, val });

    return val;
}

void ProjectViewState::changeTrackHeight(const processing::TrackId& trackId, int deltaY)
{
    auto it = m_data.find(trackId);
    if (it == m_data.end()) {
        muse::ValCh<int> val;
        val.val = DEFAULT_HEIGHT + deltaY;
        m_data.insert({ trackId, val });
    } else {
        int newVal = std::max(it->second.val + deltaY, MIN_HEIGHT);
        it->second.set(newVal);
    }
}
