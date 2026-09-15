/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <algorithm>
#include <iterator>
#include <optional>

#include "global/containers.h"

#include "trackedittypes.h"

namespace au::trackedit {
class TrackListChange
{
public:
    TrackListChange() = default;
    TrackListChange(TrackIdList before, TrackIdList after)
        : m_before(std::move(before)), m_after(std::move(after))
    {
    }

    const TrackIdList& before() const { return m_before; }
    const TrackIdList& after() const { return m_after; }

    TrackIdList added() const { return difference(m_after, m_before); }
    TrackIdList removed() const { return difference(m_before, m_after); }

    bool wasRemoved(const TrackId& trackId) const
    {
        return muse::contains(m_before, trackId) && !muse::contains(m_after, trackId);
    }

    std::optional<size_t> indexAfter(const TrackId& trackId) const
    {
        const auto it = std::find(m_after.begin(), m_after.end(), trackId);
        if (it == m_after.end()) {
            return std::nullopt;
        }
        return static_cast<size_t>(std::distance(m_after.begin(), it));
    }

    bool hasChanges() const
    {
        return !added().empty() || !removed().empty();
    }

private:
    static TrackIdList difference(const TrackIdList& lhs, const TrackIdList& rhs)
    {
        TrackIdList result;
        std::copy_if(lhs.begin(), lhs.end(), std::back_inserter(result),
                     [&rhs](const TrackId& trackId) { return !muse::contains(rhs, trackId); });
        return result;
    }

    TrackIdList m_before;
    TrackIdList m_after;
};
}
