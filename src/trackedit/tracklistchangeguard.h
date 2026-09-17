/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "itrackeditproject.h"
#include "tracklistchange.h"

namespace au::trackedit {
class TrackListChangeGuard
{
public:
    explicit TrackListChangeGuard(ITrackeditProjectPtr project)
        : m_project(std::move(project)), m_before(m_project ? m_project->trackIdList() : TrackIdList {})
    {
    }

    ~TrackListChangeGuard()
    {
        if (!m_project) {
            return;
        }

        m_project->notifyAboutTrackListChanged(TrackListChange(m_before, m_project->trackIdList()));
    }

    TrackListChangeGuard(const TrackListChangeGuard&) = delete;
    TrackListChangeGuard& operator=(const TrackListChangeGuard&) = delete;
    TrackListChangeGuard(TrackListChangeGuard&&) = delete;
    TrackListChangeGuard& operator=(TrackListChangeGuard&&) = delete;

private:
    const ITrackeditProjectPtr m_project;
    const TrackIdList m_before;
};
}
