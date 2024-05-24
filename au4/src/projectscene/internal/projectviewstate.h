#pragma once

#include "../iprojectviewstate.h"

namespace au::projectscene {
class ProjectViewState : public IProjectViewState
{
public:
    ProjectViewState() = default;

    muse::ValCh<int> trackHeight(const processing::TrackId& trackId) const override;

    void changeTrackHeight(const processing::TrackId& trackId, int deltaY) override;

private:

    mutable std::map<processing::TrackId, muse::ValCh<int /*height*/>> m_data;
};
}
