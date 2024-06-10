#pragma once

#include "processing/iprocessingselectioncontroller.h"

namespace au::au3 {
class Au3SelectionController : public processing::IProcessingSelectionController
{
public:
    Au3SelectionController() = default;

    void resetDataSelection() override;

    muse::ValCh<std::vector<processing::TrackId>> dataSelectedOnTracks() const override;
    void setDataSelectedOnTracks(const std::vector<processing::TrackId>& trackIds) override;

    muse::ValCh<processing::secs_t> dataSelectedStartTime() const override;
    void setDataSelectedStartTime(const processing::secs_t time) override;
    muse::ValCh<processing::secs_t> dataSelectedEndTime() const override;
    void setDataSelectedEndTime(const processing::secs_t time) override;

private:

    muse::ValCh<std::vector<processing::TrackId>> m_selectedTrackIds;
    muse::ValCh<processing::secs_t> m_selectedStartTime;
    muse::ValCh<processing::secs_t> m_selectedEndTime;
};
}
