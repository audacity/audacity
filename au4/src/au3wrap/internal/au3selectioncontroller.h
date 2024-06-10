#pragma once

#include "processing/iprocessingselectioncontroller.h"

namespace au::au3 {
class Au3SelectionController : public processing::IProcessingSelectionController
{
public:
    Au3SelectionController() = default;

    void resetSelection() override;

    muse::ValCh<std::vector<processing::TrackId>> selectedTrackIds() const override;
    void setSelectedTrackIds(const std::vector<processing::TrackId>& trackIds) override;

    muse::ValCh<processing::secs_t> selectedStartTime() const override;
    void setSelectedStartTime(const processing::secs_t time) override;
    muse::ValCh<processing::secs_t> selectedEndTime() const override;
    void setSelectedEndTime(const processing::secs_t time) override;

private:

    muse::ValCh<std::vector<processing::TrackId>> m_selectedTrackIds;
    muse::ValCh<processing::secs_t> m_selectedStartTime;
    muse::ValCh<processing::secs_t> m_selectedEndTime;
};
}
