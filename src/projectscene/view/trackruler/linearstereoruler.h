#pragma once

#include "projectscene/view/trackruler/itrackrulermodel.h"

namespace au::projectscene {
class LinearStereoRuler : public ITrackRulerModel
{
public:
    double stepToPosition(double step, int channel) const override;
    void setHeight(int height) override;
    void setChannelHeightRatio(double channelHeightRatio) override;
    void setCollapsed(bool isCollapsed) override;
    std::vector<TrackRulerFullStep> fullSteps() const override;
    std::vector<TrackRulerSmallStep> smallSteps() const override;

private:
    double m_height = 0.0;
    double m_channelHeightRatio = 0.5;
    bool m_collapsed = false;
};
}
