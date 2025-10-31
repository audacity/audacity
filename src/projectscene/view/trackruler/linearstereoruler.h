/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "projectscene/view/trackruler/itrackrulermodel.h"

namespace au::projectscene {
class LinearStereoRuler : public ITrackRulerModel
{
public:
    double stepToPosition(double step, size_t channel, bool isNegativeSample) const override;
    void setHeight(int height) override;
    void setChannelHeightRatio(double channelHeightRatio) override;
    void setCollapsed(bool isCollapsed) override;
    std::string sampleToText(double sample) const override;
    std::vector<TrackRulerFullStep> fullSteps() const override;
    std::vector<TrackRulerSmallStep> smallSteps() const override;

private:
    double m_height = 0.0;
    double m_channelHeightRatio = 0.5;
    bool m_collapsed = false;
};
}
