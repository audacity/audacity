/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "projectscene/view/trackruler/itrackrulermodel.h"

namespace au::projectscene {
class LinearMonoRuler : public ITrackRulerModel
{
public:
    [[nodiscard]] double stepToPosition(double step, size_t channel, bool isNegativeSample) const override;
    void setHeight(int height) override;
    void setChannelHeightRatio(double channelHeightRatio) override;
    void setCollapsed(bool isCollapsed) override;
    [[nodiscard]] std::string sampleToText(double sample) const override;
    [[nodiscard]] std::vector<TrackRulerFullStep> fullSteps() const override;
    [[nodiscard]] std::vector<TrackRulerSmallStep> smallSteps() const override;

private:
    double m_height = 0.0;
    double m_channelHeightRatio = 1.0;
    bool m_collapsed = false;
};
}
