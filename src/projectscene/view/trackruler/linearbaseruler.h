/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "projectscene/view/trackruler/itrackruler.h"

namespace au::projectscene {
class LinearBaseRuler : public ITrackRuler
{
public:
    void setHeight(int height) override;
    void setChannelHeightRatio(double channelHeightRatio) override;
    void setCollapsed(bool isCollapsed) override;
    std::string sampleToText(double sample) const override;
    void setVerticalZoom(float verticalZoom) override;

protected:
    bool isBold(double value) const;
    int getAlignment(double value) const;
    double valueToPosition(double value, double height) const;
    int getPrecision(double step) const;
    std::vector<double> fullStepsValues(double height) const;
    std::vector<double> smallStepsValues(double height) const;

    bool m_collapsed = false;
    double m_height = 0.0;
    double m_channelHeightRatio = 1.0;
    double m_minDisplayValue = -1.0f;
    double m_maxDisplayValue = 1.0f;

private:
    std::pair<double, double> stepsIncrement(double height) const;
};
}
