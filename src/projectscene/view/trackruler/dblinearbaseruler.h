/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "projectscene/view/trackruler/itrackruler.h"

namespace au::projectscene {
struct DbLinearRulerUiSettings {
    int minAdjacentStepsHeight;
    int minFullStepToInfHeight;
    int minFullStepToZeroHeight;
    std::vector<int> fullStepSizes;
};

class DbLinearBaseRuler : public ITrackRuler
{
public:
    DbLinearBaseRuler(DbLinearRulerUiSettings config);

    void setHeight(int height) override;
    void setChannelHeightRatio(double channelHeightRatio) override;
    void setCollapsed(bool isCollapsed) override;
    void setDbRange(double dbRange) override;
    std::string sampleToText(double sample) const override;
    void setDisplayBounds(std::pair<float, float> displayBounds) override;

protected:
    int computeLowestFullStepValue(double height) const;
    double valueToPosition(double value, double height, bool isNegativeSample) const;
    std::vector<int> fullStepValues(double height) const;

    bool m_collapsed = false;
    double m_height = 0.0;
    double m_channelHeightRatio = 1.0;
    double m_dbRange = -60.0;
    double m_maxDisplayValue = 1.0;
    double m_minDisplayValue = -1.0;
    bool m_isHalfWave = false;
    DbLinearRulerUiSettings m_ui_settings;
};
}
