/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "projectscene/view/trackruler/itrackrulermodel.h"

namespace au::projectscene {
struct DbLogRulerUiSettings {
    int minAdjacentStepsHeight;
};

class DbLogBaseRuler : public ITrackRulerModel
{
public:
    DbLogBaseRuler(DbLogRulerUiSettings settings);
    void setHeight(int height) override;
    void setChannelHeightRatio(double channelHeightRatio) override;
    void setCollapsed(bool isCollapsed) override;
    void setDbRange(double dbRange) override;
    std::string sampleToText(double sample) const override;
    void setVerticalZoom(float verticalZoom) override;

protected:
    bool isBold(double value) const;
    int getAlignment(double value) const;
    double valueToPosition(double value, double height, bool isNegativeSample) const;
    std::pair<double, double> stepsIncrement(double height) const;
    std::vector<double> fullStepsValues(double height) const;
    std::vector<double> smallStepsValues(double height) const;

    bool m_collapsed = false;
    double m_height = 0.0;
    double m_channelHeightRatio = 1.0;
    double m_dbRange = 0.0;
    double m_maxDisplayValueDB = 0.0;
    DbLogRulerUiSettings m_ui_settings;
};
}
