#pragma once

#include <stddef.h>
#include <vector>

namespace au::projectscene {
struct TrackRulerFullStep {
    double value;
    size_t channel;
    int alignment;
    bool isBold;
    bool fullWidthTick;
};

struct TrackRulerSmallStep {
    size_t channel;
    double value;
};

class ITrackRulerModel
{
public:
    virtual ~ITrackRulerModel() = default;

    virtual double stepToPosition(double step, int channel) const = 0;
    virtual void setHeight(int height) = 0;
    virtual void setChannelHeightRatio(double channelHeightRatio) = 0;
    virtual void setCollapsed(bool isCollapsed) = 0;
    virtual std::vector<TrackRulerFullStep> fullSteps() const = 0;
    virtual std::vector<TrackRulerSmallStep> smallSteps() const = 0;
};
}
