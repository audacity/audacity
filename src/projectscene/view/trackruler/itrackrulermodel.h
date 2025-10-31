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
    bool isNegativeSample;
};

struct TrackRulerSmallStep {
    double value;
    size_t channel;
    bool isNegativeSample;
};

class ITrackRulerModel
{
public:
    virtual ~ITrackRulerModel() = default;

    [[nodiscard]] virtual double stepToPosition(double step, size_t channel, bool isNegativeSample) const = 0;
    virtual void setHeight(int height) = 0;
    virtual void setChannelHeightRatio(double channelHeightRatio) = 0;
    virtual void setCollapsed(bool isCollapsed) = 0;
    virtual void setDbRange([[maybe_unused]] double dbRange) {}
    [[nodiscard]] virtual std::string sampleToText(double sample) const = 0;
    [[nodiscard]] virtual std::vector<TrackRulerFullStep> fullSteps() const = 0;
    [[nodiscard]] virtual std::vector<TrackRulerSmallStep> smallSteps() const = 0;
};
}
