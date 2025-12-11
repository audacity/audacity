#pragma once

#include <cstddef>
#include <vector>

namespace au::projectscene {
enum class IsFullWidthTick : bool {
    NO = false,
    YES = true,
};

enum class IsBold : bool {
    NO = false,
    YES = true,
};

enum class IsNegativeSample : bool {
    NO = false,
    YES = true,
};

struct TrackRulerFullStep {
    double value;
    size_t channel;
    int alignment;
    IsBold isBold;
    IsFullWidthTick fullWidthTick;
    IsNegativeSample isNegativeSample;
};

struct TrackRulerSmallStep {
    double value;
    size_t channel;
    IsNegativeSample isNegativeSample;
};

class ITrackRuler
{
public:
    virtual ~ITrackRuler() = default;

    [[nodiscard]] virtual double stepToPosition(double step, size_t channel, bool isNegativeSample) const = 0;
    virtual void setHeight(int height) = 0;
    virtual void setChannelHeightRatio(double channelHeightRatio) = 0;
    virtual void setCollapsed(bool isCollapsed) = 0;
    virtual void setDbRange([[maybe_unused]] double dbRange) {}
    [[nodiscard]] virtual std::string sampleToText(double sample) const = 0;
    [[nodiscard]] virtual std::vector<TrackRulerFullStep> fullSteps() const = 0;
    [[nodiscard]] virtual std::vector<TrackRulerSmallStep> smallSteps() const = 0;
    virtual void setDisplayBounds(std::pair<float, float> displayBounds) = 0;
};
}
