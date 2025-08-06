#pragma once

#include <stddef.h>
#include <vector>

namespace au::projectscene {
struct TrackRulerFullStep {
    int alignment;
    double value;
    size_t channel;
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

    virtual double stepToPosition(double step, int channel, double height) const = 0;
    virtual std::vector<TrackRulerFullStep> fullSteps() const = 0;
    virtual std::vector<TrackRulerSmallStep> smallSteps() const = 0;
    virtual std::vector<TrackRulerFullStep> collapsedFullSteps() const = 0;
    virtual std::vector<TrackRulerSmallStep> collapsedSmallSteps() const = 0;
};
}
