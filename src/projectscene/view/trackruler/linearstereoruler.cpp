/*
* Audacity: A Digital Audio Editor
*/
#include "linearstereoruler.h"

#include "framework/global/realfn.h"

using namespace au::projectscene;

namespace {
constexpr std::array<TrackRulerFullStep, 2> COLLAPSED_FULL_STEPS = { {
    TrackRulerFullStep{ 0.0, 0, 0, true, true, false },
    TrackRulerFullStep{ 0.0, 1, 0, true, true, false },
} };

constexpr std::array<TrackRulerSmallStep, 2> COLLAPSED_SMALL_STEPS = { {
    TrackRulerSmallStep{ 1.0, 0, false },
    TrackRulerSmallStep{ -1.0, 1, false }
} };

constexpr double MIN_CHANNEL_HEIGHT = 40.0;
}

double LinearStereoRuler::stepToPosition(double step, [[maybe_unused]] size_t channel, [[maybe_unused]] bool isNegativeSample) const
{
    return channel == 0 ? valueToPosition(step, m_height * m_channelHeightRatio)
           : valueToPosition(step, m_height * (1.0 - m_channelHeightRatio)) + (m_height * m_channelHeightRatio);
}

std::vector<TrackRulerFullStep> LinearStereoRuler::fullSteps() const
{
    std::vector<TrackRulerFullStep> steps;

    if (m_collapsed) {
        steps.insert(steps.end(), COLLAPSED_FULL_STEPS.begin(), COLLAPSED_FULL_STEPS.end());
        return steps;
    }

    std::vector<double> channelHeight = { m_height* m_channelHeightRatio, m_height* (1.0 - m_channelHeightRatio) };
    for (size_t ch = 0; ch < channelHeight.size(); ++ch) {
        if (channelHeight[ch] < MIN_CHANNEL_HEIGHT) {
            steps.push_back(TrackRulerFullStep { 0.0, ch, 0, true, true, false });
            continue;
        }

        const auto values = fullStepsValues(channelHeight[ch]);
        for (double value : values) {
            steps.push_back(TrackRulerFullStep { value,
                                                 ch,
                                                 getAlignment(value),
                                                 isBold(value), value == 0.0, value < 0.0 });
        }
    }

    return steps;
}

std::vector<TrackRulerSmallStep> LinearStereoRuler::smallSteps() const
{
    std::vector<TrackRulerSmallStep> steps;
    if (m_collapsed) {
        steps.insert(steps.end(), COLLAPSED_SMALL_STEPS.begin(), COLLAPSED_SMALL_STEPS.end());
        return steps;
    }

    std::vector<double> channelHeight = { m_height* m_channelHeightRatio, m_height* (1.0 - m_channelHeightRatio) };
    for (size_t ch = 0; ch < 2; ++ch) {
        if (channelHeight[ch] < MIN_CHANNEL_HEIGHT) {
            steps.push_back(TrackRulerSmallStep { 0.0, ch, false });
            continue;
        }

        const auto values = smallStepsValues(channelHeight[ch]);
        const auto fullSteps = fullStepsValues(channelHeight[ch]);
        for (double v : values) {
            if (std::find_if(fullSteps.begin(), fullSteps.end(), [v](double fs) { return muse::RealIsEqual(v, fs); }) != fullSteps.end()) {
                continue;
            }
            steps.push_back(TrackRulerSmallStep { v, ch,  v < 0.0 });
        }
    }

    return steps;
}
