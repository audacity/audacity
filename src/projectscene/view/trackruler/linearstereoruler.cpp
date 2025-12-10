/*
* Audacity: A Digital Audio Editor
*/
#include "linearstereoruler.h"

#include "framework/global/realfn.h"
#include "view/trackruler/itrackruler.h"

using namespace au::projectscene;

namespace {
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
        const double middleValue = ((m_maxDisplayValue - m_minDisplayValue) / 2.0) + m_minDisplayValue;

        return { TrackRulerFullStep{ middleValue, 0, 0, IsBold::YES, IsFullWidthTick::YES, IsNegativeSample::NO },
                 TrackRulerFullStep{ middleValue, 1, 0, IsBold::YES, IsFullWidthTick::YES, IsNegativeSample::NO } };
    }

    std::vector<double> channelHeight = { m_height* m_channelHeightRatio, m_height* (1.0 - m_channelHeightRatio) };
    for (size_t ch = 0; ch < channelHeight.size(); ++ch) {
        if (channelHeight[ch] < MIN_CHANNEL_HEIGHT) {
            const double middleValue = ((m_maxDisplayValue - m_minDisplayValue) / 2.0) + m_minDisplayValue;
            steps.push_back(TrackRulerFullStep { middleValue, ch, 0, IsBold::YES, IsFullWidthTick::YES, IsNegativeSample::NO });
            continue;
        }

        const auto values = fullStepsValues(channelHeight[ch]);
        for (double value : values) {
            steps.push_back(TrackRulerFullStep { value,
                                                 ch,
                                                 getAlignment(value),
                                                 isBold(value) ? IsBold::YES : IsBold::NO,
                                                 isFullTick(value, ch) ? IsFullWidthTick::YES : IsFullWidthTick::NO,
                                                 value < 0.0 ? IsNegativeSample::YES : IsNegativeSample::NO });
        }
    }

    return steps;
}

std::vector<TrackRulerSmallStep> LinearStereoRuler::smallSteps() const
{
    std::vector<TrackRulerSmallStep> steps;
    if (m_collapsed) {
        return { TrackRulerSmallStep{ m_maxDisplayValue, 0, IsNegativeSample::NO },
                 TrackRulerSmallStep{ m_minDisplayValue, 1, IsNegativeSample::NO } };
    }

    std::vector<double> channelHeight = { m_height* m_channelHeightRatio, m_height* (1.0 - m_channelHeightRatio) };
    for (size_t ch = 0; ch < 2; ++ch) {
        if (channelHeight[ch] < MIN_CHANNEL_HEIGHT) {
            steps.push_back(TrackRulerSmallStep { 0.0, ch, IsNegativeSample::NO });
            continue;
        }

        const auto values = smallStepsValues(channelHeight[ch]);
        const auto fullSteps = fullStepsValues(channelHeight[ch]);
        for (double v : values) {
            if (std::find_if(fullSteps.begin(), fullSteps.end(), [v](double fs) { return muse::RealIsEqual(v, fs); }) != fullSteps.end()) {
                continue;
            }
            steps.push_back(TrackRulerSmallStep { v, ch, v < 0.0 ? IsNegativeSample::YES : IsNegativeSample::NO });
        }
    }

    return steps;
}

bool LinearStereoRuler::isFullTick(double value, size_t channel) const
{
    if (muse::RealIsEqual(value, 0.0)) {
        return true;
    }

    if (channel == 0 && muse::RealIsEqual(value, m_minDisplayValue)) {
        return true;
    }

    if (channel == 1 && muse::RealIsEqual(value, m_maxDisplayValue)) {
        return true;
    }

    return false;
}
