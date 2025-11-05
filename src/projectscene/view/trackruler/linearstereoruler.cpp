/*
* Audacity: A Digital Audio Editor
*/
#include "global/realfn.h"

#include "linearstereoruler.h"
#include "linearrulerutils.h"

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
    double position;
    if (channel == 0) {
        position = (1.0 - (step / 2.0 + 0.5)) * m_height * m_channelHeightRatio;
        if (step == -1.0) {
            position += 1;
        }
    } else {
        position = (1.0 - (step / 2.0 + 0.5)) * (1.0 - m_channelHeightRatio) * m_height + m_channelHeightRatio * m_height;
        if (step == 1.0) {
            position += 1;
        }
    }

    return position;
}

void LinearStereoRuler::setHeight(int height)
{
    m_height = height;
}

void LinearStereoRuler::setChannelHeightRatio(double channelHeightRatio)
{
    m_channelHeightRatio = channelHeightRatio;
}

void LinearStereoRuler::setCollapsed(bool isCollapsed)
{
    m_collapsed = isCollapsed;
}

std::string LinearStereoRuler::sampleToText(double sample) const
{
    std::stringstream ss;
    ss << std::fixed << std::setprecision(1) << std::abs(sample);
    return ss.str();
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

        const auto values = linearrulerutils::fullStepsValues(channelHeight[ch]);
        for (double value : values) {
            steps.push_back(TrackRulerFullStep { value, ch, linearrulerutils::getAlignment(value), linearrulerutils::isBold(
                                                     value), value == 0.0, value < 0.0 });
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

        const auto values = linearrulerutils::smallStepsValues(channelHeight[ch]);
        const auto fullSteps = linearrulerutils::fullStepsValues(channelHeight[ch]);
        for (double v : values) {
            if (std::find_if(fullSteps.begin(), fullSteps.end(), [v](double fs) { return muse::RealIsEqual(v, fs); }) != fullSteps.end()) {
                continue;
            }
            steps.push_back(TrackRulerSmallStep { v, ch,  v < 0.0 });
        }
    }

    return steps;
}
