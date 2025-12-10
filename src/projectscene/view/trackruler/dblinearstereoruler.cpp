/*
* Audacity: A Digital Audio Editor
*/
#include "dblinearstereoruler.h"

#include "framework/global/realfn.h"
#include "framework/global/types/ratio.h"
#include "view/trackruler/itrackruler.h"

using namespace au::projectscene;

namespace {
constexpr int MIN_FULL_STEP_TO_INF_HEIGHT = 14;
constexpr int MIN_ADJACENT_STEPS_HEIGHT = 10;
constexpr int MIN_ADJACENT_SMALL_STEPS_HEIGHT = 5;
constexpr int MIN_FULL_STEP_TO_ZERO_HEIGHT = 10;
constexpr int MIN_CHANNEL_HEIGHT = 30;
const std::vector<int> FULL_STEP_SIZES = { 1, 2, 3, 6, 9 };
}

DbLinearStereoRuler::DbLinearStereoRuler()
    : DbLinearBaseRuler(DbLinearRulerUiSettings { MIN_ADJACENT_STEPS_HEIGHT, MIN_FULL_STEP_TO_INF_HEIGHT, MIN_FULL_STEP_TO_ZERO_HEIGHT,
                                                  std::vector<int>(FULL_STEP_SIZES.begin(), FULL_STEP_SIZES.end()) })
{
}

double DbLinearStereoRuler::stepToPosition(double step, size_t channel, bool isNegativeSample) const
{
    double middlePosition = m_height * m_channelHeightRatio;

    if (channel > 1) {
        return middlePosition;
    }

    const double startPosition = channel == 0 ? 0.0 : middlePosition;
    const double endPosition = channel == 0 ? middlePosition : m_height;
    const double channelMiddleOffset = (endPosition - startPosition) / 2.0;

    if (muse::RealIsEqual(step, m_dbRange)) {
        return m_isHalfWave ? endPosition : startPosition + channelMiddleOffset;
    }

    return startPosition + valueToPosition(step, endPosition - startPosition, isNegativeSample);
}

std::vector<TrackRulerFullStep> DbLinearStereoRuler::fullSteps() const
{
    return m_isHalfWave ? fullStepsForHalfWave() : fullStepsForFullWave();
}

std::vector<TrackRulerFullStep> DbLinearStereoRuler::fullStepsForHalfWave() const
{
    const double maxDisplayValueDB = muse::linear_to_db(m_maxDisplayValue);

    if (m_collapsed) {
        return { TrackRulerFullStep { maxDisplayValueDB - 6, 0, 0, IsBold::YES, IsFullWidthTick::YES, IsNegativeSample::NO },
                 TrackRulerFullStep { maxDisplayValueDB - 6, 1, 0, IsBold::YES, IsFullWidthTick::YES, IsNegativeSample::NO } };
    }

    std::vector<double> channelHeights = { m_height* m_channelHeightRatio, m_height* (1.0 - m_channelHeightRatio) };
    std::vector<TrackRulerFullStep> steps;

    for (size_t channel = 0; channel < channelHeights.size(); ++channel) {
        if (channelHeights[channel] < MIN_CHANNEL_HEIGHT) {
            steps.push_back(TrackRulerFullStep { maxDisplayValueDB - 6, channel, 0, IsBold::YES, IsFullWidthTick::YES,
                                                 IsNegativeSample::NO });
            continue;
        }

        std::vector<TrackRulerFullStep> channelSteps { TrackRulerFullStep { m_dbRange, channel, 1, IsBold::NO, IsFullWidthTick::YES,
                                                                            IsNegativeSample::NO },
                                                       TrackRulerFullStep { maxDisplayValueDB, channel,
                                                                            -1, IsBold::YES, IsFullWidthTick::NO, IsNegativeSample::NO },
        };

        auto valuesList = fullStepValues(channelHeights[channel]);
        for (const auto& stepValue : valuesList) {
            channelSteps.push_back(TrackRulerFullStep { static_cast<double>(stepValue), channel, 0, IsBold::NO, IsFullWidthTick::NO,
                                                        IsNegativeSample::NO });
        }

        steps.insert(steps.end(), channelSteps.begin(), channelSteps.end());
    }

    return steps;
}

std::vector<TrackRulerFullStep> DbLinearStereoRuler::fullStepsForFullWave() const
{
    if (m_collapsed) {
        return { TrackRulerFullStep { m_dbRange, 0, 0, IsBold::YES, IsFullWidthTick::YES, IsNegativeSample::NO },
                 TrackRulerFullStep { m_dbRange, 1, 0, IsBold::YES, IsFullWidthTick::YES, IsNegativeSample::YES } };
    }

    const double maxDisplayValueDB = muse::linear_to_db(m_maxDisplayValue);

    std::vector<double> channelHeights = { m_height* m_channelHeightRatio, m_height* (1.0 - m_channelHeightRatio) };
    std::vector<TrackRulerFullStep> steps;

    for (size_t channel = 0; channel < channelHeights.size(); ++channel) {
        if (channelHeights[channel] < MIN_CHANNEL_HEIGHT) {
            steps.push_back(TrackRulerFullStep { m_dbRange, channel, 0, IsBold::YES, IsFullWidthTick::YES, IsNegativeSample::NO });
            continue;
        }

        std::vector<TrackRulerFullStep> channelSteps { TrackRulerFullStep { m_dbRange, channel, 0, IsBold::NO, IsFullWidthTick::YES,
                                                                            IsNegativeSample::NO },
                                                       TrackRulerFullStep { maxDisplayValueDB, channel,
                                                                            channel == 0 ? -1 : 0, IsBold::YES,
                                                                            channel == 1 ? IsFullWidthTick::YES : IsFullWidthTick::NO,
                                                                            IsNegativeSample::NO },
        };

        channelSteps.push_back(TrackRulerFullStep { maxDisplayValueDB, channel, channel == 1 ? 1 : 0, IsBold::YES,
                                                    channel == 0 ? IsFullWidthTick::YES : IsFullWidthTick::NO, IsNegativeSample::YES });

        auto valuesList = fullStepValues(channelHeights[channel]);
        for (const auto& stepValue : valuesList) {
            channelSteps.push_back(TrackRulerFullStep { static_cast<double>(stepValue), channel, 0, IsBold::NO, IsFullWidthTick::NO,
                                                        IsNegativeSample::NO });
            channelSteps.push_back(TrackRulerFullStep { static_cast<double>(stepValue), channel, 0, IsBold::NO, IsFullWidthTick::NO,
                                                        IsNegativeSample::YES });
        }

        steps.insert(steps.end(), channelSteps.begin(), channelSteps.end());
    }

    return steps;
}

std::vector<TrackRulerSmallStep> DbLinearStereoRuler::smallSteps() const
{
    const double maxDisplayValueDB = muse::linear_to_db(m_maxDisplayValue);

    if (m_collapsed) {
        return { TrackRulerSmallStep { maxDisplayValueDB, 0, IsNegativeSample::NO },
                 TrackRulerSmallStep { maxDisplayValueDB, 1, IsNegativeSample::YES } };
    }

    std::vector<double> channelHeights = { m_height* m_channelHeightRatio, m_height* (1.0 - m_channelHeightRatio) };
    std::vector<TrackRulerSmallStep> steps;
    for (size_t channel = 0; channel < 2; ++channel) {
        std::vector<int> valuesList = fullStepValues(channelHeights[channel]);
        valuesList.push_back(static_cast<int>(maxDisplayValueDB));

        std::vector<std::tuple<int, int> > fullStepRanges;
        fullStepRanges.reserve(valuesList.size() - 1);
        for (size_t i = 0; i < valuesList.size() - 1; ++i) {
            fullStepRanges.emplace_back(valuesList[i], valuesList[i + 1]);
        }

        // We ensure small steps will be shown onlu if there is enough room between full steps
        int lowestSmallStep = static_cast<int>(maxDisplayValueDB);
        for (const auto& [startValue, endValue] : fullStepRanges) {
            double firstPos = valueToPosition(startValue, channelHeights[channel], false);
            double secondPos = valueToPosition(endValue, channelHeights[channel], false);
            if (((firstPos - secondPos) / (endValue - startValue)) > MIN_ADJACENT_SMALL_STEPS_HEIGHT) {
                lowestSmallStep = startValue + 1;
                break;
            }
        }

        for (int i = lowestSmallStep; i < maxDisplayValueDB; i++) {
            if (std::find(valuesList.begin(), valuesList.end(), i) != valuesList.end()) {
                continue;
            }

            steps.push_back(TrackRulerSmallStep { static_cast<double>(i), channel, IsNegativeSample::NO });
            steps.push_back(TrackRulerSmallStep { static_cast<double>(i), channel, IsNegativeSample::YES });
        }
    }

    return steps;
}
