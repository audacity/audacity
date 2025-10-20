#include "global/realfn.h"

#include "projectscene/view/trackruler/dblinearmonoruler.h"
#include "view/trackruler/itrackrulermodel.h"

#include <string>

using namespace au::projectscene;

namespace {
constexpr int LOW_RESOLUTION_MIN_HEIGHT = 115;

constexpr double SMALL_STEPS_RATIO = 1.0 / 6.0;
constexpr std::array<double, 2> LOW_RESOLUTION_FULL_STEPS_RATIO = { 0.0 };
constexpr std::array<double, 4> HIGH_RESOLUTION_FULL_STEPS_RATIO = { 0.0, 1.0 / 3.0, 2.0 / 3.0 };

std::vector<double> fullStepsValues(double height, double dbRange)
{
    std::vector<double> ratios;
    if (height >= LOW_RESOLUTION_MIN_HEIGHT) {
        ratios = { HIGH_RESOLUTION_FULL_STEPS_RATIO.begin(), HIGH_RESOLUTION_FULL_STEPS_RATIO.end() };
    } else {
        ratios = { LOW_RESOLUTION_FULL_STEPS_RATIO.begin(), LOW_RESOLUTION_FULL_STEPS_RATIO.end() };
    }

    std::vector<double> steps;
    steps.reserve(ratios.size());
    for (double ratio : ratios) {
        steps.push_back(ratio * dbRange);
    }
    return steps;
}

std::vector<double> smallStepsValues(double dbRange)
{
    std::vector<double> steps;
    for (double value = 0.0; value > dbRange; value += dbRange * SMALL_STEPS_RATIO) {
        steps.push_back(value);
    }
    return steps;
}
}

double DbLinearMonoRuler::stepToPosition(double step, [[maybe_unused]] size_t channel, bool isNegativeSample) const
{
    double middlePosition = m_height / 2.0;

    if (muse::RealIsEqual(step, m_dbRange)) {
        return middlePosition;
    }

    if (isNegativeSample) {
        return m_height - (middlePosition * (step / m_dbRange));
    }

    return middlePosition * (step / m_dbRange);
}

void DbLinearMonoRuler::setHeight(int height)
{
    m_height = height;
}

void DbLinearMonoRuler::setChannelHeightRatio(double channelHeightRatio)
{
    m_channelHeightRatio = channelHeightRatio;
}

void DbLinearMonoRuler::setCollapsed(bool isCollapsed)
{
    m_collapsed = isCollapsed;
}

void DbLinearMonoRuler::setDbRange(double dbRange)
{
    m_dbRange = dbRange;
}

std::string DbLinearMonoRuler::sampleToText(double sample) const
{
    std::stringstream ss;

    if (muse::RealIsEqual(sample, m_dbRange)) {
        ss << "\u221E";
    } else {
        ss << std::fixed << std::setprecision(0) << std::abs(sample);
    }

    return ss.str();
}

std::vector<TrackRulerFullStep> DbLinearMonoRuler::fullSteps() const
{
    if (m_collapsed) {
        return { TrackRulerFullStep { m_dbRange, 0, 0, true, true, false } };
    }

    const auto isBold = [this](double value) -> bool { return muse::RealIsEqual(value, 0.0) || muse::RealIsEqual(value, m_dbRange); };
    const auto getAlignment = [](double value, bool isNegativeSample) {
        if (muse::RealIsEqual(value, 0.0)) {
            return isNegativeSample ? 1 : -1;
        }
        return 0;
    };

    std::vector<double> steps = fullStepsValues(m_height, m_dbRange);
    std::vector<TrackRulerFullStep> result;
    result.reserve((2 * steps.size()) + 1);
    for (double value : steps) {
        result.push_back(TrackRulerFullStep { value, 0, getAlignment(value, false), isBold(value), false, false });
        result.push_back(TrackRulerFullStep { value, 0, getAlignment(value, true), isBold(value), false, true });
    }
    result.push_back(TrackRulerFullStep { m_dbRange, 0, 0, true, true, false });

    return result;
}

std::vector<TrackRulerSmallStep> DbLinearMonoRuler::smallSteps() const
{
    if (m_collapsed) {
        return { TrackRulerSmallStep { 0.0, 0, false }, TrackRulerSmallStep { 0.0, 0, true } };
    }

    std::vector<double> steps = smallStepsValues(m_dbRange);
    std::vector<double> fullSteps = fullStepsValues(m_height, m_dbRange);
    std::vector<TrackRulerSmallStep> result;
    for (double value : steps) {
        if (std::find(fullSteps.begin(), fullSteps.end(), value) != fullSteps.end()) {
            continue;
        }
        result.push_back(TrackRulerSmallStep { value, 0, false });
        result.push_back(TrackRulerSmallStep { value, 0, true });
    }
    return result;
}
