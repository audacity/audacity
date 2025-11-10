/*
* Audacity: A Digital Audio Editor
*/
#include "global/realfn.h"

#include "dblogmonoruler.h"
#include "dblogrulerutils.h"

using namespace au::projectscene;

namespace {
constexpr int MIN_ADJACENT_FULL_STEPS_HEIGHT = 17;
constexpr int MIN_ADJACENT_SMALL_STEPS_HEIGHT = 4;
const std::vector<std::pair<double, double> > STEP_INCREMENT = {
    { { 1.0 / 6.0, 1.0 / 30.0 },
        { 1.0 / 6.0, 1.0 / 12.0 },
        { 1.0 / 3.0, 1.0 / 6.0 },
        { 1.0, 1.0 / 3.0 } } };
constexpr std::pair <double, double> DEFAULT_INCREMENT = { 1.0, 1.0 / 3.0 };

int getAlignment(double value, bool isNegativeSample)
{
    if (muse::RealIsEqual(value, 0.0)) {
        return isNegativeSample ? 1 : -1;
    }
    return 0;
}
}

double DbLogMonoRuler::stepToPosition(double step, [[maybe_unused]] size_t channel, bool isNegativeSample) const
{
    return dblogrulerutils::valueToPosition(step, m_height, m_dbRange, isNegativeSample);
}

void DbLogMonoRuler::setHeight(int height)
{
    m_height = height;
}

void DbLogMonoRuler::setChannelHeightRatio(double channelHeightRatio)
{
    m_channelHeightRatio = channelHeightRatio;
}

void DbLogMonoRuler::setCollapsed(bool isCollapsed)
{
    m_collapsed = isCollapsed;
}

void DbLogMonoRuler::setDbRange(double dbRange)
{
    m_dbRange = dbRange;
}

std::string DbLogMonoRuler::sampleToText(double sample) const
{
    std::stringstream ss;
    ss << std::fixed << std::setprecision(0) << std::abs(sample);
    return ss.str();
}

std::vector<TrackRulerFullStep> DbLogMonoRuler::fullSteps() const
{
    if (m_collapsed) {
        return { TrackRulerFullStep { m_dbRange, 0, 0, true, true, false } };
    }

    std::vector<double> steps = dblogrulerutils::fullStepsValues(m_height, dblogrulerutils::StepSettings {
        m_dbRange,
        static_cast<double>(MIN_ADJACENT_FULL_STEPS_HEIGHT),
        static_cast<double>(MIN_ADJACENT_SMALL_STEPS_HEIGHT),
        DEFAULT_INCREMENT,
        STEP_INCREMENT
    });

    std::vector<TrackRulerFullStep> result;
    result.reserve((2 * steps.size()) + 1);
    for (double value : steps) {
        result.push_back(TrackRulerFullStep { value, 0, getAlignment(value, false), dblogrulerutils::isBold(value,
                                                                                                            m_dbRange), false, false });
        result.push_back(TrackRulerFullStep { value, 0, getAlignment(value, true), dblogrulerutils::isBold(value, m_dbRange), false,
                                              true });
    }
    result.push_back(TrackRulerFullStep { m_dbRange, 0, 0, true, true, false });

    return result;
}

std::vector<TrackRulerSmallStep> DbLogMonoRuler::smallSteps() const
{
    if (m_collapsed) {
        return { TrackRulerSmallStep { 0.0, 0, false }, TrackRulerSmallStep { 0.0, 0, true } };
    }

    const dblogrulerutils::StepSettings settings = {
        m_dbRange,
        static_cast<double>(MIN_ADJACENT_FULL_STEPS_HEIGHT),
        static_cast<double>(MIN_ADJACENT_SMALL_STEPS_HEIGHT),
        DEFAULT_INCREMENT,
        STEP_INCREMENT
    };

    std::vector<double> steps = dblogrulerutils::smallStepsValues(m_height, settings);
    std::vector<double> fullSteps = dblogrulerutils::fullStepsValues(m_height, settings);
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

void DbLogMonoRuler::setDisplayBounds(float min, float max)
{
    // No op
}
