/*
* Audacity: A Digital Audio Editor
*/
#include <iomanip>

#include "framework/global/types/ratio.h"
#include "framework/global/realfn.h"

#include "dblinearbaseruler.h"

using namespace au::projectscene;

DbLinearBaseRuler::DbLinearBaseRuler(DbLinearRulerUiSettings settings)
    : m_ui_settings(std::move(settings))
{
}

void DbLinearBaseRuler::setHeight(int height)
{
    m_height = height;
}

void DbLinearBaseRuler::setChannelHeightRatio(double channelHeightRatio)
{
    m_channelHeightRatio = channelHeightRatio;
}

void DbLinearBaseRuler::setCollapsed(bool isCollapsed)
{
    m_collapsed = isCollapsed;
}

void DbLinearBaseRuler::setDisplayBounds(std::pair<float, float> displayBounds)
{
    m_minDisplayValue = displayBounds.first;
    m_maxDisplayValue = displayBounds.second;

    m_isHalfWave = muse::RealIsEqual(m_minDisplayValue, 0.0);
}

void DbLinearBaseRuler::setDbRange(double dbRange)
{
    m_dbRange = dbRange;
}

std::string DbLinearBaseRuler::sampleToText(double sample) const
{
    std::stringstream ss;

    if (muse::RealIsEqual(sample, m_dbRange)) {
        ss << "\u221E";
    } else {
        ss << std::fixed << std::setprecision(0) << std::abs(sample);
    }

    return ss.str();
}

int DbLinearBaseRuler::computeLowestFullStepValue(double height) const
{
    // We prefer to show only multiple of 3 values as full steps
    constexpr int FULL_STEP_INCREMENT = 3;

    auto const middlePoint = height / ((m_maxDisplayValue - m_minDisplayValue) / m_maxDisplayValue);
    int lowestFullStep = static_cast<int>(m_dbRange);

    // Find the closer value to dbRange that has enough room to be drawn
    for (int i = lowestFullStep + FULL_STEP_INCREMENT; i < 0; i += FULL_STEP_INCREMENT) {
        const double position = valueToPosition(i, height, false);
        if ((middlePoint - position > m_ui_settings.minFullStepToInfHeight) && (position > m_ui_settings.minFullStepToZeroHeight)) {
            return i;
        }
    }

    return 0;
}

double DbLinearBaseRuler::valueToPosition(double value, double height, bool isNegativeSample) const
{
    const double linearValue = muse::db_to_linear(value) * (isNegativeSample ? -1.0 : 1.0);
    const double clampedLinearValue = std::clamp(linearValue, m_minDisplayValue, m_maxDisplayValue);
    const double percentage = (clampedLinearValue - m_minDisplayValue) / (m_maxDisplayValue - m_minDisplayValue);
    return (1 - percentage) * height;
}

std::vector<int> DbLinearBaseRuler::fullStepValues(double height) const
{
    const int lowestFullStep = computeLowestFullStepValue(height);
    const double maxDisplayValueDB = muse::linear_to_db(m_maxDisplayValue);

    std::vector<int> steps;
    if (lowestFullStep != 0) {
        steps.push_back(lowestFullStep);
        int previousValidStep = lowestFullStep;

        for (int step = lowestFullStep + 1; step < maxDisplayValueDB; step++) {
            const int diff = step - previousValidStep;
            if (std::find(m_ui_settings.fullStepSizes.begin(), m_ui_settings.fullStepSizes.end(),
                          diff) == m_ui_settings.fullStepSizes.end()) {
                continue;
            }

            const double position = valueToPosition(step, height, false);
            if (position < m_ui_settings.minFullStepToZeroHeight) {
                break;
            }

            const double previousPosition = valueToPosition(previousValidStep, height, false);
            if (previousPosition - position >= m_ui_settings.minAdjacentStepsHeight) {
                steps.push_back(step);
                previousValidStep = step;
            }
        }
    }

    return steps;
}
