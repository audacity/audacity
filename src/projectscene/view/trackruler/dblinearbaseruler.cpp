/*
* Audacity: A Digital Audio Editor
*/

#include "global/types/ratio.h"
#include "global/realfn.h"

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

void DbLinearBaseRuler::setVerticalZoom(float verticalZoom)
{
    m_maxDisplayValue = muse::linear_to_db(verticalZoom);
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
    auto const middlePoint = height / 2.0;
    int lowestFullStep = static_cast<int>(m_dbRange);
    for (int i = lowestFullStep + 3; i < 0; i += 3) {
        const double position = valueToPosition(i, height, false);
        if (middlePoint - position > m_ui_settings.minFullStepToInfHeight) {
            return i;
        }
    }

    return 0;
}

double DbLinearBaseRuler::valueToPosition(double value, double height, bool isNegativeSample) const
{
    const double maxLinearValue = std::abs(muse::db_to_linear(m_maxDisplayValue));
    const double linearValue = muse::db_to_linear(value) * (isNegativeSample ? -1.0 : 1.0);
    const double clampedLinearValue = std::clamp(linearValue, -maxLinearValue, maxLinearValue);

    return (0.5 - clampedLinearValue / (2 * maxLinearValue)) * height;
}

std::vector<int> DbLinearBaseRuler::fullStepValues(double height) const
{
    const int lowestFullStep = computeLowestFullStepValue(height);

    std::vector<int> steps;
    if (lowestFullStep != 0) {
        steps.push_back(lowestFullStep);
        int previousValidStep = lowestFullStep;

        for (int step = lowestFullStep + 1; step < m_maxDisplayValue; step++) {
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
