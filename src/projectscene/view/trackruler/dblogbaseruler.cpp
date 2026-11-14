/*
* Audacity: A Digital Audio Editor
*/
#include <iomanip>

#include "framework/global/realfn.h"
#include "framework/global/types/ratio.h"

#include "dblogbaseruler.h"

using namespace au::projectscene;

DbLogBaseRuler::DbLogBaseRuler(DbLogRulerUiSettings settings)
    : m_ui_settings(std::move(settings))
{
}

void DbLogBaseRuler::setHeight(int height)
{
    m_height = height;
}

void DbLogBaseRuler::setChannelHeightRatio(double channelHeightRatio)
{
    m_channelHeightRatio = channelHeightRatio;
}

void DbLogBaseRuler::setCollapsed(bool isCollapsed)
{
    m_collapsed = isCollapsed;
}

void DbLogBaseRuler::setDbRange(double dbRange)
{
    m_dbRange = dbRange;
}

std::string DbLogBaseRuler::sampleToText(double sample) const
{
    std::stringstream ss;
    ss << std::fixed << std::setprecision(0) << std::abs(sample);
    return ss.str();
}

void DbLogBaseRuler::setDisplayBounds(std::pair<float, float> displayBounds)
{
    m_maxDisplayValueDB = std::round(muse::linear_to_db(displayBounds.second));
    m_isHalfWave = muse::RealIsEqual(displayBounds.first, 0.0f);
}

bool DbLogBaseRuler::isBold(double value) const
{
    return (muse::RealIsEqual(value, m_maxDisplayValueDB)) || (muse::RealIsEqual(value, m_dbRange));
}

double DbLogBaseRuler::valueToPosition(double value, double height, bool isNegativeSample) const
{
    double middlePosition = height / (m_isHalfWave ? 1.0 : 2.0);

    if (muse::RealIsEqual(value, m_dbRange)) {
        return middlePosition;
    }

    value = std::min(value, m_maxDisplayValueDB);
    const double percentage = (value - m_maxDisplayValueDB) / (m_dbRange - m_maxDisplayValueDB);

    if (isNegativeSample) {
        return height - (middlePosition * percentage);
    }

    return middlePosition * percentage;
}

std::pair<double, double> DbLogBaseRuler::stepsIncrement(double height) const
{
    std::pair<double, double> increment = { m_maxDisplayValueDB - m_dbRange,
                                            (m_maxDisplayValueDB - m_dbRange) / 3.0 };

    int diff = static_cast<int>(m_maxDisplayValueDB - m_dbRange);
    for (int i = 3; i < -m_dbRange - m_maxDisplayValueDB; i += 3) {
        if (diff % i != 0) {
            continue;
        }

        if ((valueToPosition(-i + m_maxDisplayValueDB, height, false)
             - valueToPosition(m_maxDisplayValueDB, height, false) >= m_ui_settings.minAdjacentStepsHeight)) {
            increment = { i, i / 3.0 };
            break;
        }
    }

    return increment;
}

std::vector<double> DbLogBaseRuler::fullStepsValues(double height) const
{
    const std::pair<double, double> increment = stepsIncrement(height);

    std::vector<double> steps;
    for (double v = m_dbRange; v <= m_maxDisplayValueDB; v += increment.first) {
        steps.push_back(v);
    }
    return steps;
}

std::vector<double> DbLogBaseRuler::smallStepsValues(double height) const
{
    const std::pair<double, double> increment = stepsIncrement(height);

    std::vector<double> steps;
    for (double v = m_dbRange; v <= m_maxDisplayValueDB; v += increment.second) {
        steps.push_back(v);
    }
    return steps;
}
