/*
* Audacity: A Digital Audio Editor
*/
#include "framework/global/realfn.h"

#include "linearbaseruler.h"

using namespace au::projectscene;

namespace {
constexpr std::array<std::pair<double, double>, 4> STEP_INCREMENT = { { { 0.1, 0.05 }, { 0.5, 0.1 }, { 0.5, 0.25 }, { 1.0, 0.5 } } };
constexpr std::pair<double, double> DEFAULT_INCREMENT = { 1.0, 0.5 };
constexpr int MIN_ADJACENT_FULL_STEPS_HEIGHT = 20;
constexpr int MIN_ADJACENT_SMALL_STEPS_HEIGHT = 10;

constexpr double MAX_VALUE = 1.0;
constexpr double MIN_VALUE = -1.0;

constexpr int MAX_DECIMAL_PRECISION = 4;
}

void LinearBaseRuler::setHeight(int height)
{
    m_height = height;
}

void LinearBaseRuler::setChannelHeightRatio(double channelHeightRatio)
{
    m_channelHeightRatio = channelHeightRatio;
}

void LinearBaseRuler::setCollapsed(bool isCollapsed)
{
    m_collapsed = isCollapsed;
}

void LinearBaseRuler::setVerticalZoom(float verticalZoom)
{
    m_minDisplayValue = -verticalZoom;
    m_maxDisplayValue = verticalZoom;
}

std::string LinearBaseRuler::sampleToText(double sample) const
{
    constexpr int MIN_PRECISION = 1;
    constexpr int MAX_PRECISION = 4;

    const auto increments = stepsIncrement(m_height);
    int precision = getPrecision(increments.first);
    double multiplier = std::pow(10.0, precision);
    double rounded = std::round(std::abs(sample) * multiplier) / multiplier;

    precision = std::clamp(precision, MIN_PRECISION, MAX_PRECISION);

    std::stringstream ss;
    ss << std::fixed << std::setprecision(precision) << rounded;
    return ss.str();
}

bool LinearBaseRuler::isBold(double value) const
{
    return muse::RealIsEqual(value, m_maxDisplayValue) || muse::RealIsEqual(value, m_minDisplayValue) || muse::RealIsEqual(value, 0.0);
}

int LinearBaseRuler::getAlignment(double value) const
{
    if (muse::RealIsEqual(value, m_maxDisplayValue)) {
        return -1;
    }

    if (muse::RealIsEqual(value, m_minDisplayValue)) {
        return 1;
    }

    return 0;
}

int LinearBaseRuler::getPrecision(double step) const
{
    int precision = 0;
    double temp = step;

    while (precision < MAX_DECIMAL_PRECISION) {
        double shifted = temp * std::pow(10.0, precision);
        double fractional_part = shifted - std::floor(shifted);

        if (muse::RealIsEqual(fractional_part, 0.0)) {
            break;
        }

        precision++;
    }

    return precision;
}

double LinearBaseRuler::valueToPosition(double value, double height) const
{
    double newValue = std::clamp(value, static_cast<double>(m_minDisplayValue), static_cast<double>(m_maxDisplayValue));
    double ret = (0.5 - newValue / (m_maxDisplayValue - m_minDisplayValue)) * height;
    return ret;
}

std::pair<double, double> LinearBaseRuler::stepsIncrement(double height) const
{
    double factor = (m_maxDisplayValue - m_minDisplayValue) / (MAX_VALUE - MIN_VALUE);
    std::pair<double, double> increment = { DEFAULT_INCREMENT.first * factor, DEFAULT_INCREMENT.second * factor };
    for (const auto& stepInc : STEP_INCREMENT) {
        const auto& [fs, ss] = stepInc;
        if (fs * factor < (10 ^ (-MAX_DECIMAL_PRECISION))) {
            continue;
        }

        if ((valueToPosition(0.0, height) - valueToPosition(fs * factor, height) >= MIN_ADJACENT_FULL_STEPS_HEIGHT)
            && (valueToPosition(0.0, height) - valueToPosition(ss * factor, height) >= MIN_ADJACENT_SMALL_STEPS_HEIGHT)) {
            increment = { fs* factor, ss* factor };
            break;
        }
    }

    return increment;
}

std::vector<double> LinearBaseRuler::fullStepsValues(double height) const
{
    assert(m_minDisplayValue <= m_maxDisplayValue);

    if (muse::RealIsEqual(m_minDisplayValue, m_maxDisplayValue)) {
        return { m_minDisplayValue };
    }

    const std::pair<double, double> increment = stepsIncrement(height);
    std::vector<double> steps;
    for (double v = m_minDisplayValue; v <= m_maxDisplayValue; v += increment.first) {
        steps.push_back(v);
    }
    return steps;
}

std::vector<double> LinearBaseRuler::smallStepsValues(double height) const
{
    assert(m_minDisplayValue <= m_maxDisplayValue);

    if (muse::RealIsEqual(m_minDisplayValue, m_maxDisplayValue)) {
        return { m_minDisplayValue };
    }

    const std::pair<double, double> increment = stepsIncrement(height);
    std::vector<double> steps;
    for (double v = m_minDisplayValue; v <= m_maxDisplayValue; v += increment.second) {
        steps.push_back(v);
    }
    return steps;
}
