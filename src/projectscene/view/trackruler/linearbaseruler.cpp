/*
* Audacity: A Digital Audio Editor
*/
#include "linearbaseruler.h"

#include "framework/global/realfn.h"

using namespace au::projectscene;

namespace {
constexpr std::array<std::pair<double, double>, 4> STEP_INCREMENT = { { { 0.1, 0.05 }, { 0.5, 0.1 }, { 0.5, 0.25 }, { 1.0, 0.5 } } };
constexpr std::pair<double, double> DEFAULT_INCREMENT = { 1.0, 0.5 };
constexpr int MIN_ADJACENT_FULL_STEPS_HEIGHT = 20;
constexpr int MIN_ADJACENT_SMALL_STEPS_HEIGHT = 10;

constexpr double MAX_VALUE = 1.0;
constexpr double MIN_VALUE = -1.0;

constexpr int MAX_DISPLAY_DECIMAL_PRECISION = 4;
constexpr int MAX_DECIMAL_PRECISION = 10;
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

void LinearBaseRuler::setDisplayBounds(std::pair<float, float> displayBounds)
{
    m_minDisplayValue = displayBounds.first;
    m_maxDisplayValue = displayBounds.second;
}

std::string LinearBaseRuler::sampleToText(double sample) const
{
    constexpr int MIN_PRECISION = 1;
    constexpr int MAX_PRECISION = 4;

    const auto increments = stepsIncrement(m_height);
    const int precision = std::clamp(getPrecision(increments.first), MIN_PRECISION, MAX_PRECISION);
    const double rounded = muse::RealRound(std::abs(sample), precision);

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

    while (precision < MAX_DISPLAY_DECIMAL_PRECISION) {
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
    const double newValue = std::clamp(value, static_cast<double>(m_minDisplayValue), static_cast<double>(m_maxDisplayValue));
    const double percentage = (newValue - m_minDisplayValue) / (m_maxDisplayValue - m_minDisplayValue);
    return (1 - percentage) * height;
}

std::pair<double, double> LinearBaseRuler::stepsIncrement(double height) const
{
    double factor = (m_maxDisplayValue - m_minDisplayValue) / (MAX_VALUE - MIN_VALUE);
    std::pair<double, double> increment = { DEFAULT_INCREMENT.first * factor, DEFAULT_INCREMENT.second * factor };
    for (const auto& stepInc : STEP_INCREMENT) {
        const auto& [fs, ss] = stepInc;
        if (fs * factor < (10 ^ (-MAX_DISPLAY_DECIMAL_PRECISION))) {
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
    for (double value = m_minDisplayValue; value <= m_maxDisplayValue + increment.first * 0.5; value += increment.first) {
        steps.push_back(muse::RealRound(value, MAX_DECIMAL_PRECISION));
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
    for (double v = m_minDisplayValue; v <= m_maxDisplayValue + increment.second * 0.5; v += increment.second) {
        steps.push_back(muse::RealRound(v, MAX_DECIMAL_PRECISION));
    }

    return steps;
}
