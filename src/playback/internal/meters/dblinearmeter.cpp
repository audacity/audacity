/*
* Audacity: A Digital Audio Editor
*/

#include "dblinearmeter.h"

#include "global/types/number.h"

#include <cmath>
#include <sstream>
#include <iomanip>
#include <algorithm>

using namespace au::playback;

namespace {
constexpr double MAX_VOLUME_DB = 0.0;
constexpr int LOW_RESOLUTION_METER_THRESHOLD = 400;

template<std::size_t TIMES, size_t N>
std::vector<double> timesArray(const std::array<double, N>& input)
{
    std::vector<double> result;
    result.reserve(N);
    for (const auto& value : input) {
        result.push_back(value * TIMES);
    }
    return result;
}

double dbToValuePercentage(double dbValue, double dbRange)
{
    double m, b;

    if (dbValue > 0) {
        return 1.0;
    } else if (dbValue > dbRange / 3.0) {
        m = (1 - 0.5) / (0 - dbRange / 3.0);
        b = 0.5 - m * (dbRange / 3.0);
        return m * dbValue + b;
    } else if (dbValue > dbRange / 2.0) {
        m = (0.5 - 0.3) / (dbRange / 3.0 - dbRange / 2.0);
        b = 0.3 - m * (dbRange / 2.0);
        return m * dbValue + b;
    } else if (dbValue > ((2.0 / 3.0) * dbRange)) {
        m = (0.3 - 0.15) / (dbRange / 2.0 - (2.0 / 3.0) * dbRange);
        b = 0.15 - m * ((2.0 / 3.0) * dbRange);
        return m * dbValue + b;
    } else if (dbValue > ((5.0 / 6.0) * dbRange)) {
        m = (0.15 - 0.075) / ((2.0 / 3.0) * dbRange - (5.0 / 6.0) * dbRange);
        b = 0.075 - m * ((5.0 / 6.0) * dbRange);
        return m * dbValue + b;
    } else if (dbValue > dbRange) {
        m = (0.075 - 0.05) / ((5.0 / 6.0) * dbRange - dbRange);
        b = 0.05 - m * dbRange;
    } else {
        return 0.0;
    }

    return m * dbValue + b;
}
}

DbLinearMeter::DbLinearMeter(int meterSize, double dbRange)
    : m_meterSize(meterSize), m_dbRange(dbRange)
{
}

void DbLinearMeter::setMeterSize(int meterSize)
{
    m_meterSize = meterSize;
}

void DbLinearMeter::setDbRange(double dbRange)
{
    m_dbRange = dbRange;
}

double DbLinearMeter::stepToPosition(double step) const
{
    const double clampedValue = std::clamp(step, m_dbRange, MAX_VOLUME_DB);
    return dbToValuePercentage(clampedValue, m_dbRange);
}

double DbLinearMeter::sampleToPosition(double sample) const
{
    return stepToPosition(sample);
}

std::string DbLinearMeter::sampleToText(double sample) const
{
    std::stringstream ss;
    ss << std::fixed << std::setprecision(0) << std::abs(sample);
    return ss.str();
}

std::vector<double> DbLinearMeter::fullSteps() const
{
    constexpr std::array<double, 7> FULL_STEP_36_DB_LOW = { -36, -18, -12, -9, -6, -3, 0 };
    constexpr std::array<double, 7> FULL_STEP_60_DB_LOW = { -60, -30, -20, -15, -10, -5, 0 };
    constexpr std::array<double, 7> FULL_STEP_48_DB_LOW = { -48, -24, -16, -12, -9, -6, 0 };
    constexpr std::array<double, 7> FULL_STEP_84_DB_LOW = { -84, -42, -28, -21, -14, -7, 0 };
    constexpr std::array<double, 7> FULL_STEP_145_DB_LOW = { -144, -72, -48, -36, -27, -18, 0 };

    constexpr std::array<double, 10> FULL_STEP_36_DB_HIGH = { -36, -30, -24, -18, -15, -12, -9, -6, -3, 0 };
    constexpr std::array<double, 10> FULL_STEP_48_DB_HIGH = { -48, -40, -32, -24, -20, -16, -12, -9, -6, 0 };
    constexpr std::array<double, 11> FULL_STEP_60_DB_HIGH = { -60, -50, -40, -30, -24, -18, -12, -9, -6, -3, 0 };
    constexpr std::array<double, 13> FULL_STEP_84_DB_HIGH = { -84, -72, -60, -48, -42, -36, -30, -24, -18, -12, -9, -6, 0 };
    constexpr std::array<double, 9> FULL_STEP_145_DB_HIGH = { -144, -96, -72, -60, -48, -36, -27, -18, 0 };

    if (m_meterSize < LOW_RESOLUTION_METER_THRESHOLD) {
        if (muse::is_equal(m_dbRange, -36.0)) {
            return std::vector<double>(std::begin(FULL_STEP_36_DB_LOW), std::end(FULL_STEP_36_DB_LOW));
        } else if (muse::is_equal(m_dbRange, -48.0)) {
            return std::vector<double>(std::begin(FULL_STEP_48_DB_LOW), std::end(FULL_STEP_48_DB_LOW));
        } else if (muse::is_equal(m_dbRange, -60.0)) {
            return std::vector<double>(std::begin(FULL_STEP_60_DB_LOW), std::end(FULL_STEP_60_DB_LOW));
        } else if (muse::is_equal(m_dbRange, -72.0)) {
            return timesArray<2>(FULL_STEP_36_DB_LOW);
        } else if (muse::is_equal(m_dbRange, -84.0)) {
            return std::vector<double>(std::begin(FULL_STEP_84_DB_LOW), std::end(FULL_STEP_84_DB_LOW));
        } else if (muse::is_equal(m_dbRange, -96.0)) {
            return timesArray<2>(FULL_STEP_48_DB_LOW);
        } else if (muse::is_equal(m_dbRange, -120.0)) {
            return timesArray<2>(FULL_STEP_60_DB_LOW);
        } else if (muse::is_equal(m_dbRange, -145.0)) {
            return std::vector<double>(std::begin(FULL_STEP_145_DB_LOW), std::end(FULL_STEP_145_DB_LOW));
        }

        return std::vector<double>(std::begin(FULL_STEP_60_DB_LOW), std::end(FULL_STEP_60_DB_LOW));
    }

    if (muse::is_equal(m_dbRange, -36.0)) {
        return std::vector<double>(std::begin(FULL_STEP_36_DB_HIGH), std::end(FULL_STEP_36_DB_HIGH));
    } else if (muse::is_equal(m_dbRange, -48.0)) {
        return std::vector<double>(std::begin(FULL_STEP_48_DB_HIGH), std::end(FULL_STEP_48_DB_HIGH));
    } else if (muse::is_equal(m_dbRange, -60.0)) {
        return std::vector<double>(std::begin(FULL_STEP_60_DB_HIGH), std::end(FULL_STEP_60_DB_HIGH));
    } else if (muse::is_equal(m_dbRange, -72.0)) {
        return timesArray<2>(FULL_STEP_36_DB_HIGH);
    } else if (muse::is_equal(m_dbRange, -84.0)) {
        return std::vector<double>(std::begin(FULL_STEP_84_DB_HIGH), std::end(FULL_STEP_84_DB_HIGH));
    } else if (muse::is_equal(m_dbRange, -96.0)) {
        return timesArray<2>(FULL_STEP_48_DB_HIGH);
    } else if (muse::is_equal(m_dbRange, -120.0)) {
        return timesArray<2>(FULL_STEP_60_DB_HIGH);
    } else if (muse::is_equal(m_dbRange, -145.0)) {
        return std::vector<double>(std::begin(FULL_STEP_145_DB_HIGH), std::end(FULL_STEP_145_DB_HIGH));
    }

    return std::vector<double>(std::begin(FULL_STEP_60_DB_HIGH), std::end(FULL_STEP_60_DB_HIGH));
}

std::vector<double> DbLinearMeter::smallSteps() const
{
    if (m_meterSize < LOW_RESOLUTION_METER_THRESHOLD) {
        return {};
    }

    constexpr std::array<double, 15> SMALL_STEP_36_DB = { -27, -21, -17, -16, -14, -13, -11, -10, -8, -7, -5, -4, -2, -1 };
    constexpr std::array<double,
                         15> SMALL_STEP_48_DB = { -36, -28, -24, -22, -18, -14, -11, -10, -8, -7, -5, -4, -3, -2, -1 };
    constexpr std::array<double,
                         17> SMALL_STEP_60_DB = { -45, -35, -28, -26, -22, -20, -16.5, -15, -13.5, -11, -10, -8, -7, -5, -4, -2, -1 };
    constexpr std::array<double, 21> SMALL_STEP_84_DB
        = { -54, -44, -39, -32, -34, -26, -28, -22, -20, -16.5, -15, -13.5, -11, -10, -8, -7, -5, -4, -3, -2, -1 };
    constexpr std::array<double,
                         15> SMALL_STEP_145_DB = { -108, -84, -72, -66, -54, -42, -33, -30, -24, -21, -15, -12, -9, -6, -3 };

    if (muse::is_equal(m_dbRange, -36.0)) {
        return std::vector<double>(SMALL_STEP_36_DB.begin(), SMALL_STEP_36_DB.end());
    } else if (muse::is_equal(m_dbRange, -48.0)) {
        return std::vector<double>(std::begin(SMALL_STEP_48_DB), std::end(SMALL_STEP_48_DB));
    } else if (muse::is_equal(m_dbRange, -60.0)) {
        return std::vector<double>(std::begin(SMALL_STEP_60_DB), std::end(SMALL_STEP_60_DB));
    } else if (muse::is_equal(m_dbRange, -72.0)) {
        return timesArray<2>(SMALL_STEP_36_DB);
    } else if (muse::is_equal(m_dbRange, -84.0)) {
        return std::vector<double>(std::begin(SMALL_STEP_84_DB), std::end(SMALL_STEP_84_DB));
    } else if (muse::is_equal(m_dbRange, -96.0)) {
        return timesArray<2>(SMALL_STEP_48_DB);
    } else if (muse::is_equal(m_dbRange, -120.0)) {
        return timesArray<2>(SMALL_STEP_60_DB);
    } else if (muse::is_equal(m_dbRange, -145.0)) {
        return std::vector<double>(std::begin(SMALL_STEP_145_DB), std::end(SMALL_STEP_145_DB));
    }

    return std::vector<double>(std::begin(SMALL_STEP_60_DB), std::end(SMALL_STEP_60_DB));
}
