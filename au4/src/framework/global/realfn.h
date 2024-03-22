/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#ifndef MU_GLOBAL_REALFN_H
#define MU_GLOBAL_REALFN_H

#include <cmath>
#include <algorithm>
#include <vector>

namespace mu {
// default
constexpr double COMPARE_DOUBLE_EPSILON(1000000000.0);
constexpr double COMPARE_DOUBLE_NULL(0.000000001);
constexpr float COMPARE_FLOAT_EPSILON(1000000.0);
constexpr float COMPARE_FLOAT_NULL(0.000001F);

// use
inline double _compare_double_epsilon(COMPARE_DOUBLE_EPSILON);
inline double _compare_double_null(COMPARE_DOUBLE_NULL);
inline double _compare_float_epsilon(COMPARE_FLOAT_EPSILON);
inline double _compare_float_null(COMPARE_FLOAT_NULL);

inline int _pow10(int power)
{
    int result = 1;
    for (int i = 0; i < power; ++i) {
        result *= 10;
    }
    return result;
}

inline void SetCompareDoublePrecision(int prec)
{
    _compare_double_epsilon = _pow10(prec);
    _compare_double_null = 1.0 / _compare_double_epsilon;
}

inline void SetDefaultCompareDoublePrecision()
{
    _compare_double_epsilon = COMPARE_DOUBLE_EPSILON;
    _compare_double_null = COMPARE_DOUBLE_NULL;
}

inline void SetCompareFloatPrecision(int prec)
{
    _compare_float_epsilon = _pow10(prec);
    _compare_float_null = 1.0 / _compare_float_epsilon;
}

inline void SetDefaultCompareFloatPrecision()
{
    _compare_float_epsilon = COMPARE_FLOAT_EPSILON;
    _compare_float_null = COMPARE_FLOAT_NULL;
}

// both double and float
inline void SetCompareRealPrecision(int prec)
{
    SetCompareDoublePrecision(prec);
    SetCompareFloatPrecision(prec);
}

inline void SetDefaultCompareRealPrecision()
{
    SetDefaultCompareDoublePrecision();
    SetDefaultCompareFloatPrecision();
}

inline bool RealIsEqual(double p1, double p2)
{
    return std::abs(p1 - p2) * _compare_double_epsilon <= std::min(std::abs(p1), std::abs(p2));
}

inline bool RealIsEqual(float p1, float p2)
{
    return std::fabs(p1 - p2) * _compare_float_epsilon <= std::min(std::fabs(p1), std::fabs(p2));
}

inline bool RealIsEqual(const std::vector<double>& v1, const std::vector<double>& v2)
{
    if (v1.size() != v2.size()) {
        return false;
    }

    for (size_t i = 0; i < v1.size(); ++i) {
        if (!RealIsEqual(v1.at(i), v2.at(i))) {
            return false;
        }
    }

    return true;
}

inline bool RealIsEqual(const std::vector<float>& v1, const std::vector<float>& v2)
{
    if (v1.size() != v2.size()) {
        return false;
    }

    for (size_t i = 0; i < v1.size(); ++i) {
        if (!RealIsEqual(v1.at(i), v2.at(i))) {
            return false;
        }
    }

    return true;
}

inline bool RealIsEqualOrMore(double p1, double p2)
{
    return p1 > p2 || RealIsEqual(p1, p2);
}

inline bool RealIsEqualOrLess(double p1, double p2)
{
    return p1 < p2 || RealIsEqual(p1, p2);
}

inline bool RealIsEqualOrMore(float p1, float p2)
{
    return p1 > p2 || RealIsEqual(p1, p2);
}

inline bool RealIsEqualOrLess(float p1, float p2)
{
    return p1 < p2 || RealIsEqual(p1, p2);
}

inline bool RealIsNull(double d)
{
    return std::abs(d) <= _compare_double_null;
}

inline bool RealIsNull(float d)
{
    return std::fabs(d) <= _compare_float_null;
}

inline double RealRound(double value, int prec)
{
    int round = _pow10(prec);
    return std::floor(value * round + 0.5) / round;
}

inline double RealRound(float value, int prec)
{
    return RealRound(static_cast<double>(value), prec);
}

inline double RealFloor(double value, int prec)
{
    int round = _pow10(prec);
    return std::floor(value * round) / round;
}

inline double RealFloor(float value, int prec)
{
    return RealRound(static_cast<double>(value), prec);
}
}

#endif // MU_GLOBAL_REALFN_H
