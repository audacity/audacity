/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2023 MuseScore BVBA and others
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

#include "interpolation.h"

#include "log.h"

std::vector<mu::Interpolation::Point> mu::Interpolation::quadraticBezierCurve(const Point& p0, const Point& p1, const Point& p2,
                                                                              std::size_t N)
{
    IF_ASSERT_FAILED(N != 0) {
        return {};
    }

    std::vector<Point> interpolatedPoints;

    for (size_t i = 0; i <= N; ++i) {
        double t = static_cast<double>(i) / N;

        double one_minus_t = 1.0 - t;
        double one_minus_t2 = one_minus_t * one_minus_t;
        double t2 = t * t;

        // P = (1 - t)^2 * P0 + 2(1 - t) * t * P1 + t^2 * P2
        double x = one_minus_t2 * p0.x + 2 * one_minus_t * t * p1.x + t2 * p2.x;
        double y = one_minus_t2 * p0.y + 2 * one_minus_t * t * p1.y + t2 * p2.y;

        interpolatedPoints.emplace_back(Point { x, y });
    }

    return interpolatedPoints;
}
