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
#ifndef MU_GLOBAL_INTERPOLATION_H
#define MU_GLOBAL_INTERPOLATION_H

#include <vector>

namespace mu {
class Interpolation
{
public:
    struct Point {
        double x = 0.0;
        double y = 0.0;
    };

    static std::vector<Point> quadraticBezierCurve(const Point& p0, const Point& p1, const Point& p2, std::size_t N);
};
}

#endif // MU_GLOBAL_INTERPOLATION_H
