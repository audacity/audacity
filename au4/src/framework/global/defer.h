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
#ifndef MU_GLOBAL_DEFER_H
#define MU_GLOBAL_DEFER_H

#include <functional>

namespace mu {
struct Defer
{
    template<class F>
    Defer(F f)
        : m_f(f) {}

    ~Defer()
    {
        if (m_f) {
            m_f();
        }
    }

    std::function<void()> m_f;
};
}

#define CONCAT_IMPL(x, y) x##y
#define MACRO_CONCAT(x, y) CONCAT_IMPL(x, y)

#define DEFER const ::mu::Defer MACRO_CONCAT(defer_, __LINE__) = [&]()

#endif // MU_GLOBAL_DEFER_H
