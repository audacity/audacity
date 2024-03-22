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
#ifndef MU_GLOBAL_PTRUTILS_H
#define MU_GLOBAL_PTRUTILS_H

#include "runtime.h"
#include "log.h"

namespace mu::ptr {
template<typename T, typename E> T* checked_cast(E* source)
{
#ifndef NDEBUG
    T* casted = dynamic_cast<T*>(source);
    if (source && !casted) {
        Q_ASSERT_X(false, "checked_cast", "bad cast");
    }
    return casted;
#else
    return static_cast<T*>(source);
#endif
}

template<typename T, typename E> const T* checked_cast(const E* source)
{
#ifndef NDEBUG
    T* casted = dynamic_cast<T*>(source);
    if (source && !casted) {
        Q_ASSERT_X(false, "checked_cast", "bad cast");
    }
    return casted;
#else
    return static_cast<T*>(source);
#endif
}
}

#endif // MU_GLOBAL_PTRUTILS_H
