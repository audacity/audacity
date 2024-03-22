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
#ifndef MU_GLOBAL_RUNTIME_H
#define MU_GLOBAL_RUNTIME_H

#include <thread>
#include <sstream>

namespace mu::runtime {
inline constexpr bool isDebug()
{
#ifndef NDEBUG
    return true;
#else
    return false;
#endif
}

inline std::thread::id mainThreadId()
{
    static std::thread::id mainId = std::this_thread::get_id();
    return mainId;
}

inline std::string toString(const std::thread::id& id)
{
    std::ostringstream ss;
    ss << id;
    return ss.str();
}

void setThreadName(const std::string& name);
const std::string& threadName();
}

#endif // MU_GLOBAL_RUNTIME_H
