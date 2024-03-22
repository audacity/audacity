/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2024 MuseScore BVBA and others
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
#ifndef MU_ISYSTEMINFO_H
#define MU_ISYSTEMINFO_H

#include "modularity/imoduleinterface.h"

namespace mu {
class ISystemInfo : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ISystemInfo)
public:
    virtual ~ISystemInfo() = default;

    enum class CpuArchitecture {
        Unknown,
        Arm,
        Arm64,
        x86_64
    };

    enum class ProductType {
        Unknown,
        Windows,
        MacOS,
        Linux
    };

    virtual CpuArchitecture cpuArchitecture() const = 0;
    virtual ProductType productType() const = 0;
};
}

#endif // MU_ISYSTEMINFO_H
