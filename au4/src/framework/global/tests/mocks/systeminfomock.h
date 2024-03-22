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
#ifndef SYSTEMINFOMOCK_H
#define SYSTEMINFOMOCK_H

#include <gmock/gmock.h>

#include "global/isysteminfo.h"

namespace mu {
class SystemInfoMock : public ISystemInfo
{
public:
    MOCK_METHOD(CpuArchitecture, cpuArchitecture, (), (const, override));
    MOCK_METHOD(ProductType, productType, (), (const, override));
};
}

#endif // SYSTEMINFOMOCK_H
