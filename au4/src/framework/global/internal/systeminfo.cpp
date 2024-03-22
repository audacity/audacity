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
#include "systeminfo.h"

#include <QSysInfo>

using namespace mu;

static const std::string CPU_ARCHITECTURE_KEY = "cpuArchitecture";
static const std::string PRODUCT_TYPE_KEY = "productType";

void SystemInfo::init()
{
    CpuArchitecture cpuArchitecture = CpuArchitecture::Unknown;
    QString arch = QSysInfo::buildCpuArchitecture();

    if (arch == "x86_64") {
        cpuArchitecture = CpuArchitecture::x86_64;
    } else if (arch == "arm") {
        cpuArchitecture = CpuArchitecture::Arm;
    } else if (arch == "arm64") {
        cpuArchitecture = CpuArchitecture::Arm64;
    }

    m_params[CPU_ARCHITECTURE_KEY] = Val(int(cpuArchitecture));

    ProductType productType = ProductType::Unknown;
#if defined(Q_OS_WIN)
    productType = ProductType::Windows;
#elif defined(Q_OS_MACOS)
    productType = ProductType::MacOS;
#elif defined(Q_OS_LINUX)
    productType = ProductType::Linux;
#endif

    m_params[PRODUCT_TYPE_KEY] = Val(int(productType));
}

SystemInfo::CpuArchitecture SystemInfo::cpuArchitecture() const
{
    return static_cast<CpuArchitecture>(m_params.at(CPU_ARCHITECTURE_KEY).toInt());
}

SystemInfo::ProductType SystemInfo::productType() const
{
    return static_cast<ProductType>(m_params.at(PRODUCT_TYPE_KEY).toInt());
}
