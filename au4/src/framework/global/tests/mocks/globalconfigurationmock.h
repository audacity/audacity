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
#ifndef MU_GLOBAL_GLOBALCONFIGURATIONMOCK_H
#define MU_GLOBAL_GLOBALCONFIGURATIONMOCK_H

#include <gmock/gmock.h>

#include "iglobalconfiguration.h"

namespace mu {
class GlobalConfigurationMock : public IGlobalConfiguration
{
public:
    MOCK_METHOD(io::path_t, appBinPath, (), (const, override));
    MOCK_METHOD(io::path_t, appBinDirPath, (), (const, override));
    MOCK_METHOD(io::path_t, appDataPath, (), (const, override));
    MOCK_METHOD(io::path_t, appConfigPath, (), (const, override));
    MOCK_METHOD(io::path_t, userAppDataPath, (), (const, override));
    MOCK_METHOD(io::path_t, userBackupPath, (), (const, override));
    MOCK_METHOD(io::path_t, userDataPath, (), (const, override));
    MOCK_METHOD(io::path_t, homePath, (), (const, override));
    MOCK_METHOD(io::path_t, downloadsPath, (), (const, override));
    MOCK_METHOD(io::path_t, genericDataPath, (), (const, override));

    MOCK_METHOD(bool, useFactorySettings, (), (const, override));
    MOCK_METHOD(bool, enableExperimental, (), (const, override));

    MOCK_METHOD(bool, devModeEnabled, (), (const, override));
    MOCK_METHOD(void, setDevModeEnabled, (bool), (override));

    MOCK_METHOD(bool, metricUnit, (), (const, override));
    MOCK_METHOD(void, setMetricUnit, (bool), (override));

    MOCK_METHOD(std::string, museScoreUrl, (), (const, override));
};
}

#endif // MU_GLOBAL_GLOBALCONFIGURATIONMOCK_H
