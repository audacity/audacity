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
#ifndef MU_GLOBAL_GLOBALCONFIGURATION_H
#define MU_GLOBAL_GLOBALCONFIGURATION_H

#include "../iglobalconfiguration.h"

namespace mu {
class GlobalConfiguration : public IGlobalConfiguration
{
public:
    GlobalConfiguration() = default;

    void init();

    io::path_t appBinPath() const override;
    io::path_t appBinDirPath() const override;
    io::path_t appDataPath() const override;
    io::path_t appConfigPath() const override;

    io::path_t userAppDataPath() const override;
    io::path_t userBackupPath() const override;
    io::path_t userDataPath() const override;

    io::path_t homePath() const override;
    io::path_t downloadsPath() const override;

    bool useFactorySettings() const override;
    bool enableExperimental() const override;
    io::path_t genericDataPath() const override;

    bool devModeEnabled() const override;
    void setDevModeEnabled(bool enabled) override;

    bool metricUnit() const override;
    void setMetricUnit(bool metricUnit) override;

    std::string museScoreUrl() const override;

private:
    QString resolveAppDataPath() const;
    QString resolveUserAppDataPath() const;

    mutable io::path_t m_appDataPath;
    mutable io::path_t m_userAppDataPath;
};
}

#endif // MU_GLOBAL_GLOBALCONFIGURATION_H
