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
#ifndef MU_GLOBAL_IGLOBALCONFIGURATION_H
#define MU_GLOBAL_IGLOBALCONFIGURATION_H

#include "modularity/imoduleinterface.h"
#include "io/path.h"

namespace mu {
class IGlobalConfiguration : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IGlobalConfiguration)
public:

    virtual ~IGlobalConfiguration() = default;

    //! NOTE Each platform has its own paths and rules
    //! More details about paths: https://github.com/musescore/MuseScore/wiki/Resources

    //! //! NOTE The path to the executable file
    virtual io::path_t appBinPath() const = 0;

    //! NOTE The path to the dir with the executable file (probably readonly, probably private for a user)
    //! Like: programs/MuseScore/bin
    virtual io::path_t appBinDirPath() const = 0;

    //! NOTE The path to the dir with the app data files (probably readonly, probably private for a user)
    //! Like: programs/MuseScore/share
    virtual io::path_t appDataPath() const = 0;

    //! NOTE The path to the dir with the app configure files (must be writable, probably private for a user)
    //! Like: user/config/MuseScore
    virtual io::path_t appConfigPath() const = 0;

    //! NOTE The path to the dir with the app user data files (must be writable, probably private for a user)
    //! Like: user/appdata/MuseScore
    virtual io::path_t userAppDataPath() const = 0;

    //! NOTE The path to the dir with the user backup files (must be writable, probably private for a user)
    //! Like: user/appdata/MuseScore/backups
    virtual io::path_t userBackupPath() const = 0;

    //! NOTE The path to the dir with the user data files (must be writable, probably public for a user)
    //! Like: user/documents/MuseScore
    virtual io::path_t userDataPath() const = 0;

    //! NOTE System paths
    virtual io::path_t homePath() const = 0;
    virtual io::path_t downloadsPath() const = 0;
    virtual io::path_t genericDataPath() const = 0;

    virtual bool useFactorySettings() const = 0;
    virtual bool enableExperimental() const = 0;

    virtual bool devModeEnabled() const = 0;
    virtual void setDevModeEnabled(bool enabled) = 0;

    virtual bool metricUnit() const = 0;
    virtual void setMetricUnit(bool metricUnit) = 0;

    virtual std::string museScoreUrl() const = 0;
};
}

#endif // MU_GLOBAL_IGLOBALCONFIGURATION_H
