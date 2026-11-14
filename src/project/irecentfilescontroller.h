/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * Music Composition & Notation
 *
 * Copyright (C) 2024 Audacity BVBA and others
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
#pragma once

#include "framework/global/modularity/imoduleinterface.h"

#include "framework/global/async/notification.h"

#include "types/projecttypes.h"

class QPixmap;

namespace au::project {
class IRecentFilesController : MODULE_GLOBAL_INTERFACE
{
    INTERFACE_ID(IRecentFilesProvider)

public:
    virtual ~IRecentFilesController() = default;

    virtual const RecentFilesList& recentFilesList() const = 0;
    virtual muse::async::Notification recentFilesListChanged() const = 0;

    virtual void prependRecentFile(const RecentFile& file) = 0;
    virtual void moveRecentFile(const muse::io::path_t& before, const RecentFile& after) = 0;
    virtual void clearRecentFiles() = 0;
};
}
