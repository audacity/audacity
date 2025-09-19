/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * Music Composition & Notation
 *
 * Copyright (C) 2024 Audacity Limited
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
#ifndef AU_PROJECT_IPROJECTAUTOSAVER_H
#define AU_PROJECT_IPROJECTAUTOSAVER_H

#include "io/path.h"

#include "modularity/imoduleinterface.h"
#include "iaudacityproject.h"

namespace au::project {
// we don't use at the moment 01/09/2025 the project auto saver as we already have the autosave table
class IProjectAutoSaver : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IProjectAutoSaver)

public:
    virtual ~IProjectAutoSaver() = default;
};
}

#endif // AU_PROJECT_IPROJECTAUTOSAVER_H
