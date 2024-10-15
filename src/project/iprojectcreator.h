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
#ifndef AU_PROJECT_IPROJECTCREATOR_H
#define AU_PROJECT_IPROJECTCREATOR_H

#include "iaudacityproject.h"

#include "modularity/imoduleinterface.h"
#include "iaudacityproject.h"

namespace au::project {
class IProjectCreator : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IProjectCreator)

public:
    virtual ~IProjectCreator() = default;

    virtual IAudacityProjectPtr newProject() const = 0;
};
}

#endif // AU_PROJECT_IPROJECTCREATOR_H
