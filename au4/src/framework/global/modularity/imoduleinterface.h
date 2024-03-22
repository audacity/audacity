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
#ifndef MU_MODULARITY_IMODULEINTERFACE_H
#define MU_MODULARITY_IMODULEINTERFACE_H

#include "../thirdparty/kors_modularity/modularity/imoduleinterface.h" // IWYU pragma: export

namespace mu::modularity {
using IModuleExportInterface = kors::modularity::IModuleExportInterface;
using IModuleExportCreator = kors::modularity::IModuleExportCreator;

using IModuleInternalInterface = kors::modularity::IModuleInternalInterface;
using IModuleInternalCreator = kors::modularity::IModuleInternalCreator;
}

#endif // MU_MODULARITY_IMODULEINTERFACE_H
