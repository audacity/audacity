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
#ifndef MU_MODULARITY_IOC_H
#define MU_MODULARITY_IOC_H

#include "../thirdparty/kors_modularity/modularity/ioc.h" // IWYU pragma: export

namespace mu {
template<class I>
using Inject = kors::modularity::Inject<I>;
}

namespace mu::modularity {
using ModulesIoC = kors::modularity::ModulesIoC;

template<class T>
using Creator = kors::modularity::Creator<T>;

inline ModulesIoC* ioc()
{
    return kors::modularity::ModulesIoC::instance();
}
}

#endif // MU_MODULARITY_IOC_H
