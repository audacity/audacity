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
#ifndef MU_GLOBAL_ICRYPTOGRAPHICHASH_H
#define MU_GLOBAL_ICRYPTOGRAPHICHASH_H

#include "modularity/imoduleinterface.h"
#include "types/bytearray.h"

namespace mu {
class ICryptographicHash : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ICryptographicHash)
public:
    virtual ~ICryptographicHash() = default;

    enum class Algorithm {
        Md4
    };

    virtual ByteArray hash(const ByteArray& data, Algorithm alg) const = 0;
};
}

#endif // MU_GLOBAL_ICRYPTOGRAPHICHASH_H
