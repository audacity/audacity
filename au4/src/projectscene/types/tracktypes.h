/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * A Digital Audio Editor
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
#ifndef AU_PROJECTSCENE_TRACKTYPES_H
#define AU_PROJECTSCENE_TRACKTYPES_H

#include "qobjectdefs.h"

namespace au::projectscene {
class TrackTypes
{
    Q_GADGET

public:
    //! NOTE: must be in sync with au::processing::TrackType
    enum class Type {
        UNDEFINED = 0,
        MONO,
        STEREO,
        LABEL
    };
    Q_ENUM(Type)
};
}

#endif // AU_PROJECTSCENE_TRACKTYPES_H
