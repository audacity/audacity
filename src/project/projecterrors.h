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
#ifndef AU_PROJECT_PROJECTERRORS_H
#define AU_PROJECT_PROJECTERRORS_H

#include "global/types/ret.h"

namespace muse::io {
struct path_t;
}

namespace au::project {
enum class Err {
    Undefined       = int(muse::Ret::Code::Undefined),
    NoError         = int(muse::Ret::Code::Ok),
    UnknownError    = int(muse::Ret::Code::ProjectFirst),

    NoProjectError,
    NoPartsError,
    CorruptionError,
    CorruptionUponOpeningError,

    AudacityExceptionError,
    DatabaseError,
    ProjectFileNotFound,
    ProjectFileIsReadProtected,
    ProjectFileIsWriteProtected,
    InvalidCloudProjectId,

    UnsupportedUrl,
    MalformedOpenProjectUrl,
};

inline muse::Ret make_ret(Err e)
{
    return muse::Ret(static_cast<int>(e));
}

muse::Ret make_ret(Err err, const muse::io::path_t& filePath);
}

#endif // AU_PROJECT_PROJECTERRORS_H
