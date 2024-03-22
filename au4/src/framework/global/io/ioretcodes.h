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
#ifndef MU_IO_IORETCODES_H
#define MU_IO_IORETCODES_H

#include "global/types/ret.h"
#include "global/translation.h"

namespace mu::io {
enum class Err {
    Undefined       = int(Ret::Code::Undefined),
    NoError         = int(Ret::Code::Ok),
    UnknownError    = int(Ret::Code::SystemFirst),

    FSNotExist,
    FSAlreadyExists,
    FSRemoveError,
    FSDirNotEmptyError,
    FSClearError,
    FSReadError,
    FSWriteError,
    FSMakingError,
    FSCopyError,
    FSMoveErrors
};

inline Ret make_ret(Err e)
{
    int retCode = static_cast<int>(e);

    switch (e) {
    case Err::Undefined: return Ret(retCode);
    case Err::NoError: return Ret(retCode);
    case Err::UnknownError: return Ret(retCode);
    case Err::FSNotExist: return Ret(retCode, trc("system", "The file does not exist"));
    case Err::FSAlreadyExists: return Ret(retCode, trc("system", "The file already exists"));
    case Err::FSRemoveError: return Ret(retCode, trc("system", "The file could not be removed"));
    case Err::FSDirNotEmptyError: return Ret(retCode, trc("system", "The directory is not empty"));
    case Err::FSClearError: return Ret(retCode, trc("system", "The directory could not be cleared"));
    case Err::FSReadError: return Ret(retCode, trc("system", "An error occurred when reading from the file"));
    case Err::FSWriteError: return Ret(retCode, trc("system", "An error occurred when writing to the file"));
    case Err::FSMakingError: return Ret(retCode, trc("system", "An error occurred when making a path"));
    case Err::FSCopyError: return Ret(retCode, trc("system", "An error occurred when copying the file"));
    case Err::FSMoveErrors: return Ret(retCode, trc("system", "An error occurred when moving the file"));
    }

    return Ret(static_cast<int>(e));
}
}

#endif // MU_IO_IORETCODES_H
