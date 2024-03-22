/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2022 MuseScore BVBA and others
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

#ifndef MU_GLOBAL_DLIB_H
#define MU_GLOBAL_DLIB_H

#if defined(Q_OS_WIN) && !defined(__MINGW64__)
#include <windows.h>
#else
#include <dlfcn.h>
#endif

#include "io/path.h"

#include "log.h"

namespace mu {
inline void* loadLib(const io::path_t& path)
{
#if defined(Q_OS_WIN) && !defined(__MINGW64__)
    return LoadLibrary(path.toStdWString().c_str());
#else
    return dlopen(path.c_str(), RTLD_LAZY);
#endif
}

inline void* getLibFunc(void* libHandle, const char* funcName)
{
#if defined(Q_OS_WIN) && !defined(__MINGW64__)
    return GetProcAddress((HINSTANCE)libHandle, funcName);
#else
    return dlsym(libHandle, funcName);
#endif
}

inline void closeLib(void* libHandle)
{
#if defined(Q_OS_WIN) && !defined(__MINGW64__)
    UNUSED(libHandle);
    return;
#else
    dlclose(libHandle);
#endif
}
}

#endif // MU_GLOBAL_DLIB_H
