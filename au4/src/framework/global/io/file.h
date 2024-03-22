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
#ifndef MU_IO_FILE_H
#define MU_IO_FILE_H

#include "global/modularity/ioc.h"
#include "ifilesystem.h"

#include "iodevice.h"
#include "path.h"

namespace mu::io {
class File : public IODevice
{
    static inline Inject<IFileSystem> fileSystem;

public:

    File() = default;
    File(const path_t& filePath);
    ~File();

    path_t filePath() const;

    bool exists() const;
    bool remove();

    static bool exists(const path_t& filePath);
    static bool remove(const path_t& filePath);
    static bool copy(const path_t& src, const path_t& dst, bool replace = false);
    static Ret readFile(const io::path_t& filePath, ByteArray& out);
    static Ret writeFile(const io::path_t& filePath, const ByteArray& data);
    static bool setPermissionsAllowedForAll(const path_t& filePath);

protected:

    bool doOpen(OpenMode m) override;
    size_t dataSize() const override;
    const uint8_t* rawData() const override;
    bool resizeData(size_t size) override;
    size_t writeData(const uint8_t* data, size_t len) override;

private:

    path_t m_filePath;
    ByteArray m_data;
};
}

#endif // MU_IO_FILE_H
