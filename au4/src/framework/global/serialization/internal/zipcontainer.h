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
#ifndef MU_GLOBAL_ZIPCONTAINER_H
#define MU_GLOBAL_ZIPCONTAINER_H

#include <ctime>
#include <string>

#include "io/iodevice.h"

namespace mu {
class ZipContainer
{
public:
    explicit ZipContainer(io::IODevice* device);
    ~ZipContainer();

    enum Status {
        NoError,
        FileOpenError,
        FileReadError,
        FileWriteError,
        FileError
    };

    struct FileInfo
    {
        std::string filePath;
        bool isDir = false;
        bool isFile  = false;
        bool isSymLink = false;
        unsigned int crc = 0;
        int64_t size = 0;
        std::tm lastModified;

        bool isValid() const { return isDir || isFile || isSymLink; }
    };

    Status status() const;

    void close();

    // Read
    std::vector<FileInfo> fileInfoList() const;
    int count() const;

    bool fileExists(const std::string& fileName) const;
    ByteArray fileData(const std::string& fileName) const;

    // Write
    enum CompressionPolicy {
        AlwaysCompress,
        NeverCompress,
        AutoCompress
    };

    void setCompressionPolicy(CompressionPolicy policy);
    CompressionPolicy compressionPolicy() const;

    void addFile(const std::string& fileName, const ByteArray& data);
    void addDirectory(const std::string& dirName);

private:

    struct Impl;
    Impl* p = nullptr;
};
}

#endif // MU_GLOBAL_ZIPCONTAINER_H
