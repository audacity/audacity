/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: Blob.cpp
 * SPDX-FileContributor: Dmitry Vedenko
 */

#include "Blob.h"

#include <algorithm>
#include <cassert>
#include <utility>

#include <sqlite3.h>

namespace audacity::sqlite {
Blob::Blob(sqlite3_blob* blob) noexcept
    : mBlob{blob}
{
    assert(mBlob != nullptr);
}

Blob::~Blob() noexcept
{
    if (mBlob != nullptr) {
        sqlite3_blob_close(mBlob);
        mBlob = nullptr;
    }
}

Blob::Blob(Blob&& rhs) noexcept
{
    *this = std::move(rhs);
}

Blob& Blob::operator=(Blob&& rhs) noexcept
{
    std::swap(mBlob, rhs.mBlob);
    return *this;
}

int64_t Blob::Size() const noexcept
{
    if (mBlob == nullptr) {
        return 0;
    }

    return sqlite3_blob_bytes(mBlob);
}

int64_t
Blob::Read(void* buffer, int64_t offset, int64_t bufferSize) const noexcept
{
    if (mBlob == nullptr) {
        return 0;
    }

    const int readSize = std::min<int>(bufferSize, Size() - offset);

    if (bufferSize <= 0) {
        return 0;
    }

    if (
        SQLITE_OK
        != sqlite3_blob_read(mBlob, buffer, readSize, static_cast<int>(offset))) {
        return 0;
    }

    return readSize;
}

int64_t
Blob::Write(const void* buffer, int64_t offset, int64_t bufferSize) noexcept
{
    if (mBlob == nullptr) {
        return 0;
    }

    const int writeSize = std::min<int>(bufferSize, Size() - offset);

    if (bufferSize <= 0) {
        return 0;
    }

    if (
        SQLITE_OK
        != sqlite3_blob_write(mBlob, buffer, writeSize, static_cast<int>(offset))) {
        return 0;
    }

    return writeSize;
}
} // namespace audacity::sqlite
