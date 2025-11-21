/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: Blob.h
 * SPDX-FileContributor: Dmitry Vedenko
 */

#pragma once

#include <cstdint>

struct sqlite3_blob;

namespace audacity::sqlite {
//! A class representing a BLOB in a SQLite database
class SQLITE_HELPERS_API Blob final
{
    friend class Connection;
    explicit Blob(sqlite3_blob* blob) noexcept;

public:
    ~Blob() noexcept;

    Blob(const Blob&) = delete;
    Blob(Blob&&) noexcept;
    Blob& operator=(const Blob&) = delete;
    Blob& operator=(Blob&&) noexcept;

    //! Returns the size of the BLOB
    int64_t Size() const noexcept;

    //! Reads up to bufferSize bytes from the BLOB into the buffer at the given offset
    int64_t Read(void* buffer, int64_t offset, int64_t bufferSize) const noexcept;
    //! Writes up to bufferSize bytes from the buffer into the BLOB at the given offset
    int64_t Write(const void* buffer, int64_t offset, int64_t bufferSize) noexcept;

    //! Reads up to bufferSize bytes from the BLOB into a container at the given offset
    /*!
     * If bufferSize is negative, the entire BLOB is read.
     * ContainerType must have a resize() method and a data() method.
     */
    template<typename ContainerType>
    ContainerType Read(int64_t offset = 0, int64_t bufferSize = -1) const
    {
        if (bufferSize < 0) {
            bufferSize = Size() - offset;
        }

        if (bufferSize <= 0) {
            return {}
        }

        ContainerType buffer;
        buffer.resize(bufferSize);
        const auto bytesRead = Read(buffer.data(), offset, bufferSize);
        buffer.resize(bytesRead);

        return buffer;
    }

private:
    sqlite3_blob* mBlob {};
}; // class Blob
} // namespace audacity::sqlite
