/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: Blob.h
 * SPDX-FileContributor: Dmitry Vedenko
 */

#pragma once

#include <cstdint>

struct sqlite3_blob;

namespace sqlite
{
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

   int64_t Size() const noexcept;

   int64_t Read(void* buffer, int64_t offset, int64_t bufferSize) const noexcept;
   int64_t Write(const void* buffer, int64_t offset, int64_t bufferSize) noexcept;

   template<typename ContainerType>
   ContainerType Read(int64_t offset = 0, int64_t bufferSize = -1) const
   {
      if (bufferSize < 0)
         bufferSize = Size() - offset;

      if (bufferSize <= 0)
         return {};

      ContainerType buffer;
      buffer.resize(bufferSize);
      const auto bytesRead = Read(buffer.data(), offset, bufferSize);
      buffer.resize(bytesRead);

      return buffer;
   }

private:
   sqlite3_blob* mBlob {};
};
} // namespace sqlite
