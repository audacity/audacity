/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: SafeConnection.h
 * SPDX-FileContributor: Dmitry Vedenko
 */

#pragma once

#include <memory>
#include <mutex>

#include "Connection.h"

namespace audacity::sqlite {
//! A class representing a safe connection to SQLite
/*!
 * This class is a wrapper around the Connection class that provides a thread-safe
 * access to the connection.
 */
class SQLITE_HELPERS_API SafeConnection final : public std::enable_shared_from_this<SafeConnection>
{
    struct Tag final
    {
    };

    using MutexType = std::recursive_mutex;

public:
    SafeConnection(Tag, Connection connection);

    static std::shared_ptr<SafeConnection> Open(
        std::string_view path, OpenMode mode = OpenMode::ReadWriteCreate, ThreadMode threadMode = ThreadMode::Serialized,
        Error* openError      = nullptr);

    static std::shared_ptr<SafeConnection> Reopen(
        const Connection& connection, OpenMode mode = OpenMode::ReadWriteCreate, ThreadMode threadMode = ThreadMode::Serialized,
        Error* openError      = nullptr);

    static std::shared_ptr<SafeConnection> Reopen(
        sqlite3* connection, OpenMode mode = OpenMode::ReadWriteCreate, ThreadMode threadMode = ThreadMode::Serialized,
        Error* openError      = nullptr);

    static std::shared_ptr<SafeConnection> Reopen(
        SafeConnection& connection, OpenMode mode = OpenMode::ReadWriteCreate, ThreadMode threadMode = ThreadMode::Serialized,
        Error* openError      = nullptr);

    struct SQLITE_HELPERS_API Lock final
    {
        explicit Lock(std::shared_ptr<SafeConnection> connection);
        ~Lock() = default;

        Lock(const Lock&)            = delete;
        Lock& operator=(const Lock&) = delete;
        Lock(Lock&&)                 = default;
        Lock& operator=(Lock&&)      = default;

        Connection* operator->() noexcept;
        const Connection* operator->() const noexcept;

        Connection& operator*() noexcept;
        const Connection& operator*() const noexcept;

        explicit operator bool() const noexcept;
        bool IsValid() const noexcept;

    private:
        std::shared_ptr<SafeConnection> mSafeConnection;
        std::unique_lock<MutexType> mLock;
    };

    Lock Acquire() noexcept;

private:
    Connection mConnection;
    MutexType mConnectionMutex;
};
} // namespace audacity::sqlite
