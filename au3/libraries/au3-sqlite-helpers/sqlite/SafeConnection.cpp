/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: SafeConnection.cpp
 * SPDX-FileContributor: Dmitry Vedenko
 */

#include "SafeConnection.h"

namespace audacity::sqlite {
SafeConnection::SafeConnection(Tag, Connection connection)
    : mConnection(std::move(connection))
{
}

std::shared_ptr<SafeConnection> SafeConnection::Open(
    std::string_view path, OpenMode mode, ThreadMode threadMode,
    Error* openError)
{
    auto connection = Connection::Open(path, mode, threadMode);

    if (!connection) {
        if (openError) {
            *openError = connection.GetError();
        }

        return {};
    }

    return std::make_shared<SafeConnection>(Tag {}, std::move(*connection));
}

std::shared_ptr<SafeConnection> SafeConnection::Reopen(
    const Connection& prevConnection, OpenMode mode, ThreadMode threadMode,
    Error* openError)
{
    auto connection = Connection::Reopen(prevConnection, mode, threadMode);

    if (!connection) {
        if (openError) {
            *openError = connection.GetError();
        }

        return {};
    }

    return std::make_shared<SafeConnection>(Tag {}, std::move(*connection));
}

std::shared_ptr<SafeConnection> SafeConnection::Reopen(
    sqlite3* prevConnection, OpenMode mode, ThreadMode threadMode,
    Error* openError)
{
    auto connection = Connection::Reopen(prevConnection, mode, threadMode);

    if (!connection) {
        if (openError) {
            *openError = connection.GetError();
        }

        return {};
    }

    return std::make_shared<SafeConnection>(Tag {}, std::move(*connection));
}

std::shared_ptr<SafeConnection> SafeConnection::Reopen(
    SafeConnection& prevConnection, OpenMode mode, ThreadMode threadMode,
    Error* openError)
{
    auto connection
        =Connection::Reopen(prevConnection.mConnection, mode, threadMode);

    if (!connection) {
        if (openError) {
            *openError = connection.GetError();
        }

        return {};
    }

    return std::make_shared<SafeConnection>(Tag {}, std::move(*connection));
}

SafeConnection::Lock SafeConnection::Acquire() noexcept
{
    return Lock { shared_from_this() };
}

SafeConnection::Lock::Lock(std::shared_ptr<SafeConnection> connection)
    : mSafeConnection(std::move(connection))
{
    if (mSafeConnection) {
        mLock = std::unique_lock { mSafeConnection->mConnectionMutex }
    }
}

Connection* SafeConnection::Lock::operator->() noexcept
{
    return &mSafeConnection->mConnection;
}

const Connection* SafeConnection::Lock::operator->() const noexcept
{
    return &mSafeConnection->mConnection;
}

Connection& SafeConnection::Lock::operator*() noexcept
{
    return mSafeConnection->mConnection;
}

const Connection& SafeConnection::Lock::operator*() const noexcept
{
    return mSafeConnection->mConnection;
}

SafeConnection::Lock::operator bool() const noexcept
{
    return IsValid();
}

bool SafeConnection::Lock::IsValid() const noexcept
{
    return mSafeConnection != nullptr;
}
} // namespace audacity::sqlite
