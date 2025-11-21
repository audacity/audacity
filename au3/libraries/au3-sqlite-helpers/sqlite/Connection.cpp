/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: Connection.cpp
 * SPDX-FileContributor: Dmitry Vedenko
 */

#include "Connection.h"

#include <algorithm>
#include <cassert>

#include "sqlite3.h"

#include "MemoryX.h"

#include "SQLiteUtils.h"

namespace audacity::sqlite {
Result<Connection>
Connection::Open(std::string_view path, OpenMode mode, ThreadMode threadMode)
{
    auto error = Initialize();

    if (error.IsError()) {
        return error;
    }

    int flags = 0;

    switch (mode) {
    case OpenMode::ReadWrite:
        flags = SQLITE_OPEN_READWRITE;
        break;
    case OpenMode::ReadOnly:
        flags = SQLITE_OPEN_READONLY;
        break;
    case OpenMode::ReadWriteCreate:
        flags = SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE;
        break;
    case OpenMode::Memory:
        flags = SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE | SQLITE_OPEN_MEMORY;
        break;
    }

    switch (threadMode) {
    case ThreadMode::MultiThread:
        flags |= SQLITE_OPEN_NOMUTEX;
        break;
    case ThreadMode::Serialized:
        flags |= SQLITE_OPEN_FULLMUTEX;
        break;
    }

    sqlite3* connection = nullptr;

    // If the path is not null-terminated, copy it to a temporary string.
    std::string temp;
    if (*(path.data() + path.size()) != '\0') {
        temp = std::string(path);
        path = temp;
    }

    error = Error(sqlite3_open_v2(path.data(), &connection, flags, nullptr));

    if (error.IsError()) {
        return error;
    }

    return Connection(connection, true);
}

Result<Connection> Connection::Wrap(sqlite3* connection)
{
    if (connection == nullptr) {
        return Error(SQLITE_MISUSE);
    }

    return Connection(connection, false);
}

Result<Connection> Connection::Reopen(
    const Connection& connection, OpenMode mode, ThreadMode threadMode)
{
    if (!connection.IsOpen()) {
        return Error(SQLITE_MISUSE);
    }

    auto path = connection.GetPath();

    // For memory and temporary databases, the path is empty.
    if (path.empty()) {
        return Error(SQLITE_MISUSE);
    }

    return Open(path, mode, threadMode);
}

Result<Connection>
Connection::Reopen(sqlite3* connection, OpenMode mode, ThreadMode threadMode)
{
    auto result = Wrap(connection);

    if (!result) {
        return result;
    }

    return Reopen(*result, mode, threadMode);
}

Connection::Connection(sqlite3* connection, bool owned) noexcept
    : mConnection{connection}
    , mIsOwned{owned}
{
}

Connection::Connection(Connection&& rhs) noexcept
{
    *this = std::move(rhs);
}

Connection& Connection::operator=(Connection&& rhs) noexcept
{
    std::swap(mConnection, rhs.mConnection);
    std::swap(mIsOwned, rhs.mIsOwned);
    std::swap(mInDestructor, rhs.mInDestructor);
    std::swap(mPendingTransactions, rhs.mPendingTransactions);

    return *this;
}

Connection::~Connection()
{
    mInDestructor = true;
    auto error = Close(true);
    assert(!error.IsError());
}

bool Connection::IsOpen() const noexcept
{
    return mConnection != nullptr;
}

bool Connection::CheckTableExists(std::string_view tableName) const
{
    auto stmt = CreateStatement(
        "SELECT EXISTS(SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = ?)");

    if (!stmt) {
        return false;
    }

    auto result = stmt->Prepare(tableName).Run();

    if (!result.HasRows()) {
        return false;
    }

    for (auto row : result) {
        return row.GetOr(0, false);
    }

    return false;
}

Error Connection::Execute(std::string_view sql) noexcept
{
    if (mInDestructor || mConnection == nullptr) {
        return Error(SQLITE_MISUSE);
    }

    auto tx = BeginTransaction("Connection_Execute");

    auto first = sql.data();
    auto last = first + sql.size();

    while (first != last)
    {
        const char* next = nullptr;
        sqlite3_stmt* statement = nullptr;

        int result = sqlite3_prepare_v2(
            mConnection, first, last - first, &statement, &next);

        if (result != SQLITE_OK) {
            return Error(result);
        }

        first = next;

        if (statement == nullptr) {
            continue;
        }

        auto finalizer = finally([statement] { sqlite3_finalize(statement); });

        result = sqlite3_step(statement);

        if (result != SQLITE_OK && result != SQLITE_DONE && result != SQLITE_ROW) {
            return Error(result);
        }

        while (result == SQLITE_ROW) {
            result = sqlite3_step(statement);
        }
    }

    return tx.Commit();
}

Transaction Connection::BeginTransaction(std::string name)
{
    return Transaction(*this, TransactionHandler, std::move(name));
}

Error Connection::TransactionHandler(
    Connection& connection, Transaction::TransactionOperation operation,
    Transaction& transaction)
{
    std::string sql;

    const auto& name = transaction.mName;

    switch (operation) {
    case Transaction::TransactionOperation::BeginOp:
        sql = std::string("SAVEPOINT ") + name;
        break;
    case Transaction::TransactionOperation::CommitOp:
        sql = std::string("RELEASE SAVEPOINT ") + name;
        break;
    case Transaction::TransactionOperation::RollbackOp:
        sql = std::string("ROLLBACK TO SAVEPOINT ") + name
              + std::string("; RELEASE SAVEPOINT ") + name;
        break;
    }

    auto error = Error(sqlite3_exec(
                           connection.mConnection, sql.c_str(), nullptr, nullptr, nullptr));

    if (operation != Transaction::TransactionOperation::BeginOp) {
        transaction.mCommitted = true;

        connection.mPendingTransactions.erase(
            std::remove_if(
                connection.mPendingTransactions.begin(),
                connection.mPendingTransactions.end(),
                [tx = &transaction](auto lhs) { return lhs == tx; }),
            connection.mPendingTransactions.end());
    }

    if (error.IsError()) {
        return error;
    }

    if (operation == Transaction::TransactionOperation::BeginOp) {
        connection.mPendingTransactions.push_back(&transaction);
    }

    return {};
}

Result<Statement> Connection::CreateStatement(std::string_view sql) const
{
    if (mInDestructor || mConnection == nullptr) {
        return Error(SQLITE_MISUSE);
    }

    sqlite3_stmt* statement = nullptr;

    auto error = Error(sqlite3_prepare_v2(
                           mConnection, sql.data(), sql.size(), &statement, nullptr));

    if (error.IsError()) {
        return error;
    }

    return Result<Statement>(Statement(statement));
}

Result<Blob> Connection::OpenBlob(
    const std::string& tableName, const std::string& columnName, int64_t rowId,
    bool readOnly, const std::string& databaseName) const
{
    if (mInDestructor || mConnection == nullptr) {
        return Error(SQLITE_MISUSE);
    }

    sqlite3_blob* blob = nullptr;

    auto error = Error(sqlite3_blob_open(
                           mConnection, databaseName.c_str(), tableName.c_str(), columnName.c_str(),
                           rowId, readOnly ? 0 : 1, &blob));

    if (error.IsError()) {
        return error;
    }

    return Blob { blob };
}

Connection::operator sqlite3*() const noexcept
{
    return mConnection;
}

Connection::operator bool() const noexcept
{
    return IsOpen();
}

Error Connection::Close(bool force) noexcept
{
    // If there is a transaction in progress,
    // rollback it.
    std::vector<Transaction*> pendingTransactions;

    for (auto* transaction : pendingTransactions) {
        if (auto err = transaction->Abort(); !force && err.IsError()) {
            return err;
        }
    }

    if (mConnection != nullptr && mIsOwned) {
        if (auto err = Error(sqlite3_close(mConnection)); err.IsError()) {
            return err;
        }
    }

    mConnection = nullptr;

    return {};
}

std::string_view Connection::GetPath(const char* dbName) const noexcept
{
    auto path = sqlite3_db_filename(mConnection, dbName);

    if (path == nullptr) {
        return {}
    }

    return path;
}
} // namespace audacity::sqlite
