/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: Connection.h
 * SPDX-FileContributor: Dmitry Vedenko
 */

#pragma once

#include <mutex>
#include <optional>
#include <string>
#include <vector>

#include "Blob.h"
#include "Function.h"
#include "Result.h"
#include "Statement.h"
#include "Transaction.h"

struct sqlite3;

namespace audacity::sqlite {
//! The mode in which the database should be opened
enum class OpenMode
{
    ReadWrite,
    ReadOnly,
    ReadWriteCreate,
    Memory,
};

//! The mode in which the database should be accessed
/*!
 * Connection is never thread-safe. This mode is used to specify how the
 * multiple connections to the same database should be handled.
 */
enum class ThreadMode
{
    MultiThread,
    Serialized,
};

class Transaction;

//! A class representing a connection to a SQLite database
class SQLITE_HELPERS_API Connection final
{
public:
    //! Opens a connection to a database
    static Result<Connection> Open(
        std::string_view path, OpenMode mode = OpenMode::ReadWriteCreate, ThreadMode threadMode = ThreadMode::Serialized);

    //! Wraps an existing connection. The connection will not be closed when the
    //! object is destroyed.
    static Result<Connection> Wrap(sqlite3* connection);

    //! Opens a new connection to the same database as the existing connection
    //! but with different parameters
    static Result<Connection> Reopen(
        const Connection& connection, OpenMode mode = OpenMode::ReadWriteCreate, ThreadMode threadMode = ThreadMode::Serialized);

    //! Opens a new connection to the same database as the existing connection
    //! but with different parameters
    static Result<Connection> Reopen(
        sqlite3* connection, OpenMode mode = OpenMode::ReadWriteCreate, ThreadMode threadMode = ThreadMode::Serialized);

    Connection() = default;

    Connection(const Connection&) = delete;
    Connection(Connection&&) noexcept;

    Connection& operator=(const Connection&) = delete;
    Connection& operator=(Connection&&) noexcept;

    ~Connection();

    //! Returns true if the connection is open
    bool IsOpen() const noexcept;

    //! Returns true if the table exists in the database
    bool CheckTableExists(std::string_view tableName) const;

    //! Executes the given SQL statement and returns the result
    Error Execute(std::string_view sql) noexcept;

    //! Starts a new transaction
    Transaction BeginTransaction(std::string name);

    //! Prepares the given SQL statement for execution
    Result<Statement> CreateStatement(std::string_view sql) const;

    //! Registers a scalar function with the given name
    template<typename ScalarFunctionType>
    ScalarFunction
    CreateScalarFunction(std::string name, ScalarFunctionType function)
    {
        return ScalarFunction { mConnection, std::move(name),
                                std::move(function) };
    }

    //! Registers an aggregate function with the given name
    template<typename StepFunctionType, typename FinalFunctionType>
    AggregateFunction CreateAggregateFunction(
        std::string name, StepFunctionType stepFunction,
        FinalFunctionType finalFunction)
    {
        return AggregateFunction { mConnection, std::move(name),
                                   std::move(stepFunction),
                                   std::move(finalFunction) };
    }

    //! Opens a BLOB for reading or writing
    Result<Blob> OpenBlob(
        const std::string& tableName, const std::string& columnName, int64_t rowId, bool readOnly,
        const std::string& databaseName = "main") const;

    //! Returns the underlying sqlite3* object
    explicit operator sqlite3*() const noexcept;

    //! Returns true if the connection is open
    explicit operator bool() const noexcept;

    //! Closes the connection
    Error Close(bool force) noexcept;

    //! Returns the path to the database
    std::string_view GetPath(const char* dbName = {}) const noexcept;

private:
    Connection(sqlite3* connection, bool owned) noexcept;

    static Error TransactionHandler(
        Connection& connection, Transaction::TransactionOperation operation, Transaction& name);

    sqlite3* mConnection {};

    std::vector<Transaction*> mPendingTransactions {};

    bool mInDestructor {};
    bool mIsOwned {};
};
} // namespace audacity::sqlite
