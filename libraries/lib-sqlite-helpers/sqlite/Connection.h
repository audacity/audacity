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

#include "Result.h"
#include "Transaction.h"
#include "Statement.h"
#include "Blob.h"
#include "Function.h"

struct sqlite3;

namespace sqlite
{
enum class OpenMode
{
   ReadWrite,
   ReadOnly,
   ReadWriteCreate,
   Memory,
};

enum class ThreadMode
{
   MultiThread,
   Serialized,
};

class Transaction;

class SQLITE_HELPERS_API Connection final
{
public:
   static Result<Connection> Open(
      std::string_view path, OpenMode mode = OpenMode::ReadWriteCreate,
      ThreadMode threadMode = ThreadMode::Serialized);

   static Result<Connection> Wrap(sqlite3* connection);

   static Result<Connection> Reopen(
      const Connection& connection, OpenMode mode = OpenMode::ReadWriteCreate,
      ThreadMode threadMode = ThreadMode::Serialized);

   static Result<Connection> Reopen (
      sqlite3* connection, OpenMode mode = OpenMode::ReadWriteCreate,
      ThreadMode threadMode = ThreadMode::Serialized);

   Connection() = default;

   Connection(const Connection&) = delete;
   Connection(Connection&&) noexcept;

   Connection& operator=(const Connection&) = delete;
   Connection& operator=(Connection&&) noexcept;

   ~Connection();

   bool IsOpen() const noexcept;

   bool CheckTableExists(std::string_view tableName) const;

   Error Execute(std::string_view sql) noexcept;

   Transaction BeginTransaction(std::string name);

   Result<Statement> CreateStatement(std::string_view sql) const;

   template<typename ScalarFunctionType>
   ScalarFunction CreateScalarFunction(std::string name, ScalarFunctionType function)
   {
      return ScalarFunction { mConnection, std::move(name),
                              std::move(function) };
   }

   template<typename StepFunctionType, typename FinalFunctionType>
   AggregateFunction CreateAggregateFunction (
      std::string name, StepFunctionType stepFunction,
      FinalFunctionType finalFunction)
   {
      return AggregateFunction { mConnection, std::move(name),
                                      std::move(stepFunction),
                                      std::move(finalFunction) };
   }

   Result<Blob> OpenBlob(
      const std::string& tableName, const std::string& columnName,
      int64_t rowId, bool readOnly,
      const std::string& databaseName = "main") const;

   explicit operator sqlite3* () const noexcept;
   explicit operator bool() const noexcept;

   Error Close(bool force) noexcept;

   std::string_view GetPath(const char* dbName = {}) const noexcept;

private:
   Connection(sqlite3* connection, bool owned) noexcept;

   static Error TransactionHandler(
      Connection& connection, Transaction::TransactionOperation operation,
      Transaction& name);

   sqlite3* mConnection {};

   std::mutex mPendingTransactionsMutex {};
   std::vector<Transaction*> mPendingTransactions {};

   bool mInDestructor {};
   bool mIsOwned {};
};
} // namespace sqlite
