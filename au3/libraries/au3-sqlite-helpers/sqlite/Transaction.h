/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: Transaction.h
 * SPDX-FileContributor: Dmitry Vedenko
 */

#pragma once

#include <string>
#include <string_view>

#include "Error.h"

namespace audacity::sqlite {
class Connection;

//! A class representing a transaction in SQLite
class SQLITE_HELPERS_API Transaction final
{
    friend class Connection;

    enum class TransactionOperation
    {
        BeginOp,
        CommitOp,
        RollbackOp,
    };

    using TransactionHandler = Error (*)(Connection&, TransactionOperation, Transaction&);
    Transaction(
        Connection& connection, TransactionHandler handler, std::string_view name) noexcept;

public:
    Transaction(const Transaction&) = delete;
    Transaction(Transaction&&) = delete;

    Transaction& operator=(const Transaction&) = delete;
    Transaction& operator=(Transaction&&) = delete;

    ~Transaction();

    Error GetBeginResult() const noexcept;

    bool IsOpen() const noexcept;

    Error Commit() noexcept;
    Error Abort() noexcept;

private:
    Connection& mConnection;
    TransactionHandler mHandler;
    std::string mName;
    Error mBeginResult {};
    bool mCommitted {};
};
} // namespace audacity::sqlite
