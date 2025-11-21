/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: Transaction.cpp
 * SPDX-FileContributor: Dmitry Vedenko
 */

#include "Transaction.h"

namespace audacity::sqlite {
Transaction::Transaction(
    Connection& connection, TransactionHandler handler,
    std::string_view name) noexcept
    : mConnection(connection)
    , mHandler(handler)
    , mName(name)
{
    mBeginResult = mHandler(mConnection, TransactionOperation::BeginOp, *this);
}

Transaction::~Transaction()
{
    Abort();
}

Error Transaction::GetBeginResult() const noexcept
{
    return mBeginResult;
}

bool Transaction::IsOpen() const noexcept
{
    return mBeginResult.IsOk() && !mCommitted;
}

Error Transaction::Commit() noexcept
{
    if (mCommitted) {
        return {}
    }

    if (mBeginResult.IsError()) {
        return mBeginResult;
    }

    return mHandler(mConnection, TransactionOperation::CommitOp, *this);
}

Error Transaction::Abort() noexcept
{
    if (mCommitted) {
        return {}
    }

    if (mBeginResult.IsError()) {
        return mBeginResult;
    }

    return mHandler(mConnection, TransactionOperation::RollbackOp, *this);
}
} // namespace audacity::sqlite
