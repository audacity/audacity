/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: Statement.cpp
 * SPDX-FileContributor: Dmitry Vedenko
 */

#include "Statement.h"

#include <cassert>
#include <cstring>
#include <utility>

#include "Connection.h"

#include "sqlite3.h"

namespace audacity::sqlite {
struct StatementHandle final
{
    sqlite3_stmt* Handle {};

    explicit StatementHandle(sqlite3_stmt* Handle) noexcept
        : Handle{Handle}
    {
    }

    operator sqlite3_stmt*() noexcept
    {
        return Handle;
    }

    ~StatementHandle()
    {
        if (Handle != nullptr) {
            sqlite3_finalize(Handle);
        }
    }
};

Statement::Statement(sqlite3_stmt* stmt)
    : mStatement{std::make_shared<StatementHandle>(stmt)}
{
}

Statement::Statement(Statement&& rhs) noexcept
{
    *this = std::move(rhs);
}

Statement& Statement::operator=(Statement&& rhs) noexcept
{
    std::swap(mStatement, rhs.mStatement);
    return *this;
}

RunContext& Statement::Prepare() noexcept
{
    mRunContext = RunContext { mStatement };
    return *mRunContext;
}

RunContext::RunContext(StatementHandlePtr stmt) noexcept
    : mStatement{std::move(stmt)}
{
}

RunContext::RunContext(RunContext&& rhs) noexcept
{
    *this = std::move(rhs);
}

RunContext& RunContext::operator=(RunContext&& rhs) noexcept
{
    std::swap(mStatement, rhs.mStatement);
    std::swap(mErrors, rhs.mErrors);

    return *this;
}

template<typename Binder>
RunContext& RunContext::DoBind(Binder binder)
{
    if (mStatement == nullptr) {
        mErrors.emplace_back(Error(SQLITE_MISUSE));
    } else if (int result = binder(); result != SQLITE_OK) {
        mErrors.emplace_back(Error(result));
    }

    return *this;
}

RunContext& RunContext::Bind(
    int index, const void* data, int64_t size, bool makeCopy)
{
    if (data == nullptr) {
        return BindZeroBlob(index, size);
    }

    return DoBind(
        [&]
    {
        return sqlite3_bind_blob64(
            *mStatement, index, data, size,
            makeCopy ? SQLITE_TRANSIENT : SQLITE_STATIC);
    });
}

RunContext& RunContext::Bind(
    int index, const std::string& value, bool makeCopy)
{
    return Bind(index, std::string_view { value }, makeCopy);
}

RunContext& RunContext::Bind(
    int index, std::string_view value, bool makeCopy)
{
    return DoBind(
        [&]
    {
        if (mNeedsReset) {
            mNeedsReset = false;
            sqlite3_reset(*mStatement);
        }

        return sqlite3_bind_text(
            *mStatement, index, value.data(), value.size(),
            makeCopy ? SQLITE_TRANSIENT : SQLITE_STATIC);
    });
}

RunContext& RunContext::Bind(
    int index, const char* value, bool makeCopy)
{
    return Bind(index, std::string_view { value }, makeCopy);
}

RunContext& RunContext::Bind(int index, bool value)
{
    return Bind(index, static_cast<long long>(value));
}

RunContext&
RunContext::Bind(int index, int value)
{
    return Bind(index, static_cast<long long>(value));
}

RunContext&
RunContext::Bind(int index, long value)
{
    return Bind(index, static_cast<long long>(value));
}

RunContext&
RunContext::Bind(int index, long long value)
{
    return DoBind([&] { return sqlite3_bind_int64(*mStatement, index, value); });
}

RunContext& sqlite::RunContext::Bind(int index, std::size_t value)
{
    return DoBind([&] { return sqlite3_bind_int64(*mStatement, index, value); });
}

RunContext&
RunContext::Bind(int index, float value)
{
    return Bind(index, static_cast<double>(value));
}

RunContext&
RunContext::Bind(int index, double value)
{
    return DoBind([&] { return sqlite3_bind_double(*mStatement, index, value); });
}

RunContext&
RunContext::Bind(int index, std::nullptr_t)
{
    return DoBind([&] { return sqlite3_bind_null(*mStatement, index); });
}

RunContext&
RunContext::BindZeroBlob(int index, int64_t size)
{
    return DoBind([&]
    { return sqlite3_bind_zeroblob64(*mStatement, index, size); });
}

int RunContext::GetParameterIndex(const std::string& name) const noexcept
{
    return mStatement != nullptr
           ? sqlite3_bind_parameter_index(*mStatement, name.data())
           : -1;
}

RunResult RunContext::Run()
{
    mNeedsReset = true;
    return RunResult { mStatement, std::move(mErrors) };
}

RunResult::RunResult(StatementHandlePtr stmt, std::vector<Error> errors) noexcept
    : mStatement{std::move(stmt)}
    , mErrors{std::move(errors)}
{
    // mStatement can't be nullptr here, by construction
    assert(mStatement != nullptr);

    const auto rc = sqlite3_step(*mStatement);

    mHasRows = rc == SQLITE_ROW;

    if (rc == SQLITE_DONE) {
        mModifiedRowsCount = sqlite3_changes(sqlite3_db_handle(*mStatement));
    }

    if (rc != SQLITE_DONE && !mHasRows) {
        mErrors.emplace_back(Error(rc));
    }
}

RunResult::RunResult(RunResult&& rhs) noexcept
{
    *this = std::move(rhs);
}

RunResult& RunResult::operator=(RunResult&& rhs) noexcept
{
    std::swap(mStatement, rhs.mStatement);
    std::swap(mErrors, rhs.mErrors);
    std::swap(mHasRows, rhs.mHasRows);
    std::swap(mModifiedRowsCount, rhs.mModifiedRowsCount);

    return *this;
}

RunResult::~RunResult()
{
    if (mStatement != nullptr) {
        sqlite3_reset(*mStatement);
    }
}

bool RunResult::IsOk() const noexcept
{
    return mErrors.empty();
}

bool RunResult::HasRows() const noexcept
{
    return mHasRows;
}

int RunResult::GetModifiedRowsCount() const noexcept
{
    return mModifiedRowsCount;
}

const std::vector<Error>& RunResult::GetErrors() const noexcept
{
    return mErrors;
}

RowIterator RunResult::begin() noexcept
{
    return mHasRows ? RowIterator { mStatement, mErrors } : RowIterator {};
}

RowIterator RunResult::end() noexcept
{
    return RowIterator {};
}

RowIterator::RowIterator(StatementHandlePtr stmt, std::vector<Error>& errors) noexcept
    : mStatement{std::move(stmt)}
    , mErrors{&errors}
{
    // mStatement can't be nullptr here, by construction
    assert(mStatement != nullptr);
}

RowIterator::RowIterator () noexcept
    : mDone{true}
{
}

RowIterator::RowIterator(RowIterator&& rhs) noexcept
{
    *this = std::move(rhs);
}

RowIterator& RowIterator::operator=(RowIterator&& rhs) noexcept
{
    std::swap(mStatement, rhs.mStatement);
    std::swap(mErrors, rhs.mErrors);
    std::swap(mRowIndex, rhs.mRowIndex);
    std::swap(mDone, rhs.mDone);
    return *this;
}

RowIterator& RowIterator::operator++() noexcept
{
    if (mStatement == nullptr || mDone) {
        return *this;
    }

    const auto rc = sqlite3_step(*mStatement);

    if (rc == SQLITE_ROW) {
        ++mRowIndex;
    } else {
        mDone = true;

        if (rc != SQLITE_DONE) {
            mErrors->emplace_back(Error(rc));
        }
    }

    return *this;
}

bool RowIterator::operator==(const RowIterator& rhs) const noexcept
{
    if (mDone != rhs.mDone) {
        return false;
    }

    if (mDone) {
        return true;
    }

    return mStatement == rhs.mStatement && mRowIndex == rhs.mRowIndex;
}

bool RowIterator::operator!=(const RowIterator& rhs) const noexcept
{
    return !(*this == rhs);
}

Row RowIterator::operator*() const noexcept
{
    return Row { mStatement, *mErrors };
}

Row::Row(StatementHandlePtr statement, std::vector<Error>& errors) noexcept
    : mStatement{std::move(statement)}
    , mErrors{&errors}
{
    if (mStatement != nullptr) {
        mColumnsCount = sqlite3_column_count(*mStatement);
    }
}

template<typename Reader>
bool Row::DoGet(Reader reader, int columnIndex) const
{
    if (mStatement == nullptr) {
        if (mErrors != nullptr) {
            mErrors->emplace_back(Error(SQLITE_MISUSE));
        }
        return false;
    }

    if (columnIndex < 0 || columnIndex >= mColumnsCount) {
        if (mErrors != nullptr) {
            mErrors->emplace_back(Error(SQLITE_RANGE));
        }
        return false;
    }

    if constexpr (std::is_void_v<decltype(reader())>) {
        reader();
        return true;
    } else {
        return reader();
    }
}

bool Row::Get(int columnIndex, bool& value) const
{
    return DoGet([&] { value = sqlite3_column_int(*mStatement, columnIndex) != 0; }, columnIndex);
}

bool Row::Get(int columnIndex, int& value) const
{
    return DoGet([&] { value = sqlite3_column_int(*mStatement, columnIndex); }, columnIndex);
}

bool Row::Get(int columnIndex, long& value) const
{
    if (sizeof(long) == 4) {
        return DoGet([&] { value = sqlite3_column_int(*mStatement, columnIndex); }, columnIndex);
    } else {
        return DoGet([&] { value = sqlite3_column_int64(*mStatement, columnIndex); }, columnIndex);
    }
}

bool Row::Get(int columnIndex, long long& value) const
{
    return DoGet([&] { value = sqlite3_column_int64(*mStatement, columnIndex); }, columnIndex);
}

bool Row::Get(int columnIndex, float& value) const
{
    return DoGet(
        [&]
    {
        value
            =static_cast<float>(sqlite3_column_double(*mStatement, columnIndex));
    },
        columnIndex);
}

bool Row::Get(int columnIndex, double& value) const
{
    return DoGet([&] { value = sqlite3_column_double(*mStatement, columnIndex); },
                 columnIndex);
}

bool Row::Get(int columnIndex, std::string& value) const
{
    return DoGet(
        [&]
    {
        const auto* text = reinterpret_cast<const char*>(
            sqlite3_column_text(*mStatement, columnIndex));

        if (text == nullptr) {
            return false;
        }

        // Per sqlite convention, text is never nullptr
        value = text;
        return true;
    },
        columnIndex);
}

int Row::GetColumnCount() const
{
    return sqlite3_column_count(*mStatement);
}

int64_t Row::GetColumnBytes(int columnIndex) const
{
    return sqlite3_column_bytes(*mStatement, columnIndex);
}

int64_t Row::ReadData(int columnIndex, void* buffer, int64_t maxSize) const
{
    const auto* data = sqlite3_column_blob(*mStatement, columnIndex);

    if (data == nullptr) {
        return 0;
    }

    const auto size = std::min(maxSize, GetColumnBytes(columnIndex));

    std::memcpy(buffer, data, size);

    return size;
}
} // namespace audacity::sqlite
