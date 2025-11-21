/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: Statement.h
 * SPDX-FileContributor: Dmitry Vedenko
 */

#pragma once

#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "Error.h"

struct sqlite3_stmt;

namespace audacity::sqlite {
struct StatementHandle;
using StatementHandlePtr = std::shared_ptr<StatementHandle>;

class RowIterator;

//! A class representing a row in a result set
/*!
 * Indices are 0-based
 */
class SQLITE_HELPERS_API Row final
{
    friend class RowIterator;
    explicit Row(
        StatementHandlePtr statement, std::vector<Error>& errors) noexcept;
    Row() = default;

public:
    bool Get(int columnIndex, bool& value) const;
    bool Get(int columnIndex, int& value) const;
    bool Get(int columnIndex, long& value) const;
    bool Get(int columnIndex, long long& value) const;
    bool Get(int columnIndex, float& value) const;
    bool Get(int columnIndex, double& value) const;
    bool Get(int columnIndex, std::string& value) const;

    template<typename T> T GetOr(int columnIndex, T defaultValue = T()) const
    {
        T value;
        return Get(columnIndex, value) ? value : defaultValue;
    }

    int GetColumnCount() const;

    int64_t GetColumnBytes(int columnIndex) const;
    int64_t ReadData(int columnIndex, void* buffer, int64_t maxSize) const;

private:
    template<typename Reader>
    bool DoGet(Reader reader, int columnIndex) const;

    StatementHandlePtr mStatement {};
    std::vector<Error>* mErrors {};
    int mColumnsCount { 0 };
};

//! A class representing an iterator over a result set
class SQLITE_HELPERS_API RowIterator final
{
    friend class RunResult;
    RowIterator(
        StatementHandlePtr statement, std::vector<Error>& errors) noexcept;

    RowIterator() noexcept;

public:
    RowIterator(const RowIterator&) = delete;
    RowIterator(RowIterator&&) noexcept;
    RowIterator& operator=(const RowIterator&) = delete;
    RowIterator& operator=(RowIterator&&) noexcept;

    RowIterator& operator++() noexcept;

    bool operator==(const RowIterator& other) const noexcept;
    bool operator!=(const RowIterator& other) const noexcept;

    Row operator*() const noexcept;

private:
    StatementHandlePtr mStatement {};
    std::vector<Error>* mErrors {};

    int mRowIndex { 0 };
    bool mDone { false };
};

//! A class representing a result of a run operation
class SQLITE_HELPERS_API RunResult final
{
    friend class RunContext;
    RunResult(StatementHandlePtr statement, std::vector<Error> errors) noexcept;

public:
    RunResult(const RunResult&) = delete;
    RunResult(RunResult&&) noexcept;
    RunResult& operator=(const RunResult&) = delete;
    RunResult& operator=(RunResult&&) noexcept;

    ~RunResult();

    bool IsOk() const noexcept;
    bool HasRows() const noexcept;

    int GetModifiedRowsCount() const noexcept;

    const std::vector<Error>& GetErrors() const noexcept;

    RowIterator begin() noexcept;
    RowIterator end() noexcept;

private:
    StatementHandlePtr mStatement;
    std::vector<Error> mErrors {};
    int mModifiedRowsCount { 0 };
    bool mHasRows { false };
};

//! A class representing a context of a run operation
/*!
 * Indices are 1-based
 */
class SQLITE_HELPERS_API RunContext final
{
    friend class Statement;
    explicit RunContext(StatementHandlePtr statement) noexcept;

public:
    RunContext(const RunContext&) = delete;
    RunContext(RunContext&&) noexcept;
    RunContext& operator=(const RunContext&) = delete;
    RunContext& operator=(RunContext&&) noexcept;

    RunContext&
    Bind(int index, const void* data, int64_t size, bool makeCopy = true);

    RunContext& Bind(int index, const std::string& value, bool makeCopy = true);
    RunContext& Bind(int index, std::string_view value, bool makeCopy = true);
    RunContext& Bind(int index, const char* value, bool makeCopy = true);

    RunContext& Bind(int index, bool value);
    RunContext& Bind(int index, int value);
    RunContext& Bind(int index, long value);
    RunContext& Bind(int index, long long value);
    RunContext& Bind(int index, std::size_t value);
    RunContext& Bind(int index, float value);
    RunContext& Bind(int index, double value);

    RunContext& Bind(int index, std::nullptr_t);

    RunContext& BindZeroBlob(int index, int64_t size);

    template<typename T> RunContext& Bind(const std::string& name, const T& value)
    {
        return Bind(GetParameterIndex(name), value);
    }

    template<typename T>
    RunContext& Bind(const std::string& name, const T& value, bool make_copy)
    {
        return Bind(GetParameterIndex(name), value, make_copy);
    }

    template<typename ... Args> RunContext& BindAll(Args&&... args)
    {
        int index = 0;
        (Bind(++index, std::forward<Args>(args)), ...);
        return *this;
    }

    int GetParameterIndex(const std::string& name) const noexcept;

    RunResult Run();

private:
    template<typename Binder> RunContext& DoBind(Binder binder);

    StatementHandlePtr mStatement {};
    std::vector<Error> mErrors {};
    bool mNeedsReset { false };
};

//! A class representing a compiled statement
/*!
 * The usage pattern is as follows:
 * 1. Create a Statement object using Connection::CreateStatement
 * 2. Prepare the statement using Statement::Prepare
 * 3. Bind the parameters using RunContext::Bind
 * 4. Execute the statement using RunContext::Run
 * 5. Iterate over the result set using RunResult::begin and RunResult::end
 */
class SQLITE_HELPERS_API Statement final
{
    explicit Statement(sqlite3_stmt* stmt);

public:

    Statement(const Statement&) = delete;
    Statement(Statement&&) noexcept;

    Statement& operator=(const Statement&) = delete;
    Statement& operator=(Statement&&) noexcept;

    RunContext& Prepare() noexcept;

    template<typename ... Args>
    RunContext& Prepare(Args&&... args)
    {
        return Prepare().BindAll(std::forward<Args>(args)...);
    }

private:
    StatementHandlePtr mStatement {};
    std::optional<RunContext> mRunContext {};

    friend class Connection;
};
} // namespace audacity::sqlite
