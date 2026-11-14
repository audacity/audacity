/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: Function.cpp
 * SPDX-FileContributor: Dmitry Vedenko
 */

#include "Function.h"

#include <algorithm>

#include "sqlite3.h"

namespace audacity::sqlite {
namespace details {
void FromSQLiteValue(sqlite3_value& value, bool& result)
{
    result = sqlite3_value_int(&value) != 0;
}

void FromSQLiteValue(sqlite3_value& value, int& result)
{
    result = sqlite3_value_int(&value);
}

void FromSQLiteValue(sqlite3_value& value, unsigned int& result)
{
    result = std::max({}, sqlite3_value_int(&value));
}

void FromSQLiteValue(sqlite3_value& value, long& result)
{
    if (sizeof(long) == sizeof(int)) {
        result = sqlite3_value_int(&value);
    } else {
        result = sqlite3_value_int64(&value);
    }
}

void FromSQLiteValue(sqlite3_value& value, unsigned long& result)
{
    if (sizeof(unsigned long) == sizeof(int)) {
        result = std::max({}, sqlite3_value_int(&value));
    } else {
        result = std::max({}, sqlite3_value_int64(&value));
    }
}

void FromSQLiteValue(sqlite3_value& value, long long& result)
{
    result = sqlite3_value_int64(&value);
}

void FromSQLiteValue(sqlite3_value& value, unsigned long long& result)
{
    result = std::max({}, sqlite3_value_int64(&value));
}

void FromSQLiteValue(sqlite3_value& value, double& result)
{
    result = sqlite3_value_double(&value);
}

void FromSQLiteValue(sqlite3_value& value, float& result)
{
    result = static_cast<float>(sqlite3_value_double(&value));
}

void FromSQLiteValue(sqlite3_value& value, std::string& result)
{
    const auto* text = reinterpret_cast<const char*>(sqlite3_value_text(&value));
    const auto length = sqlite3_value_bytes(&value);

    result.assign(text, length);
}

void FromSQLiteValue(sqlite3_value& value, std::string_view& result)
{
    const auto* text = reinterpret_cast<const char*>(sqlite3_value_text(&value));
    const auto length = sqlite3_value_bytes(&value);

    result = std::string_view(text, length);
}

void SetSQLiteFunctionResult(sqlite3_context* context, bool value)
{
    sqlite3_result_int(context, value ? 1 : 0);
}

void SetSQLiteFunctionResult(sqlite3_context* context, int value)
{
    sqlite3_result_int(context, value);
}

void SetSQLiteFunctionResult(sqlite3_context* context, unsigned int value)
{
    sqlite3_result_int(context, value);
}

void SetSQLiteFunctionResult(sqlite3_context* context, long value)
{
    if (sizeof(long) == sizeof(int)) {
        sqlite3_result_int(context, value);
    } else {
        sqlite3_result_int64(context, value);
    }
}

void SetSQLiteFunctionResult(sqlite3_context* context, unsigned long value)
{
    if (sizeof(long) == sizeof(int)) {
        sqlite3_result_int(context, value);
    } else {
        sqlite3_result_int64(context, value);
    }
}

void SetSQLiteFunctionResult(sqlite3_context* context, long long value)
{
    sqlite3_result_int64(context, value);
}

void SetSQLiteFunctionResult(sqlite3_context* context, unsigned long long value)
{
    sqlite3_result_int64(context, value);
}

void SetSQLiteFunctionResult(sqlite3_context* context, double value)
{
    sqlite3_result_double(context, value);
}

void SetSQLiteFunctionResult(sqlite3_context* context, float value)
{
    sqlite3_result_double(context, value);
}

void SetSQLiteFunctionResult(sqlite3_context* context, const std::string& value)
{
    sqlite3_result_text(context, value.c_str(), value.size(), SQLITE_TRANSIENT);
}

void SetSQLiteFunctionResult(
    sqlite3_context* context, const std::string_view& value)
{
    sqlite3_result_text(context, value.data(), value.size(), SQLITE_TRANSIENT);
}

void SetSQLiteFunctionError(
    sqlite3_context* context, const std::string_view& error)
{
    sqlite3_result_error(context, error.data(), error.size());
}
} // namespace details

ScalarFunction::ScalarFunction(ScalarFunction&& rhs) noexcept
{
    *this = std::move(rhs);
}

ScalarFunction& ScalarFunction::operator=(ScalarFunction&& rhs) noexcept
{
    std::swap(mConnection, rhs.mConnection);
    std::swap(mName, rhs.mName);
    std::swap(mFunctor, rhs.mFunctor);

    return *this;
}

ScalarFunction::~ScalarFunction()
{
    if (mConnection) {
        sqlite3_create_function(
            mConnection, mName.c_str(), 0, SQLITE_UTF8, nullptr,
            nullptr, nullptr, nullptr);
    }
}

void ScalarFunction::Register(std::size_t arity)
{
    sqlite3_create_function(
        mConnection, mName.c_str(), arity, SQLITE_UTF8, this,
        &CallFunction, nullptr, nullptr);
}

void ScalarFunction::CallFunction(
    sqlite3_context* context, int argc, sqlite3_value** argv)
{
    auto* function = static_cast<ScalarFunction*>(sqlite3_user_data(context));
    function->mFunctor(context, argc, argv);
}

void AggregateFunction::Register(std::size_t arity)
{
    sqlite3_create_function(
        mConnection, mName.c_str(), arity, SQLITE_UTF8, this,
        nullptr, &CallStepFunction, &CallFinalFunction);
}

void AggregateFunction::CallStepFunction(
    sqlite3_context* context, int argc, sqlite3_value** argv)
{
    auto* function = static_cast<AggregateFunction*>(sqlite3_user_data(context));
    function->mStepFunctor(context, argc, argv);
}

void AggregateFunction::CallFinalFunction(sqlite3_context* context)
{
    auto* function = static_cast<AggregateFunction*>(sqlite3_user_data(context));
    function->mFinalFunctor(context);
}

AggregateFunction::AggregateFunction(AggregateFunction&& rhs) noexcept
{
    *this = std::move(rhs);
}

AggregateFunction& AggregateFunction::operator=(AggregateFunction&& rhs) noexcept
{
    std::swap(mConnection, rhs.mConnection);
    std::swap(mName, rhs.mName);
    std::swap(mStepFunctor, rhs.mStepFunctor);
    std::swap(mFinalFunctor, rhs.mFinalFunctor);

    return *this;
}

AggregateFunction::~AggregateFunction()
{
    if (mConnection) {
        sqlite3_create_function(
            mConnection, mName.c_str(), 0, SQLITE_UTF8, nullptr,
            nullptr, nullptr, nullptr);
    }
}
} // namespace audacity::sqlite
