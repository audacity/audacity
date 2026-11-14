/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: Function.h
 * SPDX-FileContributor: Dmitry Vedenko
 */

#pragma once

#include <functional>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>

struct sqlite3;
struct sqlite3_value;
struct sqlite3_context;

namespace audacity::sqlite {
namespace details {
using SQLiteFunctorWithArgs
    =std::function<void (sqlite3_context*, int, sqlite3_value**)>;

using SQLiteFunctor= std::function<void (sqlite3_context*)>;

SQLITE_HELPERS_API void FromSQLiteValue(sqlite3_value& value, bool& result);
SQLITE_HELPERS_API void FromSQLiteValue(sqlite3_value& value, int& result);
SQLITE_HELPERS_API void FromSQLiteValue(sqlite3_value& value, unsigned int& result);
SQLITE_HELPERS_API void FromSQLiteValue(sqlite3_value& value, long& result);
SQLITE_HELPERS_API void FromSQLiteValue(sqlite3_value& value, unsigned long& result);
SQLITE_HELPERS_API void FromSQLiteValue(sqlite3_value& value, long long& result);
SQLITE_HELPERS_API void FromSQLiteValue(sqlite3_value& value, unsigned long long& result);
SQLITE_HELPERS_API void FromSQLiteValue(sqlite3_value& value, double& result);
SQLITE_HELPERS_API void FromSQLiteValue(sqlite3_value& value, float& result);
SQLITE_HELPERS_API void FromSQLiteValue(sqlite3_value& value, std::string& result);
SQLITE_HELPERS_API void FromSQLiteValue(sqlite3_value& value, std::string_view& result);

SQLITE_HELPERS_API void SetSQLiteFunctionResult(sqlite3_context* context, bool value);
SQLITE_HELPERS_API void SetSQLiteFunctionResult(sqlite3_context* context, int value);
SQLITE_HELPERS_API void SetSQLiteFunctionResult(sqlite3_context* context, unsigned int value);
SQLITE_HELPERS_API void SetSQLiteFunctionResult(sqlite3_context* context, long value);
SQLITE_HELPERS_API void SetSQLiteFunctionResult(sqlite3_context* context, unsigned long value);
SQLITE_HELPERS_API void SetSQLiteFunctionResult(sqlite3_context* context, long long value);
SQLITE_HELPERS_API void SetSQLiteFunctionResult(sqlite3_context* context, unsigned long long value);
SQLITE_HELPERS_API void SetSQLiteFunctionResult(sqlite3_context* context, double value);
SQLITE_HELPERS_API void SetSQLiteFunctionResult(sqlite3_context* context, float value);
SQLITE_HELPERS_API void SetSQLiteFunctionResult(sqlite3_context* context, const std::string& value);
SQLITE_HELPERS_API void SetSQLiteFunctionResult(sqlite3_context* context, const std::string_view& value);

SQLITE_HELPERS_API void
SetSQLiteFunctionError(sqlite3_context* context, const std::string_view& error);

template<typename T>
std::decay_t<T> FromSQLiteValue(sqlite3_value& value)
{
    std::decay_t<T> result;
    FromSQLiteValue(value, result);
    return result;
}

template<typename ... Args, std::size_t... Is>
auto SQLiteValuesToTuple(sqlite3_value** values, std::index_sequence<Is...>)
{
    return std::make_tuple(FromSQLiteValue<Args>(*values[Is])...);
}

template<typename CallbackType>
struct SQLiteFunction {};

template<typename R, typename ... Args>
struct SQLiteFunction<std::function<R(Args...)> >
{
    static constexpr std::size_t Arity = sizeof...(Args);

    static SQLiteFunctorWithArgs ToSQLiteFunctorWithArgs(std::function<R(Args...)> callback)
    {
        return [function = std::move(callback)](
            sqlite3_context* context, int argc, sqlite3_value** argv)
        {
            if (argc != sizeof...(Args)) {
                SetSQLiteFunctionError(context, "Invalid number of arguments");
                return;
            }

            if constexpr (std::is_same_v<R, void>) {
                std::apply(
                    function, SQLiteValuesToTuple<Args...>(
                        argv, std::make_index_sequence<sizeof...(Args)>()));
            } else {
                SetSQLiteFunctionResult(
                    context,
                    std::apply(
                        function,
                        SQLiteValuesToTuple<Args...>(
                            argv, std::make_index_sequence<sizeof...(Args)>())));
            }
        };
    }
};

template<typename R> struct SQLiteFunction<std::function<R()> >
{
    static SQLiteFunctor ToSQLiteFunctor(std::function<R()> callback)
    {
        return [function = std::move(callback)](sqlite3_context* context)
        {
            if constexpr (std::is_same_v<R, void>) {
                function();
            } else {
                SetSQLiteFunctionResult(context, function());
            }
        };
    }
};

template<typename CallbackType>
auto MakeSQLiteFunctorWithArgs(CallbackType callback)
{
    using FunctionType = decltype(std::function { callback });
    return SQLiteFunction<FunctionType>::ToSQLiteFunctorWithArgs(
        std::move(callback));
}

template<typename CallbackType>
constexpr std::size_t GetFunctionArity()
{
    using FunctionType = decltype(std::function { std::declval<CallbackType>() });
    return SQLiteFunction<FunctionType>::Arity;
}

template<typename CallbackType>
auto MakeSQLiteFunctor(CallbackType callback)
{
    using FunctionType = decltype(std::function { callback });
    return SQLiteFunction<FunctionType>::ToSQLiteFunctor(
        std::move(callback));
}
} // namespace details

//! A class representing a scalar function in a SQLite database
class SQLITE_HELPERS_API ScalarFunction final
{
    template<typename ScalarFunctionType>
    ScalarFunction(
        sqlite3* connection, std::string name, ScalarFunctionType function)
        : mConnection{connection}
        , mName{std::move(name)}
        , mFunctor{details::MakeSQLiteFunctorWithArgs(std::move(function))}
    {
        Register(details::GetFunctionArity<ScalarFunctionType>());
    }

public:
    ScalarFunction() = default;
    ScalarFunction(const ScalarFunction&) = delete;
    ScalarFunction(ScalarFunction&&) noexcept;
    ScalarFunction& operator=(const ScalarFunction&) = delete;
    ScalarFunction& operator=(ScalarFunction&&) noexcept;

    ~ScalarFunction();

private:
    void Register(std::size_t arity);

    static void CallFunction(sqlite3_context* context, int argc, sqlite3_value** argv);

    sqlite3* mConnection { nullptr };
    std::string mName;
    details::SQLiteFunctorWithArgs mFunctor;
    friend class Connection;
};

//! A class representing an aggregate function in a SQLite database
class SQLITE_HELPERS_API AggregateFunction final
{
    template<typename StepFunctionType, typename FinalFunctionType>
    AggregateFunction(
        sqlite3* connection, std::string name, StepFunctionType stepFunction,
        FinalFunctionType finalFunction)
        : mConnection{connection}
        , mName{std::move(name)}
        , mStepFunctor{details::MakeSQLiteFunctorWithArgs(
                           std::move(stepFunction))}
        , mFinalFunctor{details::MakeSQLiteFunctor(std::move(finalFunction))}
    {
        Register(details::GetFunctionArity<StepFunctionType>());
    }

public:

    AggregateFunction() = default;
    AggregateFunction(const AggregateFunction&) = delete;
    AggregateFunction(AggregateFunction&&) noexcept;
    AggregateFunction& operator=(const AggregateFunction&) = delete;
    AggregateFunction& operator=(AggregateFunction&&) noexcept;

    ~AggregateFunction();

private:
    void Register(std::size_t arity);

    static void CallStepFunction(
        sqlite3_context* context, int argc, sqlite3_value** argv);

    static void CallFinalFunction(sqlite3_context* context);

    sqlite3* mConnection { nullptr };
    std::string mName;
    details::SQLiteFunctorWithArgs mStepFunctor;
    details::SQLiteFunctor mFinalFunctor;
    friend class Connection;
};
} // namespace audacity::sqlite
