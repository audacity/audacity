/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: Result.h
 * SPDX-FileContributor: Dmitry Vedenko
 */

#pragma once

#include <type_traits>
#include <variant>

#include "Error.h"

namespace audacity::sqlite {
//! A class representing a result of an operation
template<typename T>
class Result final
{
public:
    Result() = default;

    Result(T&& value) noexcept(std::is_nothrow_move_constructible_v<T>)
        : mValue(std::forward<T>(value))
    {
    }

    Result (const Error& error) noexcept(std::is_nothrow_copy_constructible_v<Error>)
        : mValue(error)
    {
    }

    Result (Error&& error) noexcept(std::is_nothrow_move_constructible_v<Error>)
        : mValue(std::move(error))
    {
    }

    bool HasValue() const noexcept
    {
        return std::holds_alternative<T>(mValue);
    }

    T& operator*()
    &
    {
        if (!HasValue()) {
            std::get_if<Error>(&mValue)->Raise();
        }

        return *std::get_if<T>(&mValue);
    }

    const T& operator*() const&
    {
        return const_cast<Result*>(this)->operator*();
    }

    T* operator->()
    {
        return & operator*();
    }

    const T* operator->() const
    {
        return & operator*();
    }

    T && operator*()
    && {
        if (!HasValue()) {
            std::get_if<Error>(&mValue)->Raise();
        }

        return std::move(*std::get_if<T>(&mValue));
    }

    explicit operator bool() const noexcept
    {
        return HasValue();
    }

    Error GetError() const noexcept
    {
        if (HasValue()) {
            return Error();
        }

        return *std::get_if<Error>(&mValue);
    }

private:
    std::variant<Error, T> mValue;
};
} // namespace audacity::sqlite
