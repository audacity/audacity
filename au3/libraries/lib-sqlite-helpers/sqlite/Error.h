/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: Error.h
 * SPDX-FileContributor: Dmitry Vedenko
 */

#pragma once

#include <string>

class TranslatableString;

namespace audacity::sqlite {
//! A class representing an error in SQLite
class SQLITE_HELPERS_API Error final
{
public:
    Error() noexcept;
    explicit Error(int code) noexcept;

    Error(const Error&) = default;
    Error(Error&&) = default;

    Error& operator=(const Error&) = default;
    Error& operator=(Error&&) = default;

    //! Returns true if the object represents an error
    bool IsError() const noexcept;
    //! Returns true if the object represents a success code
    bool IsOk() const noexcept;
    //! Returns true if the object represents a success code
    explicit operator bool() const noexcept;

    [[noreturn]] void Raise() const;

    int GetCode() const noexcept;
    TranslatableString GetErrorString() const;

private:
    int mCode;
};
} // namespace audacity::sqlite
