/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: Error.h
 * SPDX-FileContributor: Dmitry Vedenko
 */

#pragma once

#include <string>

class TranslatableString;

namespace sqlite
{
class SQLITE_HELPERS_API Error final
{
public:
   Error() noexcept;
   explicit Error(int code) noexcept;

   Error(const Error&) = default;
   Error(Error&&) = default;

   Error& operator=(const Error&) = default;
   Error& operator=(Error&&) = default;

   bool IsError() const noexcept;
   bool IsOk() const noexcept;
   explicit operator bool() const noexcept;

   [[noreturn]] void Raise () const;

   int GetCode() const noexcept;
   TranslatableString GetErrorString() const;

private:
   int mCode;
};
} // namespace sqlite
