/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file ToChars.h
 @brief Declare functions to convert numeric types to string representation.

 Dmitry Vedenko
 **********************************************************************/

#pragma once

#include <system_error>

//! Result of the conversion, similar to std::to_chars_result
struct STRING_UTILS_API ToCharsResult final
{
    char* ptr; //! A pointer to the last updated character, or last on failure
    std::errc ec; //! std::errc::value_too_large on failure, std::errc() on success
};

//! Convert a single precision floating point number to a string, always uses the dot as decimal
STRING_UTILS_API ToCharsResult ToChars(
    char* buffer, char* last, float value, int digitsAfterDecimalPoint = -1) noexcept;

//! Convert a double precision floating point number to a string, always uses the dot as decimal
STRING_UTILS_API ToCharsResult ToChars(
    char* buffer, char* last, double value, int digitsAfterDecimalPoint = -1) noexcept;

//! Convert a signed 64 bit integer value to string
STRING_UTILS_API ToCharsResult ToChars(char* buffer, char* last, long long value) noexcept;
//! Convert a unsigned 64 bit integer value to string
STRING_UTILS_API ToCharsResult ToChars(char* buffer, char* last, unsigned long long value) noexcept;
