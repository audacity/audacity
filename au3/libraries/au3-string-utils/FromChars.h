/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file FromChars.h
 @brief Declare functions to convert numeric types to string representation.

 Dmitry Vedenko
 **********************************************************************/

#pragma once

#include <system_error>

//! Result of the conversion, similar to std::from_chars_result
struct STRING_UTILS_API FromCharsResult final
{
    const char* ptr; //! A pointer to the first character not matching the pattern

    //! std::errc::invalid_argument, if there is no pattern match,
    //! std::errc::result_out_of_range if the value parsed is too large,
    //! std::errc() on success
    std::errc ec;
};

//! Parse a string into a single precision floating point value, always uses the dot as decimal
STRING_UTILS_API FromCharsResult FromChars(const char* buffer, const char* last, float& value) noexcept;

//! Parse a string into a single precision floating point value, always uses the dot as decimal
STRING_UTILS_API FromCharsResult FromChars(const char* buffer, const char* last, double& value) noexcept;

//! Parse a string into a signed 16-bit integer value
STRING_UTILS_API FromCharsResult FromChars(const char* buffer, const char* last, short& value) noexcept;
//! Parse a string into an unsigned 16-bit integer value
STRING_UTILS_API FromCharsResult FromChars(const char* buffer, const char* last, unsigned short& value) noexcept;

//! Parse a string into a signed 32-bit integer value
STRING_UTILS_API FromCharsResult FromChars(const char* buffer, const char* last, int& value) noexcept;
//! Parse a string into an unsigned 32-bit integer value
STRING_UTILS_API FromCharsResult FromChars(const char* buffer, const char* last, unsigned int& value) noexcept;

//! Parse a string into a signed long integer value
STRING_UTILS_API FromCharsResult FromChars(const char* buffer, const char* last, long& value) noexcept;
//! Parse a string into an unsigned long integer value
STRING_UTILS_API FromCharsResult FromChars(const char* buffer, const char* last, unsigned long& value) noexcept;

//! Parse a string into a signed 64-bit integer value
STRING_UTILS_API FromCharsResult FromChars(const char* buffer, const char* last, long long& value) noexcept;
//! Parse a string into an unsigned 64-bit integer value
STRING_UTILS_API FromCharsResult FromChars(const char* buffer, const char* last, unsigned long long& value) noexcept;
//! Parse a string into a boolean value. String must be "0" or "1".
STRING_UTILS_API FromCharsResult FromChars(const char* buffer, const char* last, bool& value) noexcept;
