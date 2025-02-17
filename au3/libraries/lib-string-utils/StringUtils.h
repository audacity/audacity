/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: StringUtils.h
 * SPDX-FileContributor: Dmitry Vedenko
 */

#pragma once

#include <algorithm>
#include <cstring>
#include <cwchar>
#include <string>
#include <string_view>
#include <type_traits>
#include <numeric>

#include <wx/string.h>

inline std::size_t StringLength(const char str)
{
    return 1;
}

inline std::size_t StringLength(const wchar_t str)
{
    return 1;
}

inline std::size_t StringLength(const char* str)
{
    return std::strlen(str);
}

inline std::size_t StringLength(const wchar_t* str)
{
    return std::wcslen(str);
}

template<typename CharType, size_t N>
std::size_t StringLength(const CharType (&)[N])
{
    return N - 1;
}

template<typename StringType>
std::size_t StringLength(const StringType& str)
{
    return str.length();
}

template<template < typename, typename ...> typename ContainerType, typename ResultType, typename SeparatorType, typename ... Rest >
         ResultType Join(
             const ContainerType<ResultType, Rest...>& container,
                                 const SeparatorType& separator)
         {
             if (container.empty()) {
                 return ResultType {}
             }

             const auto sepratorLength = StringLength(separator);
             const auto totalSeparatorLength = sepratorLength * (container.size() - 1);

             ResultType result;

             const auto size = std::accumulate(
                 container.begin(), container.end(), totalSeparatorLength,
                 [](size_t size, const ResultType& item)
    { return size + StringLength(item); });

             result.reserve(size);

             bool first = true;
             for (const auto& item : container) {
                 if (!first) {
                     result += separator;
                 }
                 result += item;
                 first = false;
             }

             return result;
         }

         STRING_UTILS_API std::string  ToLower(const std::string& str);
         STRING_UTILS_API std::string  ToLower(const std::string_view& str);
         STRING_UTILS_API std::string  ToLower(const char* str);
         STRING_UTILS_API std::wstring ToLower(const std::wstring& str);
         STRING_UTILS_API std::wstring ToLower(const std::wstring_view& str);
         STRING_UTILS_API std::wstring ToLower(const wchar_t* str);
         STRING_UTILS_API wxString     ToLower(const wxString& str);

         STRING_UTILS_API std::string  ToUpper(const std::string& str);
         STRING_UTILS_API std::string  ToUpper(const std::string_view& str);
         STRING_UTILS_API std::string  ToUpper(const char* str);
         STRING_UTILS_API std::wstring ToUpper(const std::wstring& str);
         STRING_UTILS_API std::wstring ToUpper(const std::wstring_view& str);
         STRING_UTILS_API std::wstring ToUpper(const wchar_t* str);
         STRING_UTILS_API wxString     ToUpper(const wxString& str);

         namespace details {
             inline const char* begin(const char* str) noexcept
             {
                 return str;
             }

             inline const char* end(const char* str) noexcept
             {
                 if (str == nullptr) {
                     return nullptr;
                 }

                 return str + StringLength(str);
             }

             inline const wchar_t* begin(const wchar_t* str) noexcept
             {
                 return str;
             }

             inline const wchar_t* end(const wchar_t* str) noexcept
             {
                 if (str == nullptr) {
                     return nullptr;
                 }

                 return str + StringLength(str);
             }
         } // namespace details

         template<typename HayType, typename PrefixType>
bool IsPrefixed(
    const HayType& hay, const PrefixType& prefix)
{
    if (StringLength(hay) < StringLength(prefix)) {
        return false;
    }

    using namespace std;
    using namespace details;

    const auto prefixBegin = begin(prefix);
    const auto prefixEnd = end(prefix);
    const auto hayBegin = begin(hay);

    return std::mismatch(prefixBegin, prefixEnd, hayBegin).first == prefixEnd;
}

template<typename HayType, typename PrefixType>
bool IsPrefixedInsensitive(const HayType& hay, const PrefixType& prefix)
{
    return IsPrefixed(ToLower(hay), ToLower(prefix));
}
