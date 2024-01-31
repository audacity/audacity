/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: StringUtils.h
 * SPDX-FileContributor: Dmitry Vedenko
 */

#pragma once

#include <algorithm>
#include <cstring>
#include <cwchar>
#include <type_traits>
#include <numeric>

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
std::size_t StringLength (const CharType (&)[N])
{
   return N - 1;
}

template<typename StringType>
std::size_t StringLength (const StringType& str)
{
   return str.length();
}

template<template<typename, typename...> typename ContainerType, typename ResultType, typename SeparatorType, typename... Rest>
ResultType Join(
   const ContainerType<ResultType, Rest...>& container,
   const SeparatorType& separator)
{
   if (container.empty())
      return ResultType {};

   const auto sepratorLength = StringLength(separator);
   const auto totalSeparatorLength = sepratorLength * (container.size() - 1);

    ResultType result;

    const auto size = std::accumulate(
       container.begin(), container.end(), totalSeparatorLength,
       [](size_t size, const ResultType& item)
       { return size + StringLength(item); });

    bool first = true;
    for (const auto& item : container)
    {
        if (!first)
            result += separator;
        result += item;
        first = false;
    }

    return result;
}

template<typename StringType>
StringType ToLower (StringType str)
{
   std::transform(begin(str), end(str), begin(str), ::tolower);
   return str;
}

template<typename StringType>
StringType ToUpper (StringType str)
{
   std::transform(begin(str), end(str), begin(str), ::toupper);
   return str;
}

namespace details
{
inline const char* begin(const char* str) noexcept
{
   return str;
}

inline const char* end(const char* str) noexcept
{
   if (str == nullptr)
      return nullptr;

   return str + StringLength(str);
}

inline const wchar_t* begin(const wchar_t* str) noexcept
{
   return str;
}

inline const wchar_t* end(const wchar_t* str) noexcept
{
   if (str == nullptr)
      return nullptr;

   return str + StringLength(str);
}
} // namespace details

template<typename HayType, typename PrefixType>
bool IsPrefixed(
   const HayType& hay, const PrefixType& prefix, const bool caseSensitive = true)
{
   if (StringLength(hay) < StringLength(prefix))
        return false;

   using namespace std;
   using namespace details;

   const auto prefixBegin = begin(prefix);
   const auto prefixEnd = end(prefix);
   const auto hayBegin = begin(hay);

   auto it = caseSensitive ?
                std::mismatch(prefixBegin, prefixEnd, hayBegin) :
                std::mismatch(
                   prefixBegin, prefixEnd, hayBegin,
                   [](auto a, auto b) { return ::tolower(a) == ::tolower(b); });

   return it.first == prefixEnd;
}
