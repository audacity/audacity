/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: StringUtils.h
 * SPDX-FileContributor: Dmitry Vedenko
 */

#pragma once

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
