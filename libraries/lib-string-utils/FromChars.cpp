/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file FromChars.cpp
 @brief Define functions to convert numeric types to string representation.

 Dmitry Vedenko
 **********************************************************************/

#include "FromChars.h"

#include "3party/fast_float.h"

#include <limits>
#include <algorithm>

namespace
{
// Converting c to unsigned here helps to eliminate one check 
unsigned digitToInt(char c) noexcept
{
   return static_cast<unsigned>(c) - '0';
}

template <typename T>
bool safeMul10Add(T& result, T a, T b)
{
   static_assert(std::is_unsigned_v<T>);

   constexpr auto bits = sizeof(a) * 8 - 3;
   // Check for the top 3 bits of a
   if (a >> bits)
      return false;

   const T times8 = a << 3;
   // a << 1 won't overflow
   const T times2 = a << 1;

   // timesX are all unsigned values, if overflow occurs - times10 will be <
   // times8
   const T times10 = times8 + times2;

   if (times10 < times8)
      return false;

   const T tempResult = times10 + b;

   if (tempResult < times10)
      return false;

   result = tempResult;

   return true;
}

template <typename ResultType>
FromCharsResult FastStringToInt(
   const char* first, const char* last, ResultType& value,
   bool isNegative) noexcept
{
   using UnsignedResultType = std::make_unsigned_t<ResultType>;

   const auto availableBytes = last - first;

   if (availableBytes <= 0)
      return { first, std::errc::invalid_argument };

   UnsignedResultType result = digitToInt(*first);

   if (result > 10)
      return { first, std::errc::invalid_argument };

   constexpr auto maxSafeDigits = std::numeric_limits<ResultType>::digits10;

   const char* ptr = first;
   const char* safeLast =
      first + std::min<decltype(availableBytes)>(availableBytes, maxSafeDigits);

   unsigned d;

   // No integer overflow here
   while (++ptr < safeLast && (d = digitToInt(*ptr)) <= 9)
   {
      result = result * 10 + d;
   }

   // But here live dragons
   while (ptr < last && (d = digitToInt(*ptr++)) <= 9)
   {
      if (!safeMul10Add<UnsignedResultType>(result, result, d))
         return { ptr, std::errc::result_out_of_range };

      // Even if there were no unsigned overflow,
      // signed overflow is still possible
      if constexpr (std::is_signed_v<ResultType>)
      {
         const UnsignedResultType max =
            static_cast<UnsignedResultType>(
               std::numeric_limits<ResultType>::max()) +
            (isNegative ? 1 : 0);

         if (result > max)
            return { ptr, std::errc::result_out_of_range };
      }
   }

   if constexpr (std::is_signed_v<ResultType>)
   {
      value = isNegative ?
                 // Unary minus on unsigned type makes MSVC unhappy
                 static_cast<ResultType>(0 - result) :
                 static_cast<ResultType>(result);
   }
   else
      value = static_cast<ResultType>(result);

   return { ptr, std::errc() };
}

template <typename ResultType>
FromCharsResult
IntFromChars(const char* buffer, const char* last, ResultType& value) noexcept
{
   const char* origin = buffer;

   if (buffer >= last)
      return { buffer, std::errc::invalid_argument };

   const bool isNegative = *buffer == '-';

   if (isNegative)
   {
      if constexpr (std::is_signed_v<ResultType>)
         ++buffer;
      else
         return { origin, std::errc::invalid_argument };
   }

   const auto fastStringResult =
      FastStringToInt(buffer, last, value, isNegative);

   if (fastStringResult.ec == std::errc::invalid_argument)
      return { origin, std::errc::invalid_argument };

   return fastStringResult;
}
} // namespace

FromCharsResult FromChars(const char* buffer, const char* last, float& value) noexcept
{
   const auto result = fast_float::from_chars(buffer, last, value);
   return { result.ptr, result.ec };
}

FromCharsResult FromChars(const char* buffer, const char* last, double& value) noexcept
{
   const auto result = fast_float::from_chars(buffer, last, value);
   return { result.ptr, result.ec };
}

FromCharsResult FromChars(const char* buffer, const char* last, short& value) noexcept
{
   return IntFromChars(buffer, last, value);
}

FromCharsResult FromChars(const char* buffer, const char* last, unsigned short& value) noexcept
{
   return IntFromChars(buffer, last, value);
}

FromCharsResult FromChars(const char* buffer, const char* last, int& value) noexcept
{
   return IntFromChars(buffer, last, value);
}

FromCharsResult FromChars(const char* buffer, const char* last, unsigned int& value) noexcept
{
   return IntFromChars(buffer, last, value);
}

FromCharsResult FromChars(const char* buffer, const char* last, long& value) noexcept
{
   return IntFromChars(buffer, last, value);
}

FromCharsResult FromChars(const char* buffer, const char* last, unsigned long& value) noexcept
{
   return IntFromChars(buffer, last, value);
}

FromCharsResult FromChars(const char* buffer, const char* last, long long& value) noexcept
{
   return IntFromChars(buffer, last, value);
}

FromCharsResult FromChars(const char* buffer, const char* last, unsigned long long& value) noexcept
{
   return IntFromChars(buffer, last, value);
}

FromCharsResult
FromChars(const char* buffer, const char* last, bool& value) noexcept
{
   if (buffer >= last)
      return { buffer, std::errc::invalid_argument };

   if (buffer[0] == '0')
   {
      value = false;
      return { buffer, std::errc() };
   }
   else if (buffer[0] == '1')
   {
      value = true;
      return { buffer, std::errc() };
   }

   return { buffer, std::errc::invalid_argument };
}
