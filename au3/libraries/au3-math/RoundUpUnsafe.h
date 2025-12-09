/**********************************************************************

  Audacity: A Digital Audio Editor

  RoundUpUnsafe.h

  Dmitry Vedenko

*******************************************************************/
#pragma once

//! Returns a rounded up integer result for numerator / denominator.
/*!
* RoundUpUnsafe(4, 2) == 2;
* RoundUpUnsafe(3, 2) == 2;
* RoundUpUnsafe(-3, 2) == -1;
*
* This function does not check if denominator is 0 of if there is an integer overflow.
*/
template<typename LType, typename RType>
auto RoundUpUnsafe(LType numerator, RType denominator) noexcept
{
    static_assert(std::is_integral_v<LType>);
    static_assert(std::is_integral_v<RType>);

    if constexpr (std::is_unsigned_v<LType> && std::is_unsigned_v<RType>) {
        return (numerator + denominator - 1) / denominator;
    } else {
        if (numerator > 0 && denominator > 0) {
            return (numerator + denominator - 1) / denominator;
        } else {
            const auto result = numerator / denominator;

            if (result < 0 || result * denominator == numerator) {
                return result;
            } else {
                return result + 1;
            }
        }
    }
}
