/**********************************************************************

  Audacity: A Digital Audio Editor

  Gain.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include <type_traits>
#include <cmath>
#include <limits>

// Implementation is based on https://www.dr-lex.be/info-stuff/volumecontrols.html
template<typename Type>
Type ExpGain(Type gain) noexcept
{
    static_assert(std::is_floating_point_v<Type>);

    constexpr Type a(1e-3);
    constexpr Type b(6.908);

    if (gain < std::numeric_limits<Type>::epsilon()) {
        return {};
    }

    const Type expGain = a * std::exp(b * gain);

    if (expGain > Type(1)) {
        return Type(1);
    }

    return expGain;
}
