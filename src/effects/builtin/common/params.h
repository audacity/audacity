/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "effects/effects_base/effectstypes.h" // IWYU pragma: export

namespace au::effects {
template<typename T>
struct Param
{
    T val = {};
    T min = {};
    T max = {};
};

template<typename T>
inline Param<T> make_param(const T& val, const T& min, const T& max)
{
    return { val, min, max };
}
}
