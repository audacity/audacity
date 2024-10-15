/*
* Audacity: A Digital Audio Editor
*/
#pragma once

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
