#ifndef AUDACITY_CLAMP
#define AUDACITY_CLAMP
#include <cassert>

// May be replaced with C++17 std::clamp once C++17 is default.
template <typename T>
constexpr const T& Clamp(const T& v, const T& min, const T& max)
{
    return assert(min <= max), v < min ? min : max < v ? max : v;
}

#endif //AUDACITY_CLAMP
