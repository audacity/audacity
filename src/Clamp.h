#ifndef AUDACITY_CLAMP
#define AUDACITY_CLAMP

// May be replaced with C++17 std::clamp once C++17 is default.
template <typename T>
const T& Clamp(const T& v, const T& min, const T& max)
{
    return v < min ? min : max < v ? max : v;
}

#endif //AUDACITY_CLAMP
