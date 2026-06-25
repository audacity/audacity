/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/global/types/number.h"
#include "realfn.h"

namespace au::shared {
class Decibel
{
public:
    constexpr Decibel() = default;
    explicit constexpr Decibel(double dB)
        : m_dB(dB) {}

    static Decibel fromLinear(double linear);

    constexpr double raw() const { return m_dB; }
    double toLinear() const;

    Decibel operator+(Decibel rhs) const { return Decibel { m_dB + rhs.m_dB }; }

    bool operator==(Decibel rhs) const { return muse::is_equal(m_dB, rhs.m_dB); }
    bool operator!=(Decibel rhs) const { return !muse::is_equal(m_dB, rhs.m_dB); }
    bool operator<(Decibel rhs) const { return muse::RealIsEqualOrLess(m_dB, rhs.m_dB) && !muse::is_equal(m_dB, rhs.m_dB); }
    bool operator<=(Decibel rhs) const { return muse::RealIsEqualOrLess(m_dB, rhs.m_dB); }
    bool operator>(Decibel rhs) const { return muse::RealIsEqualOrLess(rhs.m_dB, m_dB) && !muse::is_equal(m_dB, rhs.m_dB); }
    bool operator>=(Decibel rhs) const { return muse::RealIsEqualOrLess(rhs.m_dB, m_dB); }

private:
    double m_dB = 0.0;
};
}
