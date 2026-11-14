/*
 * Audacity: A Digital Audio Editor
 */
#include "decibel.h"

#include <cmath>

namespace au::shared {
Decibel Decibel::fromLinear(double linear)
{
    return Decibel { 20.0 * std::log10(std::abs(linear)) };
}

double Decibel::toLinear() const
{
    return std::pow(10.0, m_dB / 20.0);
}
}
