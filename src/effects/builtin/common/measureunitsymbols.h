/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QString>
#include <utility>

#include "framework/global/translation.h"

namespace au::effects {
/**
 * @brief Represents a measurement unit with symbol and full name
 */
struct Unit
{
    QString m_symbol;
    QString m_name;

    Unit(QString symbol, QString name)
        : m_symbol(std::move(symbol)), m_name(std::move(name)) {}
};

/**
 * @brief Centralized measure units for effect parameters
 *
 * Organized by category (Time, Frequency, Level, etc.)
 * Each unit has both a symbol (e.g., "s") and a full name (e.g., "seconds")
 */
namespace units {
// Time units
inline Unit seconds()
{
    return {
        muse::qtrc("effects/units", "s"),
        muse::qtrc("effects/units", "seconds")
    };
}

inline Unit milliseconds()
{
    return {
        muse::qtrc("effects/units", "ms"),
        muse::qtrc("effects/units", "milliseconds")
    };
}

inline Unit samples()
{
    return {
        muse::qtrc("effects/units", "samples"),
        muse::qtrc("effects/units", "samples")
    };
}

// Frequency units
inline Unit hertz()
{
    return {
        muse::qtrc("effects/units", "Hz"),
        muse::qtrc("effects/units", "hertz")
    };
}

inline Unit kilohertz()
{
    return {
        muse::qtrc("effects/units", "kHz"),
        muse::qtrc("effects/units", "kilohertz")
    };
}

// Level units
inline Unit decibels()
{
    return {
        muse::qtrc("effects/units", "dB"),
        muse::qtrc("effects/units", "decibels")
    };
}

// Percentage units
inline Unit percent()
{
    return {
        muse::qtrc("effects/units", "%"),
        muse::qtrc("effects/units", "percent")
    };
}

// Pitch units
inline Unit semitones()
{
    return {
        muse::qtrc("effects/units", "semitones"),
        muse::qtrc("effects/units", "semitones")
    };
}

// Tempo units
inline Unit beatsPerMinute()
{
    return {
        muse::qtrc("effects/units", "bpm"),
        muse::qtrc("effects/units", "beats per minute")
    };
}

// Angle units
inline Unit degrees()
{
    return {
        muse::qtrc("effects/units", "°"),
        muse::qtrc("effects/units", "degrees")
    };
}
} // namespace units
}
