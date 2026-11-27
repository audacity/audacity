/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "au3-components/SettingsVisitor.h"

#include <algorithm>
#include <cmath>
#include <functional>
#include <type_traits>

namespace au::effects {
//! Reads and writes one numeric effect setting as a double, whatever the type of its member
template<typename Settings>
struct SettingAccess
{
    std::function<double(const Settings&)> get;
    //! Keeps the value in the range, and rounds it for an integer setting
    std::function<void(Settings&, double)> set;
    double min = 0.0;
    double max = 0.0;
    double step = 0.0;
    double def = 0.0;
    int decimals = 0;
};

//! `adjust` is applied to the value once it is in the range, e.g. to keep it even
template<typename Settings, typename Member, typename Type, typename Value>
SettingAccess<Settings> makeSettingAccess(const EffectParameter<Settings, Member, Type, Value>& param,
                                          std::function<double(double)> adjust = nullptr)
{
    const auto mem = param.mem;
    const auto min = static_cast<double>(param.min);
    const auto max = static_cast<double>(param.max);

    SettingAccess<Settings> access;
    access.get = [mem](const Settings& settings) { return static_cast<double>(settings.*mem); };
    access.set = [mem, min, max, adjust = std::move(adjust)](Settings& settings, double value) {
        value = std::clamp(value, min, max);
        if (adjust) {
            value = adjust(value);
        }
        if constexpr (std::is_integral_v<Member>) {
            settings.*mem = static_cast<Member>(std::lround(value));
        } else {
            settings.*mem = static_cast<Member>(value);
        }
    };
    access.min = min;
    access.max = max;
    access.step = static_cast<double>(param.step);
    access.def = static_cast<double>(param.def);
    access.decimals = param.decimals;
    return access;
}
}
