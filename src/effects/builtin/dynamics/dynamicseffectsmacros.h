/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#define WIDEN(x) L##x
#define DYNAMICS_EFFECT_PARAM(settingsName, paramName, paramMin, paramDefault, paramMax, paramStep) \
    static constexpr EffectParameter paramName { \
        &settingsName::paramName, \
        WIDEN(#paramName), \
        paramDefault, \
        paramMin, \
        paramMax, \
        paramStep \
    }; \
    static_assert(paramName.min <= paramName.def && paramName.def <= paramName.max, \
                  "EffectParameter " #paramName " has default outside range");
