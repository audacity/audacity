/**********************************************************************

  Audacity: A Digital Audio Editor

  LV2Preferences.cpp

  Paul Licameli split from LV2Effect.cpp

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2 or later.  See License.txt.

**********************************************************************/

#if defined(USE_LV2)

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wparentheses"
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#elif defined(__clang__)
#pragma clang diagnostic ignored "-Wparentheses"
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
#endif

#include "LV2Preferences.h"
#include "ConfigInterface.h"

static constexpr auto SettingsStr = L"Settings";
static constexpr auto BufferSizeStr = L"BufferSize";
static constexpr auto UseLatencyStr = L"UseLatency";
static constexpr auto UseGUIStr = L"UseGUI";

namespace {
template<typename T>
bool GetSetting(const EffectDefinitionInterface& effect, const wchar_t* path,
                T& var, const T& defaultValue)
{
    return GetConfig(effect, PluginSettings::Shared, SettingsStr, path,
                     var, defaultValue);
}

template<typename T>
bool SetSetting(const EffectDefinitionInterface& effect, const wchar_t* path,
                const T& value)
{
    return SetConfig(effect, PluginSettings::Shared, SettingsStr, path,
                     value);
}
}

bool LV2Preferences::GetBufferSize(
    const EffectDefinitionInterface& effect, int& bufferSize)
{
    return GetSetting(effect, BufferSizeStr, bufferSize, 8192);
}

bool LV2Preferences::SetBufferSize(
    const EffectDefinitionInterface& effect, int bufferSize)
{
    return SetSetting(effect, BufferSizeStr, bufferSize);
}

bool LV2Preferences::GetUseLatency(
    const EffectDefinitionInterface& effect, bool& useLatency)
{
    return GetSetting(effect, UseLatencyStr, useLatency, true);
}

bool LV2Preferences::SetUseLatency(
    const EffectDefinitionInterface& effect, bool useLatency)
{
    return SetSetting(effect, UseLatencyStr, useLatency);
}

bool LV2Preferences::GetUseGUI(
    const EffectDefinitionInterface& effect, bool& useGUI)
{
    return GetSetting(effect, UseGUIStr, useGUI, true);
}

bool LV2Preferences::SetUseGUI(
    const EffectDefinitionInterface& effect, bool useGUI)
{
    return SetSetting(effect, UseGUIStr, useGUI);
}

#endif
