/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DynamicRangeProcessorEffectUtils.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "DynamicRangeProcessorTypes.h"
#include <memory>

class EffectEditor;
class EffectInstance;
class EffectOutputs;
class EffectSettingsAccess;
class EffectUIServices;
class ShuttleGui;

namespace DynamicRangeProcessorEffectUtils {
std::unique_ptr<EffectEditor> MakeCompressorEditor(
    ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access, const EffectUIServices& services, const EffectOutputs* pOutputs,
    CompressorSettings settings);

std::unique_ptr<EffectEditor> MakeLimiterEditor(
    ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access, const EffectUIServices& services, const EffectOutputs* pOutputs,
    LimiterSettings settings);
} // namespace DynamicRangeProcessorEffectUtils
