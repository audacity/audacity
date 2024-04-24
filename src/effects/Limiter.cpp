/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  Limiter.cpp

  Matthieu Hodgkinson

**********************************************************************/

#include "Limiter.h"
#include "CompressorInstance.h" // A limiter is just a compressor with hard-coded parameters.
#include "LimiterEditor.h"
#include "LoadEffects.h"
#include "ShuttleAutomation.h"

const ComponentInterfaceSymbol EffectLimiter::Symbol { XO(
   "Real-time Limiter") };

namespace
{
BuiltinEffectsModule::Registration<EffectLimiter> reg;
} // namespace

const EffectParameterMethods& EffectLimiter::Parameters() const
{
   static CapturedParameters<
      EffectLimiter, LimiterEditor::thresholdDb, LimiterEditor::kneeDb,
      LimiterEditor::lookaheadMs, LimiterEditor::releaseMs,
      LimiterEditor::makeUpDb>
      parameters;
   return parameters;
}

std::shared_ptr<EffectInstance> EffectLimiter::MakeInstance() const
{
   return std::make_shared<CompressorInstance>(*this);
}

bool EffectLimiter::CheckWhetherSkipEffect(const EffectSettings& settings) const
{
   // Given the infinite ratio, a limiter is always susceptible to modifying the
   // audio.
   return false;
}

EffectLimiter::EffectLimiter()
{
   SetLinearEffectFlag(false);
}

ComponentInterfaceSymbol EffectLimiter::GetSymbol() const
{
   return Symbol;
}

TranslatableString EffectLimiter::GetDescription() const
{
   return XO("Augments loudness while minimizing distortion.");
}

ManualPageID EffectLimiter::ManualPage() const
{
   // TODO
   return L"";
}

EffectType EffectLimiter::GetType() const
{
   return EffectTypeProcess;
}

auto EffectLimiter::RealtimeSupport() const -> RealtimeSince
{
   return RealtimeSince::After_3_1;
}

// Effect implementation

std::unique_ptr<EffectEditor> EffectLimiter::MakeEditor(
   ShuttleGui& S, EffectInstance&, EffectSettingsAccess& access,
   const EffectOutputs*) const
{
   auto& settings = access.Get();
   auto& myEffSettings = GetSettings(settings);
   auto result = std::make_unique<LimiterEditor>(*this, access, myEffSettings);
   result->PopulateOrExchange(S);
   return result;
}
