/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  Compressor.cpp

  Matthieu Hodgkinson

**********************************************************************/

#include "Compressor.h"
#include "CompressorEditor.h"
#include "CompressorInstance.h"
#include "LoadEffects.h"
#include "ShuttleAutomation.h"

const ComponentInterfaceSymbol EffectCompressor::Symbol { XO(
   "Real-time Compressor") };

namespace
{
BuiltinEffectsModule::Registration<EffectCompressor> reg;
} // namespace

const EffectParameterMethods& EffectCompressor::Parameters() const
{
   static CapturedParameters<
      EffectCompressor, CompressorEditor::thresholdDb, CompressorEditor::kneeDb,
      CompressorEditor::lookaheadMs, CompressorEditor::attackMs,
      CompressorEditor::releaseMs, CompressorEditor::ratio,
      CompressorEditor::makeUpDb>
      parameters;
   return parameters;
}

std::shared_ptr<EffectInstance> EffectCompressor::MakeInstance() const
{
   return std::make_shared<CompressorInstance>(*this);
}

bool EffectCompressor::CheckWhetherSkipEffect(
   const EffectSettings& settings) const
{
   // TODO
   return Effect::CheckWhetherSkipEffect(settings);
}

EffectCompressor::EffectCompressor()
{
   SetLinearEffectFlag(true);
}

EffectCompressor::~EffectCompressor()
{
}

ComponentInterfaceSymbol EffectCompressor::GetSymbol() const
{
   return Symbol;
}

TranslatableString EffectCompressor::GetDescription() const
{
   return XO("A compressor");
}

ManualPageID EffectCompressor::ManualPage() const
{
   // TODO
   return L"";
}

EffectType EffectCompressor::GetType() const
{
   return EffectTypeProcess;
}

auto EffectCompressor::RealtimeSupport() const -> RealtimeSince
{
   return RealtimeSince::After_3_1;
}

// Effect implementation

std::unique_ptr<EffectEditor> EffectCompressor::MakeEditor(
   ShuttleGui& S, EffectInstance&, EffectSettingsAccess& access,
   const EffectOutputs*) const
{
   auto& settings = access.Get();
   auto& myEffSettings = GetSettings(settings);
   auto result =
      std::make_unique<CompressorEditor>(*this, access, myEffSettings);
   result->PopulateOrExchange(S);
   return result;
}
