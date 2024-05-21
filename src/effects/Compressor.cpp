/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  Compressor.cpp

  Matthieu Hodgkinson

**********************************************************************/

#include "Compressor.h"
#include "CompressorEditor.h"
#include "CompressorInstance.h"
#include "DynamicRangeProcessorEffectUtils.h"
#include "DynamicRangeProcessorOutputs.h"
#include "LoadEffects.h"
#include "ShuttleAutomation.h"

const ComponentInterfaceSymbol EffectCompressor::Symbol { XO("Compressor") };

namespace
{
BuiltinEffectsModule::Registration<EffectCompressor> reg;
} // namespace

const EffectParameterMethods& EffectCompressor::Parameters() const
{
   static CapturedParameters<
      EffectCompressor, CompressorEditor::thresholdDb,
      CompressorEditor::makeupGainDb, CompressorEditor::kneeWidthDb,
      CompressorEditor::compressionRatio, CompressorEditor::lookaheadMs,
      CompressorEditor::attackMs, CompressorEditor::releaseMs,
      CompressorEditor::showGraph>
      parameters;
   return parameters;
}

std::shared_ptr<EffectInstance> EffectCompressor::MakeInstance() const
{
   return std::make_shared<CompressorInstance>(*this);
}

std::unique_ptr<EffectOutputs> EffectCompressor::MakeOutputs() const
{
   return std::make_unique<DynamicRangeProcessorOutputs>();
}

bool EffectCompressor::CheckWhetherSkipEffect(
   const EffectSettings& settings) const
{
   const auto& compressorSettings = GetSettings(settings);
   return compressorSettings.compressionRatio == 1 &&
          compressorSettings.makeupGainDb == 0 &&
          // Also look-ahead, as this adds delay when used on a real-time input
          // (mic).
          compressorSettings.lookaheadMs == 0;
}

EffectCompressor::EffectCompressor()
{
   SetLinearEffectFlag(false);
}

ComponentInterfaceSymbol EffectCompressor::GetSymbol() const
{
   return Symbol;
}

TranslatableString EffectCompressor::GetDescription() const
{
   return XO(
      "Reduces \"dynamic range\", or differences between loud and quiet parts.");
}

ManualPageID EffectCompressor::ManualPage() const
{
   // TODO: add manual page
   return L"";
}

EffectType EffectCompressor::GetType() const
{
   return EffectTypeProcess;
}

auto EffectCompressor::RealtimeSupport() const -> RealtimeSince
{
   return RealtimeSince::Always;
}

// Effect implementation

std::unique_ptr<EffectEditor> EffectCompressor::MakeEditor(
   ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access,
   const EffectOutputs* pOutputs) const
{
   return DynamicRangeProcessorEffectUtils::MakeCompressorEditor(
      S, instance, access, *this, pOutputs, GetSettings(access.Get()));
}
