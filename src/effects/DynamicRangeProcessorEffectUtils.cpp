#include "DynamicRangeProcessorEffectUtils.h"
#include "CompressorEditor.h"
#include "CompressorInstance.h"
#include "DynamicRangeProcessorHistoryPanel.h"
#include "DynamicRangeProcessorOutputs.h"
#include "LimiterEditor.h"
#include "ShuttleGui.h"

namespace
{
template <typename EditorType, typename SettingType>
std::unique_ptr<EffectEditor> MakeEditor(
   ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access,
   const EffectUIServices& services, const EffectOutputs* pOutputs,
   SettingType settings)
{
   auto& compressorInstance = *dynamic_cast<CompressorInstance*>(&instance);
   // const-cast so that the editor can set and unset
   // `DynamicRangeProcessorOutputs::mEditorCallback`.
   // TODO is there a notification that `pOutputs` was called `Assign` upon
   // somewhere ?
   auto processorOutputs = dynamic_cast<DynamicRangeProcessorOutputs*>(
      const_cast<EffectOutputs*>(pOutputs));
   auto result = std::make_unique<EditorType>(
      S.GetParent(), compressorInstance, processorOutputs, services, access,
      std::move(settings));
   result->PopulateOrExchange(S);
   return result;
}
} // namespace

std::unique_ptr<EffectEditor>
DynamicRangeProcessorEffectUtils::MakeCompressorEditor(
   ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access,
   const EffectUIServices& services, const EffectOutputs* pOutputs,
   CompressorSettings settings)
{
   return MakeEditor<CompressorEditor>(
      S, instance, access, services, pOutputs, std::move(settings));
}

std::unique_ptr<EffectEditor>
DynamicRangeProcessorEffectUtils::MakeLimiterEditor(
   ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access,
   const EffectUIServices& services, const EffectOutputs* pOutputs,
   LimiterSettings settings)
{
   return MakeEditor<LimiterEditor>(
      S, instance, access, services, pOutputs, std::move(settings));
}
