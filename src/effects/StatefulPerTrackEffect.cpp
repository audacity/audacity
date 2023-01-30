/**********************************************************************

  Audacity: A Digital Audio Editor

  StatefulPerTrackEffect.cpp

  Dominic Mazzoni
  Vaughan Johnson
  Martyn Shaw

  Paul Licameli split from PerTrackEffect.cpp

*******************************************************************//**

\class StatefulPerTrackEffect
\brief Base class for many of the effects in Audacity.

*//*******************************************************************/


#include "StatefulPerTrackEffect.h"
#include "ShuttleGui.h"
#include <wx/sizer.h>

StatefulPerTrackEffect::Instance::~Instance() = default;

bool StatefulPerTrackEffect::Instance::ProcessInitialize(
   EffectSettings &settings, double sampleRate, ChannelNames chanMap)
{
   return GetEffect()
      .ProcessInitialize(settings, sampleRate, chanMap);
}

bool StatefulPerTrackEffect::Instance::ProcessFinalize() noexcept
{
   return GetEffect().ProcessFinalize();
}

size_t StatefulPerTrackEffect::Instance::ProcessBlock(EffectSettings &settings,
   const float *const *inBlock, float *const *outBlock, size_t blockLen)
{
   return GetEffect().ProcessBlock(settings, inBlock, outBlock, blockLen);
}

StatefulPerTrackEffect::~StatefulPerTrackEffect() = default;

std::shared_ptr<EffectInstance> StatefulPerTrackEffect::MakeInstance() const
{
   // Cheat with const_cast to return an object that calls through to
   // non-const methods of a stateful effect.
   // Stateless effects should override this function and be really const
   // correct.
   return std::make_shared<Instance>(
      const_cast<StatefulPerTrackEffect&>(*this));
}

std::unique_ptr<EffectUIValidator>
StatefulPerTrackEffect::PopulateUI(ShuttleGui &S,
   EffectInstance &instance, EffectSettingsAccess &access,
   const EffectOutputs *pOutputs)
{
   auto parent = S.GetParent();

   // Let the effect subclass provide its own validator if it wants
   auto result = PopulateOrExchange(S, instance, access, pOutputs);

   parent->SetMinSize(parent->GetSizer()->GetMinSize());

   if (!result) {
      // No custom validator object?  Then use the default
      result = std::make_unique<DefaultEffectUIValidator>(
         *this, access, S.GetParent());
      parent->PushEventHandler(this);
   }
   return result;
}

bool StatefulPerTrackEffect::Process(
   EffectInstance &instance, EffectSettings &settings)
{
   // Call through to a non-virtual function
   return PerTrackEffect::Process(instance, settings);
}

size_t StatefulPerTrackEffect::SetBlockSize(size_t maxBlockSize)
{
   mBlockSize = maxBlockSize;
   return mBlockSize;
}

size_t StatefulPerTrackEffect::GetBlockSize() const
{
   return mBlockSize;
}

bool StatefulPerTrackEffect::ProcessInitialize(
   EffectSettings &, double, ChannelNames)
{
   return true;
}

bool StatefulPerTrackEffect::ProcessFinalize() noexcept
{
   return true;
}

std::unique_ptr<EffectUIValidator> StatefulPerTrackEffect::MakeEditor(
   ShuttleGui &, EffectInstance &, EffectSettingsAccess &,
   const EffectOutputs *)
{
   assert(false);
   return nullptr;
}
