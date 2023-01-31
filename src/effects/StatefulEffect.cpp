/**********************************************************************

  Audacity: A Digital Audio Editor

  StatefulEffect.cpp

  Dominic Mazzoni
  Vaughan Johnson
  Martyn Shaw

  Paul Licameli split from Effect.cpp

**********************************************************************/
#include "StatefulEffect.h"
#include "StatefulEffectUIServices.h"
#include "SampleCount.h"
#include "ShuttleGui.h"
#include <wx/sizer.h>

bool StatefulEffect::Instance::Process(EffectSettings &settings)
{
   return GetEffect().Process(*this, settings);
}

auto StatefulEffect::Instance::GetLatency(const EffectSettings &, double) const
   -> SampleCount
{
   return GetEffect().GetLatency().as_long_long();
}

size_t StatefulEffect::Instance::ProcessBlock(EffectSettings &,
   const float *const *, float *const *, size_t)
{
   return 0;
}

StatefulEffect::~StatefulEffect() = default;

std::shared_ptr<EffectInstance> StatefulEffect::MakeInstance() const
{
   // Cheat with const-cast to return an object that calls through to
   // non-const methods of a stateful effect.
   // Stateless effects should override this function and be really const
   // correct.
   return std::make_shared<Instance>(const_cast<StatefulEffect&>(*this));
}

std::unique_ptr<EffectEditor> StatefulEffect::PopulateUI(
   const EffectPlugin &, ShuttleGui &S,
   EffectInstance &instance, EffectSettingsAccess &access,
   const EffectOutputs *pOutputs) const
{
   auto parent = S.GetParent();

   // As in MakeInstance, we still cheat const for stateful effects!
   auto pThis = const_cast<StatefulEffect*>(this);

   // Let the effect subclass provide its own editor if it wants
   auto result = pThis->PopulateOrExchange(S, instance, access, pOutputs);

   parent->SetMinSize(parent->GetSizer()->GetMinSize());

   if (!result) {
      // No custom editor object?  Then use the default
      result = std::make_unique<DefaultEffectEditor>(*pThis,
         *pThis, access, S.GetParent());
      parent->PushEventHandler(pThis);
   }
   return result;
}
