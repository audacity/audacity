/**********************************************************************

  Audacity: A Digital Audio Editor

  @file ComponentHandler.cpp

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#include "ComponentHandler.h"

#include <pluginterfaces/vst/ivstparameterchanges.h>

#include "effects/VST3/VST3Wrapper.h"

internal::ComponentHandler::ComponentHandler() 
{
   FUNKNOWN_CTOR
}

internal::ComponentHandler::~ComponentHandler()
{
   FUNKNOWN_DTOR;
}

void internal::ComponentHandler::SetAccess(EffectSettingsAccess* access)
{
   mAccess = access;
}

EffectSettingsAccess* internal::ComponentHandler::GetAccess()
{
   return mAccess;
}

Steinberg::tresult internal::ComponentHandler::beginEdit(Steinberg::Vst::ParamID)
{
   return mAccess ? Steinberg::kResultOk : Steinberg::kResultFalse;
}

Steinberg::tresult internal::ComponentHandler::performEdit(Steinberg::Vst::ParamID id,
   Steinberg::Vst::ParamValue valueNormalized)
{
   using namespace Steinberg;

   if(!mAccess)
      return kResultFalse;

   mAccess->ModifySettings([&](EffectSettings& settings)
   {
      auto& vst3settings = VST3Wrapper::GetSettings(settings);
      vst3settings.parameterChanges[id] = valueNormalized;
      vst3settings.state[id] = valueNormalized;
   });

   return kResultOk;
}

Steinberg::tresult internal::ComponentHandler::endEdit(Steinberg::Vst::ParamID)
{
   return Steinberg::kResultOk;
}

Steinberg::tresult internal::ComponentHandler::restartComponent(Steinberg::int32 flags)
{
   return Steinberg::kNotImplemented;
}

IMPLEMENT_FUNKNOWN_METHODS(internal::ComponentHandler, Steinberg::Vst::IComponentHandler, Steinberg::Vst::IComponentHandler::iid)
