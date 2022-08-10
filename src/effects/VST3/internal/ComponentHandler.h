/**********************************************************************

  Audacity: A Digital Audio Editor

  @file ComponentHandler.h

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#pragma once

#include <pluginterfaces/vst/ivsteditcontroller.h>

#include "EffectInterface.h"

class VST3Wrapper;

namespace Steinberg
{
   namespace Vst
   {
      class IParameterChanges;
   }
}

namespace internal
{
   class ComponentHandler : public Steinberg::Vst::IComponentHandler
   {
      VST3Wrapper& mWrapper;
      EffectSettingsAccess* mAccess{nullptr};
   public:
      
      ComponentHandler(VST3Wrapper& wrapper);
      virtual ~ComponentHandler();

      void SetAccess(EffectSettingsAccess* access);

      EffectSettingsAccess* GetAccess();

      Steinberg::tresult PLUGIN_API beginEdit(Steinberg::Vst::ParamID id) override;

      Steinberg::tresult PLUGIN_API performEdit(Steinberg::Vst::ParamID id, Steinberg::Vst::ParamValue valueNormalized) override;

      Steinberg::tresult PLUGIN_API endEdit(Steinberg::Vst::ParamID id) override;

      Steinberg::tresult PLUGIN_API restartComponent(Steinberg::int32 flags) override;

      DECLARE_FUNKNOWN_METHODS
   };
}
