/**********************************************************************

  Audacity: A Digital Audio Editor

  @file ComponentHandler.h

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#pragma once

#include <pluginterfaces/vst/ivsteditcontroller.h>

#include "EffectInterface.h"

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
      std::shared_ptr<EffectSettingsAccess> mAccess;
   public:
      
      ComponentHandler();
      virtual ~ComponentHandler();

      void SetAccess(const std::shared_ptr<EffectSettingsAccess>& access);

      EffectSettingsAccess* GetAccess();

      Steinberg::tresult PLUGIN_API beginEdit(Steinberg::Vst::ParamID id) override;

      Steinberg::tresult PLUGIN_API performEdit(Steinberg::Vst::ParamID id, Steinberg::Vst::ParamValue valueNormalized) override;

      Steinberg::tresult PLUGIN_API endEdit(Steinberg::Vst::ParamID id) override;

      Steinberg::tresult PLUGIN_API restartComponent(Steinberg::int32 flags) override;

      DECLARE_FUNKNOWN_METHODS
   };
}
