/**********************************************************************

  Audacity: A Digital Audio Editor

  @file ParameterChanges.h

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#pragma once

#include <vector>

#include <pluginterfaces/vst/ivstparameterchanges.h>
#include "ParameterValueQueue.h"
#include "ParameterQueuePool.h"

namespace internal
{
   /** Implements IParameterChanges, uses ParameterQueuePool internally
     * to avoid unnecessary allocations. Though both getParameterData
     * and addParameterData return pointers to shared objects,
     * it's not recommended to store them, because they are pointers to
     * temporary objects.
     */
   class ParameterChanges final : public Steinberg::Vst::IParameterChanges
   {
      std::vector<ParameterQueuePool::ParameterValueQueuePtr> mParamQueues;

   public:

      ParameterChanges();

      virtual ~ParameterChanges();

      Steinberg::int32 PLUGIN_API getParameterCount() override;

      Steinberg::Vst::IParamValueQueue* PLUGIN_API getParameterData(Steinberg::int32 index) override;

      Steinberg::Vst::IParamValueQueue* PLUGIN_API
      addParameterData(const Steinberg::Vst::ParamID& id, Steinberg::int32& index) override;

      void clear();

      DECLARE_FUNKNOWN_METHODS
   };
}
