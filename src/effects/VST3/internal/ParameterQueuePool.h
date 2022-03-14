/**********************************************************************

  Audacity: A Digital Audio Editor

  @file ParameterQueuePool.h

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#pragma once

#include <memory>
#include <vector>
#include <functional>
#include <pluginterfaces/base/smartpointer.h>
#include <pluginterfaces/vst/vsttypes.h>

#include "ParameterValueQueue.h"

namespace internal
{

   class ParameterValueQueue;

   //! Helper object. Global instance provides ParameterChanges
   //! with ParameterValueQueue objects
   class ParameterQueuePool final
   {
      std::vector<Steinberg::IPtr<ParameterValueQueue>> mPool;
   public:

      class QueueCleanup {
      public:
         void operator()(ParameterValueQueue* queue);
      };

      using ParameterValueQueuePtr = std::unique_ptr<
         ParameterValueQueue,
         QueueCleanup
      >;

      static ParameterQueuePool& Instance();

      ParameterValueQueuePtr Get(Steinberg::Vst::ParamID id);

      //Cleanup
      void Reset() noexcept;
      
   };

}
