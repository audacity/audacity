/**********************************************************************

  Audacity: A Digital Audio Editor

  @file ParameterQueuePool.cpp

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#include "ParameterQueuePool.h"

#include "ParameterValueQueue.h"

void internal::ParameterQueuePool::QueueCleanup::operator()(ParameterValueQueue* queue)
{
   queue->dispose();
   queue->release();
}

internal::ParameterQueuePool& internal::ParameterQueuePool::Instance()
{
   static ParameterQueuePool instance;
   return instance;
}

internal::ParameterQueuePool::ParameterValueQueuePtr internal::ParameterQueuePool::Get(Steinberg::Vst::ParamID id)
{
   Steinberg::IPtr<ParameterValueQueue> queue;
   for(auto& q : mPool)
   {
      if(!q->isInitialized())
      {
         queue = q;
         break;
      }
   }
   if(queue == nullptr)
   {
      queue = owned(safenew ParameterValueQueue);
      mPool.push_back(queue);
   }
   queue->initialize(id);
   queue->addRef();
   return { queue.get(), QueueCleanup { } };
}

void internal::ParameterQueuePool::Reset() noexcept
{
   mPool.clear();
}
