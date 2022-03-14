/**********************************************************************

  Audacity: A Digital Audio Editor

  @file ParameterChanges.cpp

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#include "ParameterChanges.h"

#include "ParameterQueuePool.h"


internal::ParameterChanges::ParameterChanges()
{
   FUNKNOWN_CTOR
}

internal::ParameterChanges::~ParameterChanges()
{
   clear();
   FUNKNOWN_DTOR;
}

Steinberg::int32 internal::ParameterChanges::getParameterCount()
{
   return static_cast<Steinberg::int32>(mParamQueues.size());
}

Steinberg::Vst::IParamValueQueue* internal::ParameterChanges::getParameterData(Steinberg::int32 index)
{
   if(index >= 0 && index < mParamQueues.size())
      return mParamQueues[index].get();
   return nullptr;
}

Steinberg::Vst::IParamValueQueue* internal::ParameterChanges::addParameterData(const Steinberg::Vst::ParamID& id,
   Steinberg::int32& index)
{
   using namespace Steinberg;
   {
      for(int32 i = 0, count = static_cast<int32>(mParamQueues.size()); i < count; ++i)
      {
         auto& queue = mParamQueues[i];
         if(queue->getParameterId() == id)
         {
            index = i;
            return queue.get();
         }
      }
   }
   auto queue = ParameterQueuePool::Instance().Get(id);
   index = static_cast<int32>(mParamQueues.size());
   mParamQueues.push_back(std::move(queue));
   return mParamQueues.back().get();
}

void internal::ParameterChanges::clear()
{                            
   mParamQueues.clear();
}

IMPLEMENT_FUNKNOWN_METHODS(internal::ParameterChanges, Steinberg::Vst::IParameterChanges,
                           Steinberg::Vst::IParameterChanges::iid)
