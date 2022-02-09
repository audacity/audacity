/**********************************************************************

  Audacity: A Digital Audio Editor

  @file ComponentHandler.cpp

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#include "ComponentHandler.h"

#include <pluginterfaces/vst/ivstparameterchanges.h>

#include "ParameterChanges.h"

internal::ComponentHandler::ComponentHandler()
{
   //We use two instances of ParameterChanges to ensure
   //that read/write is lock and wait free.
   //Once parameter value change is requested, they are
   //written either to the pending outgoing parameters,
   //if they are not yet requested, or to any of those
   //two instances whichever of them is available.
   //When done writing, parameter changes become pending.

   using namespace Steinberg;

   FUNKNOWN_CTOR
   auto first = owned(safenew internal::ParameterChanges);
   auto second = owned(safenew internal::ParameterChanges);
   if(first && second)
   {
      mFirst = first.take();
      mSecond = second.take();
   }
}

internal::ComponentHandler::~ComponentHandler()
{
   Steinberg::Vst::IParameterChanges* ptr { nullptr };
   if(ptr = mFirst.load())
      ptr->release();
   if(ptr = mSecond.load())
      ptr->release();
   if(ptr = mPendingChanges.load())
      ptr->release();

   FUNKNOWN_DTOR;
}

Steinberg::tresult internal::ComponentHandler::beginEdit(Steinberg::Vst::ParamID)
{
   return Steinberg::kResultOk;
}

Steinberg::tresult internal::ComponentHandler::performEdit(Steinberg::Vst::ParamID id,
   Steinberg::Vst::ParamValue valueNormalized)
{
   using namespace Steinberg;

   //Try grab pending changes first (processing thread will not
   //see them until they are applied)
   auto lock = mPendingChanges.exchange(nullptr);
   if(lock == nullptr)
   {
      //There is no pending changes, then try grab
      //any of the containers. At least one of them always should be
      //available, but another one may be grabbed by the processing thread
      lock = mFirst.exchange(nullptr);
      if(lock == nullptr)
         lock = mSecond.exchange(nullptr);

      //Grabbed object may contain some old data, clean it.
      //We surely don't want to delay the processing thread with cleanup
      //routines
      static_cast<ParameterChanges*>(lock)->clear();
   }

   int32 index;
   tresult result = kInternalError;

   if(const auto queue = lock->addParameterData(id, index))
      //Since we don't yet have a support for automation,
      //sampleOffset is always 0
      result = queue->addPoint(0, valueNormalized, index);
   //else
   // for some unknown reason we have failed to
   // find/create appropriate queue for this parameter

   //now processing thread can see changes
   mPendingChanges.exchange(lock);

   return result;
}

Steinberg::tresult internal::ComponentHandler::endEdit(Steinberg::Vst::ParamID)
{
   return Steinberg::kResultOk;
}

Steinberg::tresult internal::ComponentHandler::restartComponent(Steinberg::int32 flags)
{
   return Steinberg::kNotImplemented;
}

internal::ComponentHandler::PendingChangesPtr internal::ComponentHandler::getPendingChanges()
{
   using namespace Steinberg;

   const auto pendingChanges = mPendingChanges.exchange(nullptr);
   if(pendingChanges != nullptr)
   {
      return PendingChangesPtr { pendingChanges, [handler = IPtr { this }](Vst::IParameterChanges* ptr)
      {
         //once the processing thread done reading changes
         //pointer is moved back
         ptr = handler->mFirst.exchange(ptr);
         if(ptr != nullptr)
            handler->mSecond.exchange(ptr);
      }};
   }
   return { nullptr };
}

IMPLEMENT_FUNKNOWN_METHODS(internal::ComponentHandler, Steinberg::Vst::IComponentHandler, Steinberg::Vst::IComponentHandler::iid)
