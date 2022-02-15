/**********************************************************************

  Audacity: A Digital Audio Editor

  @file ComponentHandler.h

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#pragma once

#include <memory>
#include <atomic>
#include <functional>
#include <pluginterfaces/vst/ivsteditcontroller.h>

namespace Steinberg
{
   namespace Vst
   {
      class IParameterChanges;
   }
}

namespace internal
{
   /** Stores all pending changes from the editor, and
     * safely transfers them to the processing thread on request.
     * It's lock and wait free (given that there are two threads),
     * but it's theoretically possible that
     * processing thread will never get pending changes, but in real life
     * should never be the case.
     * Current implementation isn't strict about begin/perform/endEdit
     * order correctness(actually it's completely ignored right now,
     * but that might change in future).
     */
   class ComponentHandler : public Steinberg::Vst::IComponentHandler
   {
      std::atomic<Steinberg::Vst::IParameterChanges*> mFirst { nullptr };
      std::atomic<Steinberg::Vst::IParameterChanges*> mSecond { nullptr };
      std::atomic<Steinberg::Vst::IParameterChanges*> mPendingChanges { nullptr };

   public:
      using PendingChangesPtr = std::unique_ptr<
         Steinberg::Vst::IParameterChanges,
         std::function<void (Steinberg::Vst::IParameterChanges*)>>;

      ComponentHandler();
      virtual ~ComponentHandler();

      Steinberg::tresult PLUGIN_API beginEdit(Steinberg::Vst::ParamID id) override;

      Steinberg::tresult PLUGIN_API performEdit(Steinberg::Vst::ParamID id, Steinberg::Vst::ParamValue valueNormalized) override;

      Steinberg::tresult PLUGIN_API endEdit(Steinberg::Vst::ParamID id) override;

      Steinberg::tresult PLUGIN_API restartComponent(Steinberg::int32 flags) override;

      PendingChangesPtr getPendingChanges();


      DECLARE_FUNKNOWN_METHODS
   };
}
