#pragma once
#include <pluginterfaces/base/smartpointer.h>
#include <pluginterfaces/vst/ivstaudioprocessor.h>
#include <public.sdk/source/vst/hosting/module.h>
#include "internal/ComponentHandler.h"

namespace Steinberg
{
   class IPlugFrame;
   class IPlugView;
}

namespace Steinberg
{
   namespace Vst
   {
      class IConnectionPoint;
      class IEditController;
   }
}

struct EffectSettings;

struct VST3EffectSettings
{
   // states as saved by IComponent::getState
   std::optional<std::string> mProcessorStateStr;
   std::optional<std::string> mControllerStateStr;
};

struct VST3Wrapper
{
   VST3Wrapper(std::shared_ptr<VST3::Hosting::Module> module, VST3::Hosting::ClassInfo effectClassInfo);
   VST3Wrapper(const VST3Wrapper& other);
   ~VST3Wrapper();

   const VST3::Hosting::ClassInfo mEffectClassInfo;

   // Keep strong reference to a module; this because it has to be destroyed in the destructor of this class,
   // otherwise the destruction of mEditController and mEffectComponent would trigger a memory fault.
   std::shared_ptr<VST3::Hosting::Module> mModule;

   Steinberg::IPtr<Steinberg::Vst::IAudioProcessor> mAudioProcessor;
   Steinberg::Vst::ProcessSetup mSetup;
   Steinberg::IPtr<Steinberg::Vst::IComponent>      mEffectComponent;
   Steinberg::IPtr<Steinberg::Vst::IEditController> mEditController;
   Steinberg::IPtr<Steinberg::Vst::IConnectionPoint> mComponentConnectionProxy;
   Steinberg::IPtr<Steinberg::Vst::IConnectionPoint> mControllerConnectionProxy;
   Steinberg::IPtr<internal::ComponentHandler> mComponentHandler;
   //Holds pending parameter changes to be applied to multiple realtime effects.
   //Not used in the "offline" mode
   internal::ComponentHandler::PendingChangesPtr mPendingChanges;

   bool FetchSettings(      VST3EffectSettings& settings) const;
   bool StoreSettings(const VST3EffectSettings& settings) const;

   bool LoadPreset(Steinberg::IBStream* fileStream);
   bool SavePreset(Steinberg::IBStream* fileStream) const;


   VST3EffectSettings mSettings;  // temporary, until the effect is really stateless

   //! This function will be rewritten when the effect is really stateless
   VST3EffectSettings& GetSettings(EffectSettings&) const
   {
      return const_cast<VST3Wrapper*>(this)->mSettings;
   }

   //! This function will be rewritten when the effect is really stateless
   const VST3EffectSettings& GetSettings(const EffectSettings&) const
   {
      return mSettings;
   }

   //! This is what ::GetSettings will be when the effect becomes really stateless
   /*
   static inline VST3EffectSettings& GetSettings(EffectSettings& settings)
   {
      auto pSettings = settings.cast<VST3EffectSettings>();
      assert(pSettings);
      return *pSettings;
   }
   */

private:
   void Initialize();

};
