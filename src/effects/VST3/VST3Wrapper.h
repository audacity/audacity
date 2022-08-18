#pragma once

#include <pluginterfaces/base/smartpointer.h>
#include <pluginterfaces/vst/ivstaudioprocessor.h>
#include <pluginterfaces/vst/ivstparameterchanges.h>
#include <public.sdk/source/vst/hosting/module.h>

#include "EffectInterface.h"
#include "internal/ComponentHandler.h"

class VST3Wrapper;

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
      class IParameterChanges;
   }
}

class VST3Wrapper
{
   EffectSettings mDefaultSettings;
public:
   Steinberg::IPtr<Steinberg::Vst::IAudioProcessor> mAudioProcessor;
   Steinberg::Vst::ProcessSetup mSetup;
   Steinberg::IPtr<Steinberg::Vst::IComponent>      mEffectComponent;
   Steinberg::IPtr<Steinberg::Vst::IEditController> mEditController;
   Steinberg::IPtr<Steinberg::Vst::IConnectionPoint> mComponentConnectionProxy;
   Steinberg::IPtr<Steinberg::Vst::IConnectionPoint> mControllerConnectionProxy;
   Steinberg::IPtr<internal::ComponentHandler> mComponentHandler;

   VST3Wrapper(VST3::Hosting::Module& module, VST3::UID effectUID);
   ~VST3Wrapper();

   VST3Wrapper(const VST3Wrapper&) = delete;
   VST3Wrapper(VST3Wrapper&&) = delete;
   VST3Wrapper& operator=(const VST3Wrapper&) = delete;
   VST3Wrapper& operator=(VST3Wrapper&&) = delete;

   void FetchSettings(const EffectSettings&);
   void StoreSettings(EffectSettings&) const;

   bool LoadPreset(Steinberg::IBStream* fileStream);
   bool SavePreset(Steinberg::IBStream* fileStream) const;

   Steinberg::int32 GetLatencySamples() const;
   
   static EffectSettings MakeSettings();

   static void LoadSettings(const CommandParameters& parms, EffectSettings& settings);
   static void SaveSettings(const EffectSettings& settings, CommandParameters& parms);
   static void LoadUserPreset(const EffectDefinitionInterface& effect, const RegistryPath& name, EffectSettings& settings);
   static void SaveUserPreset(const EffectDefinitionInterface& effect, const RegistryPath& name, const EffectSettings& settings);

private:
   const VST3::UID mEffectUID;
};
