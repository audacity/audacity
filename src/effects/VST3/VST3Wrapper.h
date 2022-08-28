#pragma once

#include <pluginterfaces/base/smartpointer.h>
#include <pluginterfaces/vst/ivstaudioprocessor.h>
#include <pluginterfaces/vst/ivstparameterchanges.h>
#include <public.sdk/source/vst/hosting/module.h>

#include "EffectInterface.h"

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
      class IComponentHandler;
      class IConnectionPoint;
      class IEditController;
      class IParameterChanges;
   }
}

class SingleInputParameterValue final : public Steinberg::Vst::IParamValueQueue
{
   Steinberg::Vst::ParamID mParameterId{};
   Steinberg::Vst::ParamValue mValue;
public:

   SingleInputParameterValue() { FUNKNOWN_CTOR }
   ~SingleInputParameterValue() { FUNKNOWN_DTOR }

   void Set(Steinberg::Vst::ParamID id, const Steinberg::Vst::ParamValue value);

   Steinberg::tresult PLUGIN_API addPoint(Steinberg::int32 sampleOffset, Steinberg::Vst::ParamValue value,
      Steinberg::int32& index) override;

   Steinberg::Vst::ParamID PLUGIN_API getParameterId() override;

   Steinberg::tresult PLUGIN_API getPoint(Steinberg::int32 index, Steinberg::int32& sampleOffset,
      Steinberg::Vst::ParamValue& value) override;

   Steinberg::int32 PLUGIN_API getPointCount() override;

   DECLARE_FUNKNOWN_METHODS
};

class VST3Wrapper
{
   EffectSettings mDefaultSettings;
   VST3::Hosting::Module& mModule;
public:
   Steinberg::IPtr<Steinberg::Vst::IAudioProcessor> mAudioProcessor;
   Steinberg::Vst::ProcessSetup mSetup;
   Steinberg::IPtr<Steinberg::Vst::IComponent>      mEffectComponent;
   Steinberg::IPtr<Steinberg::Vst::IEditController> mEditController;
   Steinberg::IPtr<Steinberg::Vst::IConnectionPoint> mComponentConnectionProxy;
   Steinberg::IPtr<Steinberg::Vst::IConnectionPoint> mControllerConnectionProxy;
   Steinberg::IPtr<Steinberg::Vst::IComponentHandler> mComponentHandler;

   VST3Wrapper(VST3::Hosting::Module& module, VST3::UID effectUID);
   ~VST3Wrapper();

   VST3Wrapper(const VST3Wrapper&) = delete;
   VST3Wrapper(VST3Wrapper&&) = delete;
   VST3Wrapper& operator=(const VST3Wrapper&) = delete;
   VST3Wrapper& operator=(VST3Wrapper&&) = delete;

   VST3::Hosting::Module& GetModule() const { return mModule; }

   bool IsActive() const noexcept;

   void FetchSettings(const EffectSettings&);
   void StoreSettings(EffectSettings&) const;

   bool LoadPreset(Steinberg::IBStream* fileStream);
   bool SavePreset(Steinberg::IBStream* fileStream) const;

   bool Initialize(const EffectSettings& settings, Steinberg::Vst::SampleRate sampleRate, Steinberg::int32 processMode, Steinberg::int32 maxSamplesPerBlock);
   void Finalize();

   //Updates internal state with changes from settings
   void ConsumeChanges(const EffectSettings& settings);
   //Used to send EffectSettings to the IAudioProcessor, while effect is inactive(!)
   void FlushSettings(EffectSettings& settings);

   //Intialize first, before calling to Process. It's safe to it use from another thread
   size_t Process(const float* const* inBlock, float* const* outBlock, size_t blockLen);
   
   void SuspendProcessing();
   void ResumeProcessing();

   void BeginParameterEdit(EffectSettingsAccess& access);
   void EndParameterEdit();

   Steinberg::int32 GetLatencySamples() const;
   
   static EffectSettings MakeSettings();

   static void LoadSettings(const CommandParameters& parms, EffectSettings& settings);
   static void SaveSettings(const EffectSettings& settings, CommandParameters& parms);
   static void LoadUserPreset(const EffectDefinitionInterface& effect, const RegistryPath& name, EffectSettings& settings);
   static void SaveUserPreset(const EffectDefinitionInterface& effect, const RegistryPath& name, const EffectSettings& settings);

   static void AssignSettings(EffectSettings& dst, EffectSettings&& src);
   static void CopySettingsContents(const EffectSettings& src, EffectSettings& dst, SettingsCopyDirection copyDirection);

private:
   bool mActive {false};
   const VST3::UID mEffectUID;

   std::vector<std::pair<Steinberg::Vst::ParamID, Steinberg::Vst::ParamValue>> mParameters;
   //A preallocated array of Steinberg::Vst::IParameterValueQueue
   //used as a view to an actual parameter changes that reside
   //in VST3EffectSettings structure, dynamically assigned during
   //processing
   std::unique_ptr<SingleInputParameterValue[]> mParameterQueues;
};
