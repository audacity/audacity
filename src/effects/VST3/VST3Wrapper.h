#pragma once
#include <map>
#include <pluginterfaces/base/smartpointer.h>
#include <pluginterfaces/vst/ivstaudioprocessor.h>
#include <pluginterfaces/vst/ivstparameterchanges.h>
#include <public.sdk/source/vst/hosting/module.h>
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

struct EffectSettings;

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

class VST3Wrapper final
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

   bool IsActive() const noexcept;

   bool LoadPreset(Steinberg::IBStream* fileStream);
   bool SavePreset(Steinberg::IBStream* fileStream) const;

   bool Initialize(const EffectSettings& settings, Steinberg::Vst::SampleRate sampleRate, Steinberg::int32 processMode, Steinberg::int32 maxSamplesPerBlock);
   void ProcessStart(EffectSettings& settings);
   size_t Process(EffectSettings& settings, const float* const* inBlock, float* const* outBlock, size_t blockLen);
   void Finalize();

   void SuspendProcessing();
   void ResumeProcessing();

   Steinberg::int32 GetLatencySamples() const;

   void UpdateParameter(EffectSettingsAccess& access, Steinberg::Vst::ParamID id, Steinberg::Vst::ParamValue value);

   //Used to flush changes to the IAudioProcessor, while
   //plugin is inactive(!)
   void FlushSettings(EffectSettings& settings);

   static EffectSettings MakeSettings();
   static void CopySettings(const EffectSettings& src, EffectSettings& dst, SettingsCopyDirection copyDirection);

   static void LoadSettings(const CommandParameters& parms, EffectSettings& settings);
   static void SaveSettings(const EffectSettings& settings, CommandParameters& parms);
   static void LoadUserPreset(const EffectDefinitionInterface& effect, const RegistryPath& name, EffectSettings& settings);
   static void SaveUserPreset(const EffectDefinitionInterface& effect, const RegistryPath& name, const EffectSettings& settings);

private:
   const VST3::UID mEffectUID;

   bool mActive{false};

   std::vector<std::pair<Steinberg::Vst::ParamID, Steinberg::Vst::ParamValue>> mParameters;
   //A preallocated array of Steinberg::Vst::IParameterValueQueue
   //used as a view to an actual parameter changes that reside
   //in VST3EffectSettings structure, dynamically assigned during
   //processing
   std::unique_ptr<SingleInputParameterValue[]> mParameterQueues;
};
