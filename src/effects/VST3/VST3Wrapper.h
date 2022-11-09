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

   //!Fetch state from settings object, may change internal runtime data
   void FetchSettings(EffectSettings&);
   //!Saves current state inside settings object, clears all runtime data
   void StoreSettings(EffectSettings&) const;
   
   bool LoadPreset(Steinberg::IBStream* fileStream);
   bool SavePreset(Steinberg::IBStream* fileStream) const;

   //!Initializes effect for processing using settings.
   bool Initialize(EffectSettings& settings,
      Steinberg::Vst::SampleRate sampleRate,
      Steinberg::int32 processMode,
      Steinberg::int32 maxSamplesPerBlock);
   //!Frees up resources allocated for processing, should be called
   //!after processing is complete. Optionally settings object may
   //!be passed to update runtime data with current internal state.
   void Finalize(EffectSettings* settings);

   //!Prepares effect to process next block with changes written to the settings object
   void ProcessBlockStart(const EffectSettings& settings);

   //Used to send EffectSettings changes to the IAudioProcessor, while effect is inactive(!)
   void FlushParameters(EffectSettings& settings);

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
   static OptionalMessage LoadUserPreset(
      const EffectDefinitionInterface& effect, const RegistryPath& name, EffectSettings& settings);
   static void SaveUserPreset(const EffectDefinitionInterface& effect, const RegistryPath& name, const EffectSettings& settings);

   static void CopySettingsContents(const EffectSettings& src, EffectSettings& dst);

private:

   //Reads runtime data changes to apply them during next processing pass
   void ConsumeChanges(const EffectSettings& settings);

   bool mActive {false};
   const VST3::UID mEffectUID;

   std::vector<std::pair<Steinberg::Vst::ParamID, Steinberg::Vst::ParamValue>> mParameters;
   //A preallocated array of Steinberg::Vst::IParameterValueQueue
   //used as a view to an actual parameter changes that reside
   //in VST3EffectSettings structure, dynamically assigned during
   //processing
   std::unique_ptr<SingleInputParameterValue[]> mParameterQueues;
};
