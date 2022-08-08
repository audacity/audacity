#pragma once
#include <unordered_map>
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

struct VST3EffectSettings
{
   //Holds the parameter that has been changed since last
   std::unordered_map<Steinberg::Vst::ParamID, Steinberg::Vst::ParamValue> parameterChanges;
   //Holds the "current" parameter values
   std::unordered_map<Steinberg::Vst::ParamID, Steinberg::Vst::ParamValue> state;
};

class InputParameterValueQueue final : public Steinberg::Vst::IParamValueQueue
{
   Steinberg::Vst::ParamID mParameterId{};
   const Steinberg::Vst::ParamValue* mValues{nullptr};
public:

   InputParameterValueQueue() { FUNKNOWN_CTOR }
   ~InputParameterValueQueue() { FUNKNOWN_DTOR }

   void Bind(Steinberg::Vst::ParamID id, const Steinberg::Vst::ParamValue* values);

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
   
   bool LoadPreset(Steinberg::IBStream* fileStream);
   bool SavePreset(Steinberg::IBStream* fileStream) const;

   bool Initialize(Steinberg::Vst::SampleRate sampleRate, Steinberg::int32 processMode, Steinberg::int32 maxSamplesPerBlock);
   size_t Process(const EffectSettings& settings, const float* const* inBlock, float* const* outBlock, size_t blockLen);
   void Finalize();

   void SuspendProcessing();
   void ResumeProcessing();

   Steinberg::int32 GetLatencySamples() const;

   //Updates native UI state to match the internal processor state,
   //used when native UI isn't a source of changes
   bool SyncComponentStates();

   //Used to flush changes to the IAudioProcessor, while
   //plugin is inactive(!)
   void FlushSettings(const EffectSettings& settings);

   static EffectSettings MakeSettings();

   static VST3EffectSettings& GetSettings(EffectSettings& settings);
   static const VST3EffectSettings& GetSettings(const EffectSettings& settings);

private:
   const VST3::UID mEffectUID;

   bool mActive{false};

   //A preallocated array of Steinberg::Vst::IParameterValueQueue
   //used as a view to an actual parameter changes that reside
   //in VST3EffectSettings structure, dynamically assigned during
   //processing
   std::unique_ptr<InputParameterValueQueue[]> mParameterQueues;
};
