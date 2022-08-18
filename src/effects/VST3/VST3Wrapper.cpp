#include "VST3Wrapper.h"
#include <stdexcept>
#include <optional>
#include <map>

#include "EffectInterface.h"

#include "AudacityVst3HostApplication.h"

#include <pluginterfaces/vst/ivsteditcontroller.h>
#include <pluginterfaces/vst/ivstparameterchanges.h>

#include "ConfigInterface.h"
#include "memorystream.h"
#include "VST3Utils.h"
#include "internal/ConnectionProxy.h"

namespace
{

// define some shared registry keys
constexpr auto processorStateKey  = wxT("ProcessorState");
constexpr auto controllerStateKey = wxT("ControllerState");

struct VST3EffectSettings
{
   ///Holds the parameter that has been changed since last processing pass.
   std::map<Steinberg::Vst::ParamID, Steinberg::Vst::ParamValue> parameterChanges;

   ///Holds the last known processor/component state, rarely updates (usually only on UI or preset change)
   std::optional<wxString> processorState;
   ///Holds the last known controller state, rarely updates (usually only on UI or preset change)
   std::optional<wxString> controllerState;
};

VST3EffectSettings& GetSettings(EffectSettings& settings)
{
   auto vst3settings = settings.cast<VST3EffectSettings>();
   assert(vst3settings);
   return *vst3settings;
}

const VST3EffectSettings& GetSettings(const EffectSettings& settings)
{
   auto vst3settings = settings.cast<VST3EffectSettings>();
   assert(vst3settings);
   return *vst3settings;
}


//Activates main audio input/output buses and disables others (event, audio aux)
void ActivateMainAudioBuses(Steinberg::Vst::IComponent& component)
{
   using namespace Steinberg;

   std::vector<Vst::SpeakerArrangement> defaultInputSpeakerArrangements;
   std::vector<Vst::SpeakerArrangement> defaultOutputSpeakerArrangements;

   const auto processor = FUnknownPtr<Vst::IAudioProcessor>(&component);

   for(int i = 0, count = component.getBusCount(Vst::kAudio, Vst::kInput); i < count; ++i)
   {
      Vst::BusInfo busInfo {};
      Vst::SpeakerArrangement arrangement {0ull};
      if(component.getBusInfo(Vst::kAudio, Vst::kInput, i, busInfo) == kResultOk)
      {
         if(busInfo.busType == Vst::kMain && busInfo.channelCount > 0)
            arrangement = (1ull << busInfo.channelCount) - 1ull;
      }
      if(component.activateBus(Vst::kAudio, Vst::kInput, i, arrangement > 0) != kResultOk)
         arrangement = 0;

      defaultInputSpeakerArrangements.push_back(arrangement);
   }
   for(int i = 0, count = component.getBusCount(Vst::kAudio, Vst::kOutput); i < count; ++i)
   {
      Vst::BusInfo busInfo {};
      Vst::SpeakerArrangement arrangement {0ull};
      if(component.getBusInfo(Vst::kAudio, Vst::kOutput, i, busInfo) == kResultOk)
      {
         if(busInfo.busType == Vst::kMain && busInfo.channelCount > 0)
            arrangement = (1ull << busInfo.channelCount) - 1ull;
      }
      if(component.activateBus(Vst::kAudio, Vst::kOutput, i, arrangement > 0) != kResultOk)
         arrangement = 0;

      defaultOutputSpeakerArrangements.push_back(arrangement);
   }
   for(int i = 0, count = component.getBusCount(Vst::kEvent, Vst::kInput); i < count; ++i)
      component.activateBus(Vst::kEvent, Vst::kInput, i, 0);
   for(int i = 0, count = component.getBusCount(Vst::kEvent, Vst::kOutput); i < count; ++i)
      component.activateBus(Vst::kEvent, Vst::kOutput, i, 0);

   processor->setBusArrangements(
      defaultInputSpeakerArrangements.empty() ? nullptr : defaultInputSpeakerArrangements.data(), defaultInputSpeakerArrangements.size(),
      defaultOutputSpeakerArrangements.empty() ? nullptr : defaultOutputSpeakerArrangements.data(), defaultOutputSpeakerArrangements.size()
   );
}

//The component should be disabled
bool SetupProcessing(Steinberg::Vst::IComponent& component, Steinberg::Vst::ProcessSetup& setup)
{
   using namespace Steinberg;
   auto processor = FUnknownPtr<Vst::IAudioProcessor>(&component);

   if(processor->setupProcessing(setup) == kResultOk)
   {
      ActivateMainAudioBuses(component);
      return true;
   }
   return false;
}

class InputParameterChanges final : public Steinberg::Vst::IParameterChanges
{
   const Steinberg::int32 mParameterCount;
   SingleInputParameterValue* const mParameterQueues;
public:

   InputParameterChanges(const std::vector<std::pair<Steinberg::Vst::ParamID, Steinberg::Vst::ParamValue>>& values, SingleInputParameterValue* queues)
      : mParameterQueues(queues), mParameterCount(values.size())
   {
      FUNKNOWN_CTOR

      int queueIndex{0};
      for(auto& p : values)
         queues[queueIndex++].Set(p.first, p.second);
   }

   ~InputParameterChanges()
   {
      FUNKNOWN_DTOR;
   }

   Steinberg::Vst::IParamValueQueue* PLUGIN_API
   addParameterData(const Steinberg::Vst::ParamID& id, Steinberg::int32& index) override
   {
      return nullptr;
   }

   Steinberg::int32 PLUGIN_API getParameterCount() override
   {
      return mParameterCount;
   }
   Steinberg::Vst::IParamValueQueue* PLUGIN_API getParameterData(Steinberg::int32 index) override
   {
      return &mParameterQueues[index];
   }

   DECLARE_FUNKNOWN_METHODS;
};

IMPLEMENT_FUNKNOWN_METHODS(InputParameterChanges, Steinberg::Vst::IParameterChanges,
                     Steinberg::Vst::IParameterChanges::iid);

class ComponentHandler : public Steinberg::Vst::IComponentHandler
{
   EffectSettingsAccess* mAccess{nullptr};
   //Used to prevent calls from non-UI thread
   const std::thread::id mThreadId;
public:
   
   ComponentHandler()
      : mThreadId(std::this_thread::get_id())
   {
      FUNKNOWN_CTOR;
   }
   virtual ~ComponentHandler() { FUNKNOWN_DTOR; }

   void SetAccess(EffectSettingsAccess* access) { mAccess = access; }

   EffectSettingsAccess* GetAccess() { return mAccess; }

   Steinberg::tresult PLUGIN_API beginEdit(Steinberg::Vst::ParamID id) override { return Steinberg::kResultOk; }

   Steinberg::tresult PLUGIN_API performEdit(Steinberg::Vst::ParamID id, Steinberg::Vst::ParamValue valueNormalized) override
   {
      if(std::this_thread::get_id() != mThreadId)
         return Steinberg::kResultFalse;

      if(mAccess)
      {
         mAccess->ModifySettings([&](EffectSettings& settings)
         {
            auto& vst3settings = GetSettings(settings);
            vst3settings.parameterChanges[id] = valueNormalized;
         });
      }

      return Steinberg::kResultOk;
   }

   Steinberg::tresult PLUGIN_API endEdit(Steinberg::Vst::ParamID id) override { return Steinberg::kResultOk; }

   Steinberg::tresult PLUGIN_API restartComponent(Steinberg::int32 flags) override { return Steinberg::kNotImplemented; }

   DECLARE_FUNKNOWN_METHODS
};

IMPLEMENT_FUNKNOWN_METHODS(ComponentHandler, Steinberg::Vst::IComponentHandler, Steinberg::Vst::IComponentHandler::iid)

}


void SingleInputParameterValue::Set(Steinberg::Vst::ParamID id, const Steinberg::Vst::ParamValue value)
{
   mParameterId = id;
   mValue = value;
}

Steinberg::tresult SingleInputParameterValue::addPoint(Steinberg::int32 sampleOffset, Steinberg::Vst::ParamValue value,
   Steinberg::int32& index)
{
   return Steinberg::kResultFalse;
}

Steinberg::Vst::ParamID SingleInputParameterValue::getParameterId()
{
   return mParameterId;
}

Steinberg::tresult SingleInputParameterValue::getPoint(Steinberg::int32 index, Steinberg::int32& sampleOffset,
   Steinberg::Vst::ParamValue& value)
{
   sampleOffset = 0;
   value = mValue;
   return Steinberg::kResultOk;
}

Steinberg::int32 SingleInputParameterValue::getPointCount()
{
   return 1;
}

IMPLEMENT_FUNKNOWN_METHODS(SingleInputParameterValue, Steinberg::Vst::IParamValueQueue, Steinberg::Vst::IParamValueQueue::iid);

VST3Wrapper::VST3Wrapper(VST3::Hosting::Module& module, VST3::UID effectUID)
   : mEffectUID(std::move(effectUID))
{
   using namespace Steinberg;

   const auto& pluginFactory = module.getFactory();

   auto effectComponent = pluginFactory.createInstance<Vst::IComponent>(mEffectUID);
   if(!effectComponent)
      throw std::runtime_error("Cannot create VST3 effect component");
   if(effectComponent->initialize(&AudacityVst3HostApplication::Get()) != kResultOk)
      throw std::runtime_error("Cannot initialize VST3 effect component");

   auto audioProcessor = FUnknownPtr<Vst::IAudioProcessor>(effectComponent);
   if(!audioProcessor)
      //It's stated that "This interface must always be supported by audio processing plug-ins."
      throw std::runtime_error("VST3 plugin does not provide audio processor interface");
   
   if(audioProcessor->canProcessSampleSize(Vst::kSample32) != kResultTrue)
      throw std::runtime_error("32-bit sample size not supported");

   mEffectComponent = effectComponent;
   mAudioProcessor = audioProcessor;

   auto editController = FUnknownPtr<Vst::IEditController>(mEffectComponent);
   if(editController.get() == nullptr)
   {
      TUID controllerCID;
            
		if (mEffectComponent->getControllerClassId (controllerCID) == kResultTrue)
         editController = pluginFactory.createInstance<Vst::IEditController>(VST3::UID(controllerCID));
   }

   if(editController.get() == nullptr)
      throw std::runtime_error("Failed to instantiate edit controller");
   
   mEditController = editController;
   mEditController->initialize(&AudacityVst3HostApplication::Get());

   mComponentHandler = owned(safenew ComponentHandler);
   mEditController->setComponentHandler(mComponentHandler);

   const auto componentConnectionPoint = FUnknownPtr<Vst::IConnectionPoint>{ mEffectComponent };
   const auto controllerConnectionPoint = FUnknownPtr<Vst::IConnectionPoint>{ mEditController };

   if (componentConnectionPoint && controllerConnectionPoint)
   {
      mComponentConnectionProxy = owned(safenew internal::ConnectionProxy(componentConnectionPoint));
      mControllerConnectionProxy = owned(safenew internal::ConnectionProxy(controllerConnectionPoint));

      mComponentConnectionProxy->connect(controllerConnectionPoint);
      mControllerConnectionProxy->connect(componentConnectionPoint);
   }

   mParameterQueues = std::make_unique<SingleInputParameterValue[]>(mEditController->getParameterCount());
   mParameters.reserve(mEditController->getParameterCount());

   Steinberg::MemoryStream stateStream;
   if(mEffectComponent->getState(&stateStream) == kResultOk)
   {
      int64 unused;
      stateStream.seek(0, IBStream::kIBSeekSet, &unused);
      mEditController->setComponentState(&stateStream);
   }

   mDefaultSettings = MakeSettings();
   StoreSettings(mDefaultSettings);
}

VST3Wrapper::~VST3Wrapper()
{
   using namespace Steinberg;

   if(mComponentConnectionProxy)
      mComponentConnectionProxy->disconnect(FUnknownPtr<Vst::IConnectionPoint>(mEditController));
   if(mControllerConnectionProxy)
      mControllerConnectionProxy->disconnect(FUnknownPtr<Vst::IConnectionPoint>(mEffectComponent));

   if(mEditController)
   {
      mEditController->setComponentHandler(nullptr);
      mEditController->terminate();
   }
   if(mEffectComponent)
      mEffectComponent->terminate();
}

void VST3Wrapper::FetchSettings(const EffectSettings& settings)
{
   const auto* vst3settings = &GetSettings(settings);
   if(!vst3settings->processorState.has_value())
   {
      vst3settings = &GetSettings(mDefaultSettings);
      if(!vst3settings->processorState.has_value())
         return;
   }

   auto processorState = PresetsBufferStream::fromString(*vst3settings->processorState);
   processorState->seek(0, Steinberg::IBStream::kIBSeekSet);
   if(mEffectComponent->setState(processorState) != Steinberg::kResultOk)
      return;

   processorState->seek(0, Steinberg::IBStream::kIBSeekSet);
   if(mEditController->setComponentState(processorState) != Steinberg::kResultOk)
      return;

   if(!vst3settings->controllerState.has_value())
      return;

   auto controllerState = PresetsBufferStream::fromString(*vst3settings->controllerState);
   controllerState->seek(0, Steinberg::IBStream::kIBSeekSet);
   mEditController->setState(controllerState);
}

void VST3Wrapper::StoreSettings(EffectSettings& settings) const
{
   using namespace Steinberg;
   auto& vst3settings = GetSettings(settings);

   vst3settings.processorState.reset();
   vst3settings.controllerState.reset();
   vst3settings.parameterChanges.clear();

   {
      PresetsBufferStream processorState;
      if(mEffectComponent->getState(&processorState) != kResultOk)
         return;
      vst3settings.processorState = processorState.toString();
   }
   PresetsBufferStream controllerState;
   if(mEditController->getState(&controllerState) == kResultOk)
      vst3settings.controllerState = controllerState.toString();
}

bool VST3Wrapper::LoadPreset(Steinberg::IBStream* fileStream)
{
   using namespace Steinberg;

   return Vst::PresetFile::loadPreset
   (
      fileStream,
      FUID::fromTUID(mEffectUID.data()),
      mEffectComponent.get(),
      mEditController.get()
   );
}


bool VST3Wrapper::SavePreset(Steinberg::IBStream* fileStream) const
{
   using namespace Steinberg;

   return Vst::PresetFile::savePreset
   (
      fileStream,
      FUID::fromTUID(mEffectUID.data()),
      mEffectComponent.get(),

      mEditController.get()
   );
}

bool VST3Wrapper::Initialize(const EffectSettings& settings, Steinberg::Vst::SampleRate sampleRate, Steinberg::int32 processMode, Steinberg::int32 maxSamplesPerBlock)
{
   using namespace Steinberg;

   FetchSettings(settings);

   auto setup = mSetup;
   setup.processMode = processMode;
   setup.sampleRate = sampleRate;
   setup.maxSamplesPerBlock = maxSamplesPerBlock;
   setup.symbolicSampleSize = Steinberg::Vst::kSample32;
   if(!SetupProcessing(*mEffectComponent.get(), setup))
      return false;
   mSetup = setup;
   
   if(mEffectComponent->setActive(true) == kResultOk)
   {
      mActive = true;
      mAudioProcessor->setProcessing(true);
      return true;
   }
   return false;
}

void VST3Wrapper::Finalize()
{
   mActive = false;
   mAudioProcessor->setProcessing(false);
   mEffectComponent->setActive(false);
}

void VST3Wrapper::ConsumeChanges(EffectSettings& settings)
{
   const auto& vst3settings = GetSettings(settings);
   for(auto& p : vst3settings.parameterChanges)
   {
      auto it = std::find_if(mParameters.begin(), mParameters.end(), [&p](const auto& v) { return v.first == p.first; });
      if(it != mParameters.end())
         it->second = p.second;
      else
         mParameters.push_back(p);
   }
}

//Used as a workaround for issue #2555: some plugins do not accept changes
//via IEditController::setParamNormalized, but seem to read current
//parameter values directly from the DSP model.
//
//When processing is disabled this call helps synchronize internal state of the
//IEditController, so that next time UI is opened it displays correct values.
//
//As a side effect subsequent call to IAudioProcessor::process may flush
//plugin internal buffers
void VST3Wrapper::FlushSettings(EffectSettings& settings)
{
   if(!mActive)
   {
      ConsumeChanges(settings);
      if(mEffectComponent->setActive(true) == Steinberg::kResultOk)
      {
         mAudioProcessor->setProcessing(true);
         Process(nullptr, nullptr, 0);
         mAudioProcessor->setProcessing(false);
         mEffectComponent->setActive(false);
      }
   }
}

size_t VST3Wrapper::Process(const float* const* inBlock, float* const* outBlock, size_t blockLen)
{
   using namespace Steinberg;
   
   InputParameterChanges inputParameterChanges(mParameters, mParameterQueues.get());
   mParameters.clear();

   Vst::ProcessData data;
   data.processMode = mSetup.processMode;
   data.symbolicSampleSize = mSetup.symbolicSampleSize;
   data.inputParameterChanges = &inputParameterChanges;

   static_assert(std::numeric_limits<decltype(blockLen)>::max()
      >= std::numeric_limits<decltype(data.numSamples)>::max());

   data.numSamples = static_cast<decltype(data.numSamples)>(std::min(
      blockLen, 
      static_cast<decltype(blockLen)>(mSetup.maxSamplesPerBlock)
   ));

   data.numInputs = inBlock == nullptr ? 0 : mEffectComponent->getBusCount(Vst::kAudio, Vst::kInput);
   data.numOutputs = outBlock == nullptr ? 0 : mEffectComponent->getBusCount(Vst::kAudio, Vst::kOutput);

   if(data.numInputs > 0)
   {
      int inputBlocksOffset {0};

      data.inputs = static_cast<Vst::AudioBusBuffers*>(
         alloca(sizeof(Vst::AudioBusBuffers) * data.numInputs));

      for(int busIndex = 0; busIndex < data.numInputs; ++busIndex)
      {
         Vst::BusInfo busInfo { };
         if(mEffectComponent->getBusInfo(Vst::kAudio, Vst::kInput, busIndex, busInfo) != kResultOk)
         {
            return 0;
         }
         if(busInfo.busType == Vst::kMain)
         {
            data.inputs[busIndex].numChannels = busInfo.channelCount;
            data.inputs[busIndex].channelBuffers32 = const_cast<float**>(inBlock + inputBlocksOffset);
            inputBlocksOffset += busInfo.channelCount;
         }
         else
         {
            //aux is not yet supported
            data.inputs[busIndex].numChannels = 0;
            data.inputs[busIndex].channelBuffers32 = nullptr;
         }
         data.inputs[busIndex].silenceFlags = 0UL;
      }
   }
   if(data.numOutputs > 0)
   {
      int outputBlocksOffset {0};

      data.outputs = static_cast<Vst::AudioBusBuffers*>(
         alloca(sizeof(Vst::AudioBusBuffers) * data.numOutputs));
      for(int busIndex = 0; busIndex < data.numOutputs; ++busIndex)
      {
         Vst::BusInfo busInfo { };
         if(mEffectComponent->getBusInfo(Vst::kAudio, Vst::kOutput, busIndex, busInfo) != kResultOk)
         {
            return 0;
         }
         if(busInfo.busType == Vst::kMain)
         {
            data.outputs[busIndex].numChannels = busInfo.channelCount;
            data.outputs[busIndex].channelBuffers32 = const_cast<float**>(outBlock + outputBlocksOffset);
            outputBlocksOffset += busInfo.channelCount;
         }
         else
         {
            //aux is not yet supported
            data.outputs[busIndex].numChannels = 0;
            data.outputs[busIndex].channelBuffers32 = nullptr;
         }
         data.outputs[busIndex].silenceFlags = 0UL;
      }
   }

   const auto processResult = mAudioProcessor->process(data);
   
   return processResult == kResultOk ?
      data.numSamples : 0;
}


void VST3Wrapper::SuspendProcessing()
{
   mAudioProcessor->setProcessing(false);
}

void VST3Wrapper::ResumeProcessing()
{
   mAudioProcessor->setProcessing(true);
}

void VST3Wrapper::BeginParameterEdit(EffectSettingsAccess& access)
{
   static_cast<ComponentHandler*>(mComponentHandler.get())->SetAccess(&access);
}

void VST3Wrapper::EndParameterEdit()
{
   static_cast<ComponentHandler*>(mComponentHandler.get())->SetAccess(nullptr);
}



Steinberg::int32 VST3Wrapper::GetLatencySamples() const
{
   return mAudioProcessor->getLatencySamples();
}

EffectSettings VST3Wrapper::MakeSettings()
{
   return EffectSettings::Make<VST3EffectSettings>();
}

void VST3Wrapper::LoadSettings(const CommandParameters& parms, EffectSettings& settings)
{
   auto& vst3settings = GetSettings(settings);
   vst3settings.processorState.reset();
   vst3settings.controllerState.reset();
   vst3settings.parameterChanges.clear();

   if(parms.HasEntry(processorStateKey))
   {
      vst3settings.processorState = parms.Read(processorStateKey);
      if(parms.HasEntry(controllerStateKey))
         vst3settings.controllerState = parms.Read(controllerStateKey);
   }
}

void VST3Wrapper::SaveSettings(const EffectSettings& settings, CommandParameters& parms)
{
   const auto& vst3settings = GetSettings(settings);

   if(vst3settings.processorState.has_value())
      parms.Write(processorStateKey, *vst3settings.processorState);
   if(vst3settings.controllerState.has_value())
      parms.Write(controllerStateKey, *vst3settings.controllerState);
}

void VST3Wrapper::LoadUserPreset(const EffectDefinitionInterface& effect, const RegistryPath& name, EffectSettings& settings)
{
   auto& vst3settings = GetSettings(settings);

   vst3settings.processorState.reset();
   vst3settings.controllerState.reset();
   vst3settings.parameterChanges.clear();

   wxString processorStateStr;
   if(GetConfig(effect, PluginSettings::Private, name, processorStateKey, processorStateStr, wxEmptyString))
   {
      vst3settings.processorState = processorStateStr;
      wxString controllerStateStr;
      if(GetConfig(effect, PluginSettings::Private, name, controllerStateKey, controllerStateStr, wxEmptyString))
         vst3settings.controllerState = controllerStateStr;
   }
}

void VST3Wrapper::SaveUserPreset(const EffectDefinitionInterface& effect, const RegistryPath& name, const EffectSettings& settings)
{
   using namespace Steinberg;

   const auto& vst3settings = GetSettings(settings);
   if(vst3settings.processorState.has_value())
   {
      SetConfig(effect, PluginSettings::Private, name, processorStateKey, *vst3settings.processorState);
      if(vst3settings.controllerState.has_value())
         SetConfig(effect, PluginSettings::Private, name, controllerStateKey, *vst3settings.controllerState);
   }
}
