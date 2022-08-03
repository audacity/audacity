#include "VST3Wrapper.h"
#include <stdexcept>

#include "EffectInterface.h"

#include "AudacityVst3HostApplication.h"

#include <pluginterfaces/vst/ivsteditcontroller.h>
#include <pluginterfaces/vst/ivstparameterchanges.h>

#include "memorystream.h"

#include "VST3Utils.h"
#include "internal/ConnectionProxy.h"
#include "internal/ComponentHandler.h"

namespace
{
   
bool SyncComponentStates(Steinberg::Vst::IComponent& component, Steinberg::Vst::IEditController& editController)
{
   using namespace Steinberg;

   Steinberg::MemoryStream stateStream;
   if(component.getState(&stateStream) == kResultOk)
   {
      int64 unused;
      stateStream.seek(0, IBStream::kIBSeekSet, &unused);
      return editController.setComponentState(&stateStream) == kResultOk;
   }
   return false;
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
   const VST3EffectSettings& mVST3Settings;
   const InputParameterValueQueue* mParameterQueues;
public:

   InputParameterChanges(const VST3EffectSettings& vst3settings, InputParameterValueQueue* queues)
      : mVST3Settings(vst3settings), mParameterQueues(queues)
   {
      FUNKNOWN_CTOR

      int queueIndex{0};
      for(auto& p : vst3settings.parameterChanges)
         queues[queueIndex++].Bind(p.first, &p.second);
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
      return mVST3Settings.parameterChanges.size();
   }
   Steinberg::Vst::IParamValueQueue* PLUGIN_API getParameterData(Steinberg::int32 index) override
   {
      return const_cast<InputParameterValueQueue*>(&mParameterQueues[index]);
   }

   DECLARE_FUNKNOWN_METHODS;
};

IMPLEMENT_FUNKNOWN_METHODS(InputParameterChanges, Steinberg::Vst::IParameterChanges,
                        Steinberg::Vst::IParameterChanges::iid);

}


void VST3Wrapper::InitComponents()
{
   using namespace Steinberg;

   const auto& pluginFactory = mModule->getFactory();

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
}


void InputParameterValueQueue::Bind(Steinberg::Vst::ParamID id, const Steinberg::Vst::ParamValue* values)
{
   mParameterId = id;
   mValues = values;
}

Steinberg::tresult InputParameterValueQueue::addPoint(Steinberg::int32 sampleOffset, Steinberg::Vst::ParamValue value,
   Steinberg::int32& index)
{
   return Steinberg::kResultFalse;
}

Steinberg::Vst::ParamID InputParameterValueQueue::getParameterId()
{
   return mParameterId;
}

Steinberg::tresult InputParameterValueQueue::getPoint(Steinberg::int32 index, Steinberg::int32& sampleOffset,
   Steinberg::Vst::ParamValue& value)
{
   sampleOffset = 0;
   value = mValues[index];
   return Steinberg::kResultOk;
}

Steinberg::int32 InputParameterValueQueue::getPointCount()
{
   return 1;
}

IMPLEMENT_FUNKNOWN_METHODS(InputParameterValueQueue, Steinberg::Vst::IParamValueQueue, Steinberg::Vst::IParamValueQueue::iid);

VST3Wrapper::VST3Wrapper(std::shared_ptr<VST3::Hosting::Module> module, VST3::UID effectUID)
   : mEffectUID(std::move(effectUID)), mModule(std::move(module))
{
   using namespace Steinberg;
   InitComponents();

   const auto& pluginFactory = mModule->getFactory();
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

   mComponentHandler = owned(safenew internal::ComponentHandler());
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

   SyncComponentStates(*mEffectComponent.get(), *mEditController.get());
   mParameterQueues = std::make_unique<InputParameterValueQueue[]>(mEditController->getParameterCount());
}

VST3Wrapper::VST3Wrapper(const VST3Wrapper& other)
   : mModule(other.mModule), mEffectUID(other.mEffectUID)
{
   mSetup = other.mSetup;
   InitComponents();
   mParameterQueues = std::make_unique<InputParameterValueQueue[]>(other.mEditController->getParameterCount());
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


bool VST3Wrapper::FetchSettings(VST3EffectSettings& vst3Settings) const
{
   using namespace Steinberg;

   auto processorState = owned(safenew PresetsBufferStream);
   if (mEffectComponent->getState(processorState) == kResultOk)
   {
      vst3Settings.mProcessorStateStr = processorState->toString();
   }
   else
   {
      vst3Settings.mProcessorStateStr = std::nullopt;
   }

   auto controllerState = owned(safenew PresetsBufferStream);
   if (mEditController->getState(controllerState) == kResultOk)
   {
      vst3Settings.mControllerStateStr = controllerState->toString();
   }
   else
   {
      vst3Settings.mControllerStateStr = std::nullopt;
   }

   return true;   
}



bool VST3Wrapper::StoreSettings(const VST3EffectSettings& vst3settings) const
{
   using namespace Steinberg;

   // we need at least the processor state string, otherwise we can not set the EditController
   if (!vst3settings.mProcessorStateStr)
      return false;

   auto processorState = PresetsBufferStream::fromString(*vst3settings.mProcessorStateStr);
   if (mEffectComponent->setState(processorState) != kResultOk)
      return false;

   if (vst3settings.mControllerStateStr)
   {
      auto controllerState = PresetsBufferStream::fromString(*vst3settings.mControllerStateStr);

      if (mEditController->setComponentState(processorState) != kResultOk ||
         mEditController->setState(controllerState) != kResultOk)
         return false;
   }

   return true;
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

bool VST3Wrapper::Initialize(Steinberg::Vst::SampleRate sampleRate, Steinberg::int32 processMode, Steinberg::int32 maxSamplesPerBlock)
{
   auto setup = mSetup;
   setup.processMode = processMode;
   setup.sampleRate = sampleRate;
   setup.maxSamplesPerBlock = maxSamplesPerBlock;
   setup.symbolicSampleSize = Steinberg::Vst::kSample32;
   if(!SetupProcessing(*mEffectComponent.get(), setup))
      return false;
   mSetup = setup;
   
   using namespace Steinberg;
   
   if(mEffectComponent->setActive(true) == kResultOk)
   {
      mActive = true;
      mAudioProcessor->setProcessing(true);
      return true;
   }
   return false;
}

size_t VST3Wrapper::Process(const EffectSettings& settings, const float* const* inBlock, float* const* outBlock,
                            size_t blockLen)
{
   using namespace Steinberg;

   const auto vst3settings = GetSettings(settings);
   InputParameterChanges inputParameterChanges(vst3settings, mParameterQueues.get());

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

void VST3Wrapper::Finalize()
{
   mActive = false;
   mAudioProcessor->setProcessing(false);
   mEffectComponent->setActive(false);
}

void VST3Wrapper::SuspendProcessing()
{
   mAudioProcessor->setProcessing(false);
}

void VST3Wrapper::ResumeProcessing()
{
   mAudioProcessor->setProcessing(true);
}

Steinberg::int32 VST3Wrapper::GetLatencySamples() const
{
   return mAudioProcessor->getLatencySamples();
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
void VST3Wrapper::FlushSettings(const EffectSettings& settings)
{
   if(mComponentHandler && !mActive)
   {
      if(mEffectComponent->setActive(true) == Steinberg::kResultOk)
      {
         mAudioProcessor->setProcessing(true);
         Process(settings, nullptr, nullptr, 0);
         mAudioProcessor->setProcessing(false);
         mEffectComponent->setActive(false);
      }
   }
}

EffectSettings VST3Wrapper::MakeSettings()
{
   return EffectSettings::Make<VST3EffectSettings>();
}

VST3EffectSettings& VST3Wrapper::GetSettings(EffectSettings& settings)
{
   auto vst3settings = settings.cast<VST3EffectSettings>();
   assert(vst3settings);
   return *vst3settings;
}

const VST3EffectSettings& VST3Wrapper::GetSettings(const EffectSettings& settings)
{
   auto vst3settings = settings.cast<const VST3EffectSettings>();
   assert(vst3settings);
   return *vst3settings;
}


