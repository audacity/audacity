#include "VST3Wrapper.h"
#include <stdexcept>
#include <optional>
#include <map>

#include "EffectInterface.h"

#include "AudacityVst3HostApplication.h"

#include <pluginterfaces/vst/ivsteditcontroller.h>
#include <pluginterfaces/vst/ivstparameterchanges.h>
#include <wx/tokenzr.h>

#include "ConfigInterface.h"
#include "memorystream.h"
#include "MemoryX.h"
#include "VST3Utils.h"
#include "internal/ConnectionProxy.h"

namespace
{

// define some shared registry keys
constexpr auto processorStateKey  = wxT("ProcessorState");
constexpr auto controllerStateKey = wxT("ControllerState");
constexpr auto parametersKey = wxT("Parameters");

Steinberg::Vst::SpeakerArrangement GetBusArragementForChannels(
   int32_t channelsCount, Steinberg::Vst::SpeakerArrangement defaultArragment)
{
   if (channelsCount == 1)
      return defaultArragment;

   return Steinberg::Vst::SpeakerArr::kStereo;
}

struct VST3EffectSettings
{
   ///Holds the parameter that has been changed since last processing pass.
   std::map<Steinberg::Vst::ParamID, Steinberg::Vst::ParamValue> parameterChanges;
   ///Incremented whenever changes has been made to parameters, helps to avoid
   ///redundant store operations when processing is active
   int changesCounter { 0 };

   ///Holds the last known processor/component state, rarely updates (usually only on UI or preset change)
   std::optional<wxString> processorState;
   ///Holds the last known controller state, rarely updates (usually only on UI or preset change)
   std::optional<wxString> controllerState;
};

std::map<Steinberg::Vst::ParamID, Steinberg::Vst::ParamValue> ParametersFromString(const wxString& str)
{
   std::map<Steinberg::Vst::ParamID, Steinberg::Vst::ParamValue> result;
   wxStringTokenizer tokenizer(str, ";");
   while(tokenizer.HasMoreTokens())
   {
      auto token = tokenizer.GetNextToken();

      const auto split = token.Find('=');
      if(split == wxNOT_FOUND)
         continue;

      unsigned long id;
      double value;
      if(!token.Left(split).ToULong(&id) ||
         !token.Right(token.Length() - split - 1).ToDouble(&value))
         continue;

      result[id] = value;
   }
   return result;
}

wxString ParametersToString(const std::map<Steinberg::Vst::ParamID, Steinberg::Vst::ParamValue>& params)
{
   wxString result;
   for(auto& p : params)
      result.Append(wxString::Format(
         "%lu=%f;", static_cast<unsigned long>(p.first), p.second));
   return result;
}

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
bool ActivateMainAudioBuses(Steinberg::Vst::IComponent& component)
{
   using namespace Steinberg;

   constexpr int32 MaxChannelsPerAudioBus = 2;

   std::vector<Vst::SpeakerArrangement> defaultInputSpeakerArrangements;
   std::vector<Vst::SpeakerArrangement> defaultOutputSpeakerArrangements;

   const auto processor = FUnknownPtr<Vst::IAudioProcessor>(&component);

   for(int i = 0, count = component.getBusCount(Vst::kAudio, Vst::kInput); i < count; ++i)
   {
      Vst::BusInfo busInfo {};
      Vst::SpeakerArrangement arrangement {0ull};
      
      component.getBusInfo(Vst::kAudio, Vst::kInput, i, busInfo);

      Vst::SpeakerArrangement defaultArragement {};
      processor->getBusArrangement(Vst::kInput, i, defaultArragement);

      arrangement =
         GetBusArragementForChannels(busInfo.channelCount, defaultArragement);
      
      component.activateBus(Vst::kAudio, Vst::kInput, i, busInfo.busType == Vst::kMain);

      defaultInputSpeakerArrangements.push_back(arrangement);
   }
   for(int i = 0, count = component.getBusCount(Vst::kAudio, Vst::kOutput); i < count; ++i)
   {
      Vst::BusInfo busInfo {};
      Vst::SpeakerArrangement arrangement {0ull};
      
      component.getBusInfo(Vst::kAudio, Vst::kOutput, i, busInfo);

      Vst::SpeakerArrangement defaultArragement {};
      processor->getBusArrangement(Vst::kOutput, i, defaultArragement);

      arrangement =
         busInfo.busType == Vst::kMain ?
            GetBusArragementForChannels(busInfo.channelCount, defaultArragement) :
            Vst::SpeakerArr::kEmpty;

      component.activateBus(Vst::kAudio, Vst::kOutput, i, busInfo.busType == Vst::kMain);
      defaultOutputSpeakerArrangements.push_back(arrangement);
   }
   for(int i = 0, count = component.getBusCount(Vst::kEvent, Vst::kInput); i < count; ++i)
      component.activateBus(Vst::kEvent, Vst::kInput, i, 0);
   for(int i = 0, count = component.getBusCount(Vst::kEvent, Vst::kOutput); i < count; ++i)
      component.activateBus(Vst::kEvent, Vst::kOutput, i, 0);

   auto result = processor->setBusArrangements(
      defaultInputSpeakerArrangements.empty() ? nullptr : defaultInputSpeakerArrangements.data(), defaultInputSpeakerArrangements.size(),
      defaultOutputSpeakerArrangements.empty() ? nullptr : defaultOutputSpeakerArrangements.data(), defaultOutputSpeakerArrangements.size()
   );

   return result == kResultOk;
}

//The component should be disabled
bool SetupProcessing(Steinberg::Vst::IComponent& component, Steinberg::Vst::ProcessSetup& setup)
{
   using namespace Steinberg;
   auto processor = FUnknownPtr<Vst::IAudioProcessor>(&component);

   if(processor->setupProcessing(setup) == kResultOk)
   {
      //We don't (yet) support custom input/output channel configuration
      //on the host side. No support for event bus. Use default bus and
      //channel configuration
      return ActivateMainAudioBuses(component);
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
   VST3Wrapper& mWrapper;
   EffectSettingsAccess* mAccess{nullptr};
   //Used to prevent calls from non-UI thread
   const std::thread::id mThreadId;
   
   EffectSettings* mStateChangeSettings {nullptr};
   std::map<Steinberg::Vst::ParamID, Steinberg::Vst::ParamValue> mParametersCache;
public:
   
   ComponentHandler(VST3Wrapper& wrapper)
      : mThreadId(std::this_thread::get_id()), mWrapper(wrapper)
   {
      FUNKNOWN_CTOR;
   }
   virtual ~ComponentHandler() { FUNKNOWN_DTOR; }

   void SetAccess(EffectSettingsAccess* access) { mAccess = access; }

   EffectSettingsAccess* GetAccess() { return mAccess; }

   void BeginStateChange(EffectSettings& settings)
   {
      mStateChangeSettings = &settings;
   }

   void EndStateChange()
   {
      assert(mStateChangeSettings != nullptr);
      if(!mParametersCache.empty())
         FlushCache(*mStateChangeSettings);
      mStateChangeSettings = nullptr;
   }

   void FlushCache(EffectSettings& settings)
   {
      if(mParametersCache.empty())
         return;

      auto& vst3settings = GetSettings(settings);
      for(auto& p : mParametersCache)
         vst3settings.parameterChanges[p.first] = p.second;
      mParametersCache.clear();
      ++vst3settings.changesCounter;
   }

   void ResetCache()
   {
      mParametersCache.clear();
   }

   Steinberg::tresult PLUGIN_API beginEdit(Steinberg::Vst::ParamID id) override { return Steinberg::kResultOk; }

   Steinberg::tresult PLUGIN_API performEdit(Steinberg::Vst::ParamID id, Steinberg::Vst::ParamValue valueNormalized) override
   {
      if(std::this_thread::get_id() != mThreadId)
         return Steinberg::kResultFalse;

      if(mStateChangeSettings != nullptr || !mWrapper.IsActive())
         // Collecting edit callbacks from the plug-in, in response to changes
         // of complete state, while doing FetchSettings() (which may be delayed...)
         mParametersCache[id] = valueNormalized;
      else if(mAccess)
      {
         mAccess->ModifySettings([&](EffectSettings& settings)
         {
            auto& vst3settings = GetSettings(settings);
            vst3settings.parameterChanges[id] = valueNormalized;
            ++vst3settings.changesCounter;
            return nullptr;
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
   , mModule{ module }
{
   using namespace Steinberg;

   //Preinitialize with some default values in case if parameters
   //flush happens before processing initialized
   mSetup.maxSamplesPerBlock = 512;
   mSetup.processMode = Vst::kOffline;
   mSetup.symbolicSampleSize = Vst::kSample32;
   mSetup.sampleRate = 44100.0;

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

   if(!SetupProcessing(*mEffectComponent, mSetup))
      throw std::runtime_error("bus configuration not supported");

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

   mComponentHandler = owned(safenew ComponentHandler(*this));
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

bool VST3Wrapper::IsActive() const noexcept
{
   return mActive;
}

void VST3Wrapper::FetchSettings(EffectSettings& settings)
{
   //TODO: perform version check
   {
      auto componentHandler = static_cast<ComponentHandler*>(mComponentHandler.get());
      componentHandler->ResetCache();
      componentHandler->BeginStateChange(settings);
      auto cleanup = finally([&] { componentHandler->EndStateChange(); });

      //Restore state
      const auto* vst3settings = &GetSettings(settings);
      if(!vst3settings->processorState.has_value())
         vst3settings = &GetSettings(mDefaultSettings);

      if(vst3settings->processorState.has_value())
      {
         auto processorState = PresetsBufferStream::fromString(*vst3settings->processorState);
         processorState->seek(0, Steinberg::IBStream::kIBSeekSet);
         if(mEffectComponent->setState(processorState) == Steinberg::kResultOk)
         {
            processorState->seek(0, Steinberg::IBStream::kIBSeekSet);
            if(mEditController->setComponentState(processorState) == Steinberg::kResultOk)
            {
               if(vst3settings->controllerState.has_value())
               {
                  auto controllerState = PresetsBufferStream::fromString(*vst3settings->controllerState);
                  controllerState->seek(0, Steinberg::IBStream::kIBSeekSet);
                  mEditController->setState(controllerState);
               }
            }
         }
      }
   }
   //restore parameters if present
   auto& vst3setting = GetSettings(settings);
   for(auto& p : vst3setting.parameterChanges)
      mEditController->setParamNormalized(p.first, p.second);
}

void VST3Wrapper::StoreSettings(EffectSettings& settings) const
{
   using namespace Steinberg;
   
   VST3EffectSettings vst3settings;

   {
      PresetsBufferStream processorState;
      if(mEffectComponent->getState(&processorState) == kResultOk)
         vst3settings.processorState = processorState.toString();
   }
   {
      PresetsBufferStream controllerState;
      if(mEditController->getState(&controllerState) == kResultOk)
         vst3settings.controllerState = controllerState.toString();
   }
   
   std::swap(vst3settings, GetSettings(settings));
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

bool VST3Wrapper::Initialize(EffectSettings& settings, Steinberg::Vst::SampleRate sampleRate, Steinberg::int32 processMode, Steinberg::int32 maxSamplesPerBlock)
{
   using namespace Steinberg;

   Vst::ProcessSetup setup = {
      processMode,
      Vst::kSample32,
      maxSamplesPerBlock,
      sampleRate
   };
   
   if(!SetupProcessing(*mEffectComponent.get(), setup))
      return false;

   mSetup = setup;

   FetchSettings(settings);

   if(mEffectComponent->setActive(true) == kResultOk)
   {
      if(mAudioProcessor->setProcessing(true) != kResultFalse)
      {
         mActive = true;
         ConsumeChanges(settings);
         //make zero-flush, to make sure parameters are delivered to the processor...
         Process(nullptr, nullptr, 0);
         StoreSettings(settings);
         return true;
      }
   }
   return false;
}

void VST3Wrapper::Finalize(EffectSettings* settings)
{
   //Could be FlushParameters but processor is already configured
   //If no calls to process were performed deliver changes here
   if(settings != nullptr)
   {
      ConsumeChanges(*settings);
      Process(nullptr, nullptr, 0);
   }
   mAudioProcessor->setProcessing(false);
   mEffectComponent->setActive(false);
   mActive = false;

   if(settings != nullptr)
      StoreSettings(*settings);
}

void VST3Wrapper::ProcessBlockStart(const EffectSettings& settings)
{
   ConsumeChanges(settings);
}

void VST3Wrapper::ConsumeChanges(const EffectSettings& settings)
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
void VST3Wrapper::FlushParameters(EffectSettings& settings)
{
   if(!mActive)
   {
      auto componentHandler = static_cast<ComponentHandler*>(mComponentHandler.get());
      componentHandler->FlushCache(settings);

      SetupProcessing(*mEffectComponent, mSetup);
      mActive = true;
      if(mEffectComponent->setActive(true) == Steinberg::kResultOk)
      {
         ConsumeChanges(settings);
         if(mAudioProcessor->setProcessing(true) != Steinberg::kResultFalse)
         {
            Process(nullptr, nullptr, 0);
            mAudioProcessor->setProcessing(false);
         }
      }
      mEffectComponent->setActive(false);
      mActive = false;
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
   auto componentHandler = static_cast<ComponentHandler*>(mComponentHandler.get());
   componentHandler->SetAccess(nullptr);
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
   VST3EffectSettings vst3settings;

   if(parms.HasEntry(processorStateKey))
   {
      vst3settings.processorState = parms.Read(processorStateKey);
      if(parms.HasEntry(controllerStateKey))
         vst3settings.controllerState = parms.Read(controllerStateKey);
   }
   if(parms.HasEntry(parametersKey))
      vst3settings.parameterChanges = ParametersFromString(parms.Read(parametersKey));

   std::swap(vst3settings, GetSettings(settings));
}

void VST3Wrapper::SaveSettings(const EffectSettings& settings, CommandParameters& parms)
{
   const auto& vst3settings = GetSettings(settings);

   if(vst3settings.processorState.has_value())
      parms.Write(processorStateKey, *vst3settings.processorState);
   if(vst3settings.controllerState.has_value())
      parms.Write(controllerStateKey, *vst3settings.controllerState);
   if(!vst3settings.parameterChanges.empty())
      parms.Write(parametersKey, ParametersToString(vst3settings.parameterChanges)); 
}

OptionalMessage VST3Wrapper::LoadUserPreset(
   const EffectDefinitionInterface& effect, const RegistryPath& name, EffectSettings& settings)
{
   VST3EffectSettings vst3settings;
   
   wxString processorStateStr;
   if(GetConfig(effect, PluginSettings::Private, name, processorStateKey, processorStateStr, wxEmptyString))
   {
      vst3settings.processorState = processorStateStr;
      wxString controllerStateStr;
      if(GetConfig(effect, PluginSettings::Private, name, controllerStateKey, controllerStateStr, wxEmptyString))
         vst3settings.controllerState = controllerStateStr;
   }
   wxString parametersStr;
   if(GetConfig(effect, PluginSettings::Private, name, parametersKey, parametersStr, wxEmptyString))
      vst3settings.parameterChanges = ParametersFromString(parametersStr);

   std::swap(vst3settings, GetSettings(settings));
   return { nullptr };
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
   if(!vst3settings.parameterChanges.empty())
      SetConfig(effect, PluginSettings::Private, name, parametersKey, ParametersToString(vst3settings.parameterChanges));
}

void VST3Wrapper::CopySettingsContents(const EffectSettings& src, EffectSettings& dst)
{
   auto& from = GetSettings(*const_cast<EffectSettings*>(&src));
   auto& to = GetSettings(dst);

   to.changesCounter = from.changesCounter;
   //Don't allocate in worker
   std::swap(from.parameterChanges, to.parameterChanges);
}
