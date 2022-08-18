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
#include "internal/ComponentHandler.h"

namespace
{

// define some shared registry keys
constexpr auto processorStateKey  = wxT("ProcessorState");
constexpr auto controllerStateKey = wxT("ControllerState");

struct VST3EffectSettings
{
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

}

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

   mComponentHandler = owned(safenew internal::ComponentHandler);
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
