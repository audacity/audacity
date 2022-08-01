#include "VST3Wrapper.h"
#include <stdexcept>

#include "AudacityVst3HostApplication.h"

#include <pluginterfaces/vst/ivsteditcontroller.h>

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

}


void VST3Wrapper::Initialize()
{
   using namespace Steinberg;

   const auto& pluginFactory = mModule->getFactory();

   auto effectComponent = pluginFactory.createInstance<Vst::IComponent>(mEffectClassInfo.ID());
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


VST3Wrapper::VST3Wrapper(std::shared_ptr<VST3::Hosting::Module> module, VST3::Hosting::ClassInfo effectClassInfo)
   : mEffectClassInfo(std::move(effectClassInfo)), mModule(std::move(module))
{
   using namespace Steinberg;
   Initialize();

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

   SyncComponentStates(*mEffectComponent.get(), *mEditController.get());
}

VST3Wrapper::VST3Wrapper(const VST3Wrapper& other)
   : mModule(other.mModule), mEffectClassInfo(other.mEffectClassInfo)
{
   mSetup = other.mSetup;
   Initialize();
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
      FUID::fromTUID(mEffectClassInfo.ID().data()),
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
      FUID::fromTUID(mEffectClassInfo.ID().data()),
      mEffectComponent.get(),
      mEditController.get()
   );
}
