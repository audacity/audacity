/**********************************************************************

  Audacity: A Digital Audio Editor

  @file VST3Effect.cpp

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#include "VST3Effect.h"

#include <pluginterfaces/vst/ivsteditcontroller.h>
#include <public.sdk/source/vst/hosting/hostclasses.h>

#include "VST3Utils.h"

namespace {

Steinberg::Vst::IHostApplication& LocalContext()
{
   static Steinberg::Vst::HostApplication localContext;
   return localContext;
}

void ActivateDefaultBuses(Steinberg::Vst::IComponent* component, const Steinberg::Vst::MediaType mediaType, const Steinberg::Vst::BusDirection direction, bool state)
{
   using namespace Steinberg;

   Vst::BusInfo busInfo;
   for(int i = 0, count = component->getBusCount(mediaType, direction); i < count; ++i)
      if(component->getBusInfo(mediaType, direction, i, busInfo) == kResultOk &&
         busInfo.flags & Vst::BusInfo::kDefaultActive)
      {
         component->activateBus(mediaType, direction, i, state ? 1 : 0);
      }
}

}

void VST3Effect::Initialize()
{
   using namespace Steinberg;

   const auto& pluginFactory = mModule->getFactory();
   if(auto effectComponent = pluginFactory.createInstance<Vst::IComponent>(mEffectClassInfo.ID()))
   {
      if(effectComponent->initialize(&LocalContext()) == kResultOk)
      {
         if(auto audioProcessor = FUnknownPtr<Vst::IAudioProcessor>(effectComponent))
         {
            mEffectComponent = effectComponent;
            mAudioProcessor = audioProcessor;
         }
         else
            //It's stated that "This interface must always be supported by audio processing plug-ins."
            throw std::runtime_error("VST3 plugin does not provide audio processor interface");
      }
      else
         throw std::runtime_error("Cannot initialize VST3 effect component");
   }
   else
      throw std::runtime_error("Cannot create VST3 effect component");

   //defaults
   mSetup.processMode = Vst::kOffline;
   mSetup.symbolicSampleSize = Vst::kSample32;
   mSetup.maxSamplesPerBlock = mUserBlockSize;
   mSetup.sampleRate = 44100.0;

   ActivateDefaultBuses(mEffectComponent.get(), Vst::kAudio, Vst::kInput, true);
   ActivateDefaultBuses(mEffectComponent.get(), Vst::kAudio, Vst::kOutput, true);
}

EffectFamilySymbol VST3Effect::GetFamilySymbol()
{
   return XO("VST3");
}

VST3Effect::VST3Effect(const VST3Effect& other)
   : mModule(other.mModule), mEffectClassInfo(other.mEffectClassInfo)
{
   mUseLatency = other.mUseLatency;
   mUserBlockSize = other.mUserBlockSize;
   Initialize();
   mSetup = other.mSetup;
}

VST3Effect::~VST3Effect()
{
   using namespace Steinberg;

   CloseUI();
   
   if(mEditController)
   {
      mEditController->setComponentHandler(nullptr);
      mEditController->terminate();
   }
   if(mEffectComponent)
      mEffectComponent->terminate();
}

VST3Effect::VST3Effect(
   std::shared_ptr<VST3::Hosting::Module> module, 
   VST3::Hosting::ClassInfo effectClassInfo)
   : mModule(std::move(module)), mEffectClassInfo(std::move(effectClassInfo))
{
   using namespace Steinberg;
   Initialize();

   const auto& pluginFactory = mModule->getFactory();
   auto editController = FUnknownPtr<Vst::IEditController>(mEffectComponent);
   if(!editController)
   {
      //Plugin does not have a separate edit controller
      TUID controllerCID;
            
		if (mEffectComponent->getControllerClassId (controllerCID) == kResultTrue)
		{
			if(mEditController = pluginFactory.createInstance<Vst::IEditController> (VST3::UID (controllerCID)))
			{
            if(mEditController->initialize(&LocalContext()) != kResultOk)
            {
               wxLogMessage("Cannot create edit controller: initialization failed");
               mEditController = nullptr;
            }
			}
         else
            wxLogMessage("Cannot create edit controller: attempt failed");
		}
      else
         wxLogMessage("Cannot create edit controller: failed read controller Class ID from the effect component");
   }
   else
      mEditController = editController;
}


PluginPath VST3Effect::GetPath()
{
   return VST3Utils::MakePluginPathString( { mModule->getPath() }, mEffectClassInfo.ID().toString());
}

ComponentInterfaceSymbol VST3Effect::GetSymbol()
{
   return wxString { mEffectClassInfo.name() };
}

VendorSymbol VST3Effect::GetVendor()
{
   return wxString { mEffectClassInfo.vendor() };
}

wxString VST3Effect::GetVersion()
{
   return mEffectClassInfo.version();
}

TranslatableString VST3Effect::GetDescription()
{
   //i18n-hint VST3 effect description string
   return XO("SubCategories: %s").Format( mEffectClassInfo.subCategoriesString() );
}

EffectType VST3Effect::GetType()
{
   using namespace Steinberg::Vst::PlugType;
   if(mEffectClassInfo.subCategoriesString() == kFxGenerator)
      return EffectTypeGenerate;
   const auto& cats = mEffectClassInfo.subCategories();
   
   if(std::find(cats.begin(), cats.end(), kFx) != cats.end())
      return EffectTypeProcess;

   return EffectTypeNone;
}

EffectFamilySymbol VST3Effect::GetFamily()
{
   return VST3Effect::GetFamilySymbol();
}

bool VST3Effect::IsInteractive()
{
   return mEditController != nullptr;
}

bool VST3Effect::IsDefault()
{
   return false;
}

bool VST3Effect::SupportsRealtime()
{
   return true;
}

bool VST3Effect::SupportsAutomation()
{
   if(mEditController == nullptr)
      return false;

   using namespace Steinberg;
   
   for(int i = 0, count = mEditController->getParameterCount(); i < count; ++i)
   {
      Vst::ParameterInfo parameterInfo { };
      if(mEditController->getParameterInfo(i, parameterInfo) == kResultOk)
      {
         if(parameterInfo.flags & Vst::ParameterInfo::kCanAutomate)
            return true;
      }
   }

   return false;
}

bool VST3Effect::GetAutomationParameters(CommandParameters& parms)
{
   return false;
}

bool VST3Effect::SetAutomationParameters(CommandParameters& parms)
{
   return false;
}

bool VST3Effect::LoadUserPreset(const RegistryPath& name)
{
   return false;
}

bool VST3Effect::SaveUserPreset(const RegistryPath& name)
{
   return false;
}

RegistryPaths VST3Effect::GetFactoryPresets()
{
   return { };
}

bool VST3Effect::LoadFactoryPreset(int id)
{
   return false;
}

bool VST3Effect::LoadFactoryDefaults()
{
   return false;
}

unsigned VST3Effect::GetAudioInCount()
{
   return 0;
}

unsigned VST3Effect::GetAudioOutCount()
{
   return 0;
}

int VST3Effect::GetMidiInCount()
{
   //Dummy
   return 0;
}

int VST3Effect::GetMidiOutCount()
{
   //Dummy
   return 0;
}

void VST3Effect::SetSampleRate(double rate)
{
   mSetup.sampleRate = rate;
}

size_t VST3Effect::SetBlockSize(size_t maxBlockSize)
{
   mSetup.maxSamplesPerBlock = 
      static_cast<Steinberg::int32>(std::min(maxBlockSize, mUserBlockSize));
   return mSetup.maxSamplesPerBlock;
}

size_t VST3Effect::GetBlockSize() const
{
   return mSetup.maxSamplesPerBlock;
}

sampleCount VST3Effect::GetLatency()
{
   if(mUseLatency)
   {
      if(!mRealtimeGroupProcessors.empty())
         return mRealtimeGroupProcessors[0]->GetLatency();
      auto delay = mInitialDelay;
      mInitialDelay = 0u;
      return delay;
   }
   return { 0u };
}

size_t VST3Effect::GetTailSize()
{
   //Not supported, note that tail size in samples can
   //have different values in realtime processors
   return 0;
}

bool VST3Effect::ProcessInitialize(sampleCount, ChannelNames)
{
   return false;
}

bool VST3Effect::ProcessFinalize()
{
   return false;
}

size_t VST3Effect::ProcessBlock(const float* const* inBlock, float* const* outBlock, size_t blockLen)
{
   return 0;
}

bool VST3Effect::RealtimeInitialize()
{
   return false;
}

bool VST3Effect::RealtimeAddProcessor(unsigned numChannels, float sampleRate)
{
   return false;
}

bool VST3Effect::RealtimeFinalize() noexcept
{
   return false;
}

bool VST3Effect::RealtimeSuspend()
{
   return false;
}

bool VST3Effect::RealtimeResume() noexcept
{
   return false;
}

bool VST3Effect::RealtimeProcessStart()
{
   return false;
}

size_t VST3Effect::RealtimeProcess(int group, const float* const* inBuf, float* const* outBuf, size_t numSamples)
{
   return 0;
}

bool VST3Effect::RealtimeProcessEnd() noexcept
{
   return false;
}

int VST3Effect::ShowClientInterface(wxWindow& parent, wxDialog& dialog, bool forceModal)
{
   return 0;
}

bool VST3Effect::SetHost(EffectHostInterface* host)
{
   return false;
}

bool VST3Effect::IsGraphicalUI()
{
   return false;
}

bool VST3Effect::PopulateUI(ShuttleGui& S)
{
   return false;
}

bool VST3Effect::ValidateUI()
{
   return false;
}

bool VST3Effect::HideUI()
{
   return true;
}

bool VST3Effect::CloseUI()
{
   return false;
}

bool VST3Effect::CanExportPresets()
{
   return false;
}

void VST3Effect::ExportPresets()
{
   
}

void VST3Effect::ImportPresets()
{
   
}

bool VST3Effect::HasOptions()
{
   return false;
}

void VST3Effect::ShowOptions()
{

}
