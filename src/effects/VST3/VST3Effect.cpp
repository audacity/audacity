/**********************************************************************

  Audacity: A Digital Audio Editor

  @file VST3Effect.cpp

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#include "VST3Effect.h"

#include "AudacityException.h"

#include "Base64.h"

#include "BasicUI.h"
#include "widgets/wxWidgetsWindowPlacement.h"

#include <stdexcept>
#include <wx/log.h>
#include <wx/stdpaths.h>
#include <wx/regex.h>
#include <pluginterfaces/vst/ivstaudioprocessor.h>
#include <pluginterfaces/vst/ivsteditcontroller.h>
#include <pluginterfaces/vst/ivstprocesscontext.h>
#include <public.sdk/source/vst/hosting/hostclasses.h>
#include <public.sdk/source/vst/vstpresetfile.h>

#include "internal/ComponentHandler.h"
#include "internal/ParameterChanges.h"
#include "internal/PlugFrame.h"
#include "internal/ConnectionProxy.h"

#include "widgets/NumericTextCtrl.h"

#include "SelectFile.h"

#include "ShuttleGui.h"

#include "VST3Utils.h"
#include "VST3ParametersWindow.h"
#include "VST3OptionsDialog.h"

#ifdef __WXMSW__
#include <shlobj.h>
#elif __WXGTK__
#include "internal/x11/SocketWindow.h"
#endif

#include "ConfigInterface.h"
#include "VST3ParametersWindow.h"

namespace {

   // define some shared registry keys
   constexpr auto processorStateKey  = wxT("ProcessorState");
   constexpr auto controllerStateKey = wxT("ControllerState");

Steinberg::Vst::IHostApplication& LocalContext()
{
   static Steinberg::Vst::HostApplication localContext;
   return localContext;
}

class PresetsBufferStream : public Steinberg::Vst::BufferStream
{
public:

   static Steinberg::IPtr<PresetsBufferStream> fromString(const wxString& str)
   {
      Steinberg::Buffer buffer(str.size() / 4 * 3);
      auto len = Base64::Decode(str, buffer);
      wxASSERT(len <= buffer.getSize());
      buffer.setSize(len);

      auto result = owned(safenew PresetsBufferStream);
      result->mBuffer.take(buffer);
      return result;
   }

   wxString toString() const
   {
      return Base64::Encode(mBuffer, mBuffer.getSize());
   }

};

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

wxString GetFactoryPresetsBasePath()
{
#ifdef __WXMSW__
   PWSTR commonFolderPath { nullptr };
   auto cleanup = finally([&](){ CoTaskMemFree(commonFolderPath); });
   if(SHGetKnownFolderPath(FOLDERID_ProgramData, KF_FLAG_DEFAULT , NULL, &commonFolderPath) == S_OK)
      return wxString(commonFolderPath) + "\\VST3 Presets\\";
   return {};
#elif __WXMAC__
   return wxString("Library/Audio/Presets/");
#elif __WXGTK__
   return wxString("/usr/local/share/vst3/presets/");
#endif
}

wxString GetPresetsPath(const wxString& basePath, const VST3::Hosting::ClassInfo& effectClassInfo)
{
   wxRegEx fixName(R"([\\*?/:<>|])");
   wxString companyName = wxString (effectClassInfo.vendor()).Trim();
   wxString pluginName = wxString (effectClassInfo.name()).Trim();

   fixName.ReplaceAll( &companyName, { "_" });
   fixName.ReplaceAll( &pluginName, { "_" });

   wxFileName result;
   result.SetPath(basePath);
   result.AppendDir(companyName);
   result.AppendDir(pluginName);
   auto path = result.GetPath();

   return path;
}

wxString GetFactoryPresetsPath(const VST3::Hosting::ClassInfo& effectClassInfo)
{
   return GetPresetsPath(
      GetFactoryPresetsBasePath(),
      effectClassInfo
   );
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

   if(mEditController)
   {
      mComponentHandler = owned(safenew internal::ComponentHandler);
      mEditController->setComponentHandler(mComponentHandler);

      auto componentConnectionPoint = FUnknownPtr<Vst::IConnectionPoint>{ mEffectComponent };
      auto controllerConnectionPoint = FUnknownPtr<Vst::IConnectionPoint>{ mEditController };

      if (componentConnectionPoint && controllerConnectionPoint)
      {
         mComponentConnectionProxy = owned(safenew internal::ConnectionProxy(componentConnectionPoint));
         mControllerConnectionProxy = owned(safenew internal::ConnectionProxy(controllerConnectionPoint));

         mComponentConnectionProxy->connect(controllerConnectionPoint);
         mControllerConnectionProxy->connect(componentConnectionPoint);
      }
   }
}


PluginPath VST3Effect::GetPath() const
{
   return VST3Utils::MakePluginPathString( { mModule->getPath() }, mEffectClassInfo.ID().toString());
}

ComponentInterfaceSymbol VST3Effect::GetSymbol() const
{
   return wxString { mEffectClassInfo.name() };
}

VendorSymbol VST3Effect::GetVendor() const
{
   return wxString { mEffectClassInfo.vendor() };
}

wxString VST3Effect::GetVersion() const
{
   return mEffectClassInfo.version();
}

TranslatableString VST3Effect::GetDescription() const
{
   //i18n-hint VST3 effect description string
   return XO("SubCategories: %s").Format( mEffectClassInfo.subCategoriesString() );
}

EffectType VST3Effect::GetType() const
{
   using namespace Steinberg::Vst::PlugType;
   if(mEffectClassInfo.subCategoriesString() == kFxGenerator)
      return EffectTypeGenerate;
   const auto& cats = mEffectClassInfo.subCategories();
   
   if(std::find(cats.begin(), cats.end(), kFx) != cats.end())
      return EffectTypeProcess;

   return EffectTypeNone;
}

EffectFamilySymbol VST3Effect::GetFamily() const
{
   return VST3Effect::GetFamilySymbol();
}

bool VST3Effect::IsInteractive() const
{
   return mEditController != nullptr;
}

bool VST3Effect::IsDefault() const
{
   return false;
}

auto VST3Effect::RealtimeSupport() const -> RealtimeSince
{
   // TODO reenable after achieving statelessness
   // Also, as with old VST, perhaps only for plug-ins known not to be
   // just generators
   return RealtimeSince::Never;
//   return RealtimeSince::Always;
}

bool VST3Effect::SupportsAutomation() const
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

bool VST3Effect::SaveSettings(
   const EffectSettings &, CommandParameters & parms) const
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
         {
            parms.Write(
               VST3Utils::MakeAutomationParameterKey(parameterInfo),
               mEditController->getParamNormalized(parameterInfo.id)
            );
         }
      }
   }

   return true;
}

bool VST3Effect::LoadSettings(
   const CommandParameters & parms, EffectSettings &settings) const
{
   using namespace Steinberg;

   if(mComponentHandler == nullptr)
      return false;
   
   long index { };
   wxString key;
   if(parms.GetFirstEntry(key, index))
   {
      do
      {
         Steinberg::Vst::ParamID id;
         Vst::ParamValue value;
         if(VST3Utils::ParseAutomationParameterKey(key, id) && parms.Read(key, &value))
         {
            if(mComponentHandler->beginEdit(id) == kResultOk)
            {
               auto cleanup = finally([&]{
                  mComponentHandler->endEdit(id);
               });
               mComponentHandler->performEdit(id, value);
            }
            mEditController->setParamNormalized(id, value);
         }
      } while(parms.GetNextEntry(key, index));
   }

   if(mPlainUI != nullptr)
      mPlainUI->ReloadParameters();

   return true;
}

bool VST3Effect::LoadUserPreset(
   const RegistryPath& name, EffectSettings &settings) const
{
   using namespace Steinberg;

   if(!mEditController)
      return false;   

   if(!PluginSettings::HasConfigValue(*this, PluginSettings::Private, name, processorStateKey))
      return false;

   wxString processorStateStr;
   if(!GetConfig(*this, PluginSettings::Private, name, processorStateKey, processorStateStr, wxEmptyString))
      return false;
   auto processorState = PresetsBufferStream::fromString(processorStateStr);
   if(mEffectComponent->setState(processorState) != kResultOk)
      return false;

   if(PluginSettings::HasConfigValue(*this, PluginSettings::Private, name, controllerStateKey))
   {
      wxString controllerStateStr;
      if(!GetConfig(*this, PluginSettings::Private, name, controllerStateKey, controllerStateStr, wxEmptyString))
         return false;
      auto controllerState = PresetsBufferStream::fromString(controllerStateStr);

      if(mEditController->setComponentState(processorState) != kResultOk ||
         mEditController->setState(controllerState) != kResultOk)
         return false;
   }

   SyncParameters(settings);

   if(mPlainUI != nullptr)
      mPlainUI->ReloadParameters();

   return true;
}

bool VST3Effect::SaveUserPreset(
   const RegistryPath& name, const EffectSettings &) const
{
   using namespace Steinberg;

   if(!mEditController)
      return false;

   auto processorState = owned(safenew PresetsBufferStream);
   if(mEffectComponent->getState(processorState) != kResultOk)
      return false;

   SetConfig(*this, PluginSettings::Private, name, processorStateKey, processorState->toString());

   auto controllerState = owned(safenew PresetsBufferStream);
   if(mEditController->getState(controllerState) == kResultOk)
      SetConfig(*this, PluginSettings::Private, name, controllerStateKey, controllerState->toString());

   return true;
}

RegistryPaths VST3Effect::GetFactoryPresets() const
{
   if(!mRescanFactoryPresets)
      return mFactoryPresets;

   wxArrayString paths;
   wxDir::GetAllFiles(GetFactoryPresetsPath(mEffectClassInfo), &paths);

   RegistryPaths result;
   for(auto& path : paths)
   {
      wxFileName filename(path);
      result.push_back(filename.GetName());
   }
   mFactoryPresets = std::move(result);
   mRescanFactoryPresets = false;

   return mFactoryPresets;
}

bool VST3Effect::LoadFactoryPreset(int id, EffectSettings &) const
{
   if(id >= 0 && id < mFactoryPresets.size())
   {
      auto filename = wxFileName(GetFactoryPresetsPath(mEffectClassInfo), mFactoryPresets[id] + ".vstpreset");
      // To do: externalize state so const_cast isn't needed
      return const_cast<VST3Effect*>(this)->LoadPreset(filename.GetFullPath());
   }
   return true;
}

bool VST3Effect::LoadFactoryDefaults(EffectSettings &) const
{
   using namespace Steinberg;
   if(mComponentHandler == nullptr)
      return false;

   for(int i = 0, count = mEditController->getParameterCount(); i < count; ++i)
   {
      Vst::ParameterInfo parameterInfo { };
      if(mEditController->getParameterInfo(i, parameterInfo) == kResultOk)
      {
         if(parameterInfo.flags & Vst::ParameterInfo::kIsReadOnly)
            continue;

         if(mComponentHandler->beginEdit(parameterInfo.id) == kResultOk)
         {
            auto cleanup = finally([&]{
               mComponentHandler->endEdit(parameterInfo.id);
            });
            mComponentHandler->performEdit(parameterInfo.id, parameterInfo.defaultNormalizedValue);
         }
         mEditController->setParamNormalized(parameterInfo.id, parameterInfo.defaultNormalizedValue);
      }
   }
   
   if(mPlainUI != nullptr)
      mPlainUI->ReloadParameters();

   return true;
}

namespace
{
   unsigned CountChannels(Steinberg::Vst::IComponent* component,
      const Steinberg::Vst::MediaTypes mediaType, 
      const Steinberg::Vst::BusDirection busDirection,
      const Steinberg::Vst::BusType busType)
   {
      using namespace Steinberg;

      unsigned channelsCount{0};

      const auto busCount = component->getBusCount(mediaType, busDirection);
      for(auto i = 0; i < busCount; ++i)
      {
         Vst::BusInfo busInfo;
         if(component->getBusInfo(mediaType, busDirection, i, busInfo) == kResultOk)
         {
            if(busInfo.busType == busType)
               channelsCount += busInfo.channelCount;
         }
      }
      return channelsCount;
   }
}

unsigned VST3Effect::GetAudioInCount() const
{
   return CountChannels(
      mEffectComponent,
      Steinberg::Vst::kAudio,
      Steinberg::Vst::kInput,
      Steinberg::Vst::kMain);
}

unsigned VST3Effect::GetAudioOutCount() const
{
   return CountChannels(
      mEffectComponent,
      Steinberg::Vst::kAudio,
      Steinberg::Vst::kOutput,
      Steinberg::Vst::kMain);
}

int VST3Effect::GetMidiInCount() const
{
   //Dummy
   return 0;
}

int VST3Effect::GetMidiOutCount() const
{
   //Dummy
   return 0;
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

bool VST3Effect::ProcessInitialize(
   EffectSettings &settings, double sampleRate, sampleCount, ChannelNames)
{
   mSetup.sampleRate = sampleRate;
   using namespace Steinberg;

   if(mAudioProcessor->canProcessSampleSize(mSetup.symbolicSampleSize) ==kResultTrue &&
      mAudioProcessor->setupProcessing(mSetup) == kResultOk)
   {
      if(mEffectComponent->setActive(true) == kResultOk)
      {
         SyncParameters(settings);//will do nothing for realtime effect
         mAudioProcessor->setProcessing(true);
         mInitialDelay = static_cast<decltype(mInitialDelay)>(mAudioProcessor->getLatencySamples());
         return true;
      }
   }
   return false;
}

bool VST3Effect::ProcessFinalize()
{
   using namespace Steinberg;

   mAudioProcessor->setProcessing(false);
   return mEffectComponent->setActive(false) == Steinberg::kResultOk;
}

namespace
{
   size_t VST3ProcessBlock(
      Steinberg::Vst::IComponent* effect,
      const Steinberg::Vst::ProcessSetup& setup,
      const float* const* inBlock,
      float* const* outBlock,
      size_t blockLen,
      Steinberg::Vst::IParameterChanges* inputParameterChanges)
   {
      using namespace Steinberg;
      if(auto audioProcessor = FUnknownPtr<Vst::IAudioProcessor>(effect))
      {
         Vst::ProcessData data;
         data.processMode = setup.processMode;
         data.symbolicSampleSize = setup.symbolicSampleSize;
         data.inputParameterChanges = inputParameterChanges;

         static_assert(std::numeric_limits<decltype(blockLen)>::max()
            >= std::numeric_limits<decltype(data.numSamples)>::max());

         data.numSamples = static_cast<decltype(data.numSamples)>(std::min(
            blockLen, 
            static_cast<decltype(blockLen)>(setup.maxSamplesPerBlock)
         ));

         data.numInputs = effect->getBusCount(Vst::kAudio, Vst::kInput);
         data.numOutputs = effect->getBusCount(Vst::kAudio, Vst::kOutput);

         if(data.numInputs > 0)
         {
            int inputBlocksOffset {0};

            data.inputs = static_cast<Vst::AudioBusBuffers*>(
               alloca(sizeof(Vst::AudioBusBuffers) * data.numInputs));

            for(int busIndex = 0; busIndex < data.numInputs; ++busIndex)
            {
               Vst::BusInfo busInfo { };
               if(effect->getBusInfo(Vst::kAudio, Vst::kInput, busIndex, busInfo) != kResultOk)
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
               if(effect->getBusInfo(Vst::kAudio, Vst::kOutput, busIndex, busInfo) != kResultOk)
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
      
         const auto processResult = audioProcessor->process(data);
      
         return processResult == kResultOk ?
            data.numSamples : 0;
      }
      return 0;
   }
}

size_t VST3Effect::ProcessBlock(EffectSettings &,
   const float* const* inBlock, float* const* outBlock, size_t blockLen)
{
   internal::ComponentHandler::PendingChangesPtr pendingChanges { nullptr };
   if(mComponentHandler)
      pendingChanges = mComponentHandler->getPendingChanges();
   return VST3ProcessBlock(mEffectComponent.get(), mSetup, inBlock, outBlock, blockLen, pendingChanges.get());
}

bool VST3Effect::RealtimeInitialize(EffectSettings &settings, double sampleRate)
{
   mSetup.sampleRate = sampleRate;
   //reload current parameters form the editor into parameter queues
   SyncParameters(settings);
   return true;
}

bool VST3Effect::RealtimeAddProcessor(
   EffectSettings &settings, unsigned numChannels, float sampleRate)
{
   using namespace Steinberg;

   try
   {
      auto effect = std::make_unique<VST3Effect>(*this);
      effect->mSetup.processMode = Vst::kRealtime;
      effect->mSetup.sampleRate = sampleRate;
      if(!effect->ProcessInitialize(settings, sampleRate, {0}, nullptr))
         throw std::runtime_error { "VST3 realtime initialization failed" };

      mRealtimeGroupProcessors.push_back(std::move(effect));
      return true;
   }
   catch(std::exception& e)
   {
      //make_unique<> isn't likely to fail here since this effect instance
      //somehow succeeded
      wxLogError("Failed to add realtime processor: %s", e.what());
   }
   return false;
}

bool VST3Effect::RealtimeFinalize(EffectSettings &) noexcept
{
   return GuardedCall<bool>([this]()
   {
      for(auto& processor : mRealtimeGroupProcessors)
         processor->ProcessFinalize();

      mRealtimeGroupProcessors.clear();
      mPendingChanges.reset();
      
      return true;
   });
}

bool VST3Effect::RealtimeSuspend()
{
   for(auto& effect : mRealtimeGroupProcessors)
      effect->mAudioProcessor->setProcessing(false);
   return true;
}

bool VST3Effect::RealtimeResume()
{
   for(auto& effect : mRealtimeGroupProcessors)
      effect->mAudioProcessor->setProcessing(true);
   return true;
}

bool VST3Effect::RealtimeProcessStart(EffectSettings &)
{
   assert(mPendingChanges == nullptr);

   if(mComponentHandler != nullptr)
      //Same parameter changes are used among all of the realtime processors
      mPendingChanges = mComponentHandler->getPendingChanges();
   return true;
}

size_t VST3Effect::RealtimeProcess(size_t group, EffectSettings &,
   const float* const* inBuf, float* const* outBuf, size_t numSamples)
{
   if (group >= mRealtimeGroupProcessors.size())
      return 0;
   auto& effect = mRealtimeGroupProcessors[group];
   return VST3ProcessBlock(effect->mEffectComponent.get(), effect->mSetup, inBuf, outBuf, numSamples, mPendingChanges.get());
}

bool VST3Effect::RealtimeProcessEnd(EffectSettings &) noexcept
{
   return GuardedCall<bool>([this]()
   {
      mPendingChanges.reset();
      return true;
   });
}

int VST3Effect::ShowClientInterface(wxWindow& parent, wxDialog& dialog, bool forceModal)
{
   if(!IsGraphicalUI())
   {
      //Restrict resize of the "plain" dialog
      dialog.SetMaxSize(dialog.GetSize());
      dialog.SetMinSize(dialog.GetSize());
   }
   if(forceModal)
      return dialog.ShowModal();

   dialog.Show();
   return 0;
}

bool VST3Effect::InitializePlugin()
{
   return true;
}
   
std::shared_ptr<EffectInstance> VST3Effect::MakeInstance() const
{
   return const_cast<VST3Effect*>(this)->DoMakeInstance();
}
   
std::shared_ptr<EffectInstance> VST3Effect::DoMakeInstance()
{
   ReloadUserOptions();
   return std::make_shared<Instance>(*this);
}

bool VST3Effect::IsGraphicalUI()
{
   return mPlugView != nullptr;
}

std::unique_ptr<EffectUIValidator> VST3Effect::PopulateUI(ShuttleGui& S,
   EffectInstance &, EffectSettingsAccess &access)
{
   using namespace Steinberg;

   mParent = S.GetParent();

   if(mComponentHandler != nullptr)
   {
      // PRL:  Is this sync really needed?
      access.ModifySettings([&](EffectSettings &settings){
         SyncParameters(settings);
      });

      auto parent = S.GetParent();
      if(GetType() == EffectTypeGenerate)
      {
         auto vSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);
         auto controlsRoot = safenew wxWindow(parent, wxID_ANY);
         if(!LoadVSTUI(controlsRoot))
            mPlainUI = VST3ParametersWindow::Setup(*controlsRoot, *mEditController, *mComponentHandler);
         vSizer->Add(controlsRoot);

         auto &extra = access.Get().extra;
         mDuration = safenew NumericTextCtrl(
               parent, wxID_ANY,
               NumericConverter::TIME,
               extra.GetDurationFormat(),
               extra.GetDuration(),
               mSetup.sampleRate,
               NumericTextCtrl::Options{}
                  .AutoPos(true)
            );
         mDuration->SetName( XO("Duration") );

         auto hSizer = std::make_unique<wxBoxSizer>(wxHORIZONTAL);
         hSizer->Add(safenew wxStaticText(parent, wxID_ANY, _("Duration:")));
         hSizer->AddSpacer(5);
         hSizer->Add(mDuration);
         vSizer->AddSpacer(10);
         vSizer->Add(hSizer.release());

         parent->SetMinSize(vSizer->CalcMin());
         parent->SetSizer(vSizer.release());
      }
      else if(!LoadVSTUI(parent))
      {
         mPlainUI = VST3ParametersWindow::Setup(
            *parent,
            *mEditController,
            *mComponentHandler
         );
      }

      return std::make_unique<DefaultEffectUIValidator>(*this, access);
   }
   return nullptr;
}

bool VST3Effect::ValidateUI(EffectSettings &settings)
{
   if (mDuration != nullptr)
      settings.extra.SetDuration(mDuration->GetValue());
   return true;
}

bool VST3Effect::CloseUI()
{
   mPlainUI = nullptr;
   mParent = nullptr;
   if(mPlugView)
   {
      mPlugView->setFrame(nullptr);
      mPlugView->removed();
      mPlugView = nullptr;
      return true;
   }
   return false;
}

bool VST3Effect::CanExportPresets()
{
   return true;
}

void VST3Effect::ExportPresets(const EffectSettings &) const
{
   using namespace Steinberg;

   auto path = SelectFile(FileNames::Operation::Presets,
      XO("Save VST3 Preset As:"),
      wxEmptyString,
      wxEmptyString,
      wxT(".vstpreset"),
      {
        { XO("VST3 preset file"), { wxT("vstpreset") }, true }
      },
      wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER,
      NULL);

   if (path.empty())
      return;

   auto dialogPlacement = wxWidgetsWindowPlacement { mParent };

   auto fileStream = owned(Vst::FileStream::open(path.c_str(), "wb"));
   if(!fileStream)
   {
      BasicUI::ShowMessageBox(
         //i18n-hint: VST3 preset export error
         XO("Cannot open file"),
         BasicUI::MessageBoxOptions()
            .Caption(XO("Error"))
            .Parent(&dialogPlacement)
      );
      return;
   }

   if(!Vst::PresetFile::savePreset(
      fileStream,
      FUID::fromTUID (mEffectClassInfo.ID().data()),
      mEffectComponent.get(),
      mEditController.get()))
   {
      BasicUI::ShowMessageBox(
         XO("Failed to save VST3 preset to file"),
         BasicUI::MessageBoxOptions()
            .Caption(XO("Error"))
            .Parent(&dialogPlacement)
      );
      return;
   }
}

void VST3Effect::ImportPresets(EffectSettings &)
{
   using namespace Steinberg;

   auto path = SelectFile(FileNames::Operation::Presets,
      XO("Load VST3 preset:"),
      wxEmptyString,
      wxEmptyString,
      wxT(".vstpreset"),
      {
         { XO("VST3 preset file"), { wxT("vstpreset") }, true }
      },
      wxFD_OPEN | wxRESIZE_BORDER,
      nullptr
   );
   if(path.empty())
      return;

   LoadPreset(path);
}

bool VST3Effect::HasOptions()
{
   return true;
}

void VST3Effect::ShowOptions()
{
   VST3OptionsDialog dlg(mParent, *this);
   if (dlg.ShowModal())
   {
      ReloadUserOptions();
   }
}

void VST3Effect::OnEffectWindowResize(wxSizeEvent& evt)
{
   using namespace Steinberg;

   if(!mPlugView)
      return;

   const auto window = static_cast<wxWindow*>(evt.GetEventObject());
   const auto windowSize = evt.GetSize();

   {
      //Workaround to prevent dialog window resize when
      //plugin window reaches its maximum size
      auto root = wxGetTopLevelParent(window);
      wxSize maxRootSize = root->GetMaxSize();

      //remember the current dialog size as its new maximum size
      if(window->GetMaxWidth() != -1 && windowSize.GetWidth() >= window->GetMaxWidth())
         maxRootSize.SetWidth(root->GetSize().GetWidth());
      if(window->GetMaxHeight() != -1 && windowSize.GetHeight() >= window->GetMaxHeight())
         maxRootSize.SetHeight(root->GetSize().GetHeight());
      root->SetMaxSize(maxRootSize);
   }
   
   ViewRect plugViewSize { 0, 0, windowSize.x, windowSize.y };
   mPlugView->checkSizeConstraint(&plugViewSize);
   mPlugView->onSize(&plugViewSize);
}

bool VST3Effect::LoadVSTUI(wxWindow* parent)
{
   using namespace Steinberg;
   if(mEditController == nullptr)
      return false;

   bool useGUI { true };
   GetConfig(*this, PluginSettings::Shared, wxT("Options"),
            wxT("UseGUI"),
            useGUI,
            useGUI);
   if(!useGUI)
      return false;

   if(const auto view = owned (mEditController->createView (Vst::ViewType::kEditor))) 
   {  
      parent->Bind(wxEVT_SIZE, &VST3Effect::OnEffectWindowResize, this);

      ViewRect defaultSize;
      if(view->getSize(&defaultSize) == kResultOk)
      {
         if(view->canResize() == Steinberg::kResultTrue)
         {
            ViewRect minSize {0, 0, parent->GetMinWidth(), parent->GetMinHeight()};
            ViewRect maxSize {0, 0, 10000, 10000};
            if(view->checkSizeConstraint(&minSize) != kResultOk)
               minSize = defaultSize;

            if(view->checkSizeConstraint(&maxSize) != kResultOk)
               maxSize = defaultSize;

            //No need to accommodate the off-by-one error with Steinberg::ViewRect
            //as we do it with wxRect

            if(defaultSize.getWidth() < minSize.getWidth())
               defaultSize.right = defaultSize.left + minSize.getWidth();
            if(defaultSize.getWidth() > maxSize.getWidth())
               defaultSize.right = defaultSize.left + maxSize.getWidth();

            if(defaultSize.getHeight() < minSize.getHeight())
               defaultSize.bottom = defaultSize.top + minSize.getHeight();
            if(defaultSize.getHeight() > maxSize.getHeight())
               defaultSize.bottom = defaultSize.top + maxSize.getHeight();

            parent->SetMinSize({minSize.getWidth(), minSize.getHeight()});
            parent->SetMaxSize({maxSize.getWidth(), maxSize.getHeight()});
         }
         else
         {
            parent->SetMinSize({defaultSize.getWidth(), defaultSize.getHeight()});
            parent->SetMaxSize(parent->GetMinSize());
         }

         parent->SetSize({defaultSize.getWidth(), defaultSize.getHeight()});
      }

#if __WXGTK__
      mPlugView = view;
      safenew internal::x11::SocketWindow(parent, wxID_ANY, view);
      return true;
#else

      static const auto platformType =
#  if __WXMAC__
         kPlatformTypeNSView;
#  elif __WXMSW__
         kPlatformTypeHWND;
#  else
#     error "Platform not supported"
#  endif
      auto plugFrame = owned(safenew internal::PlugFrame { parent });
      view->setFrame(plugFrame);
      if(view->attached(parent->GetHandle(), platformType) != kResultOk)
         return false;

      mPlugView = view;

      return true;
#endif

      
   }
   return false;
}

// Transfers data from the dialog
void VST3Effect::SyncParameters(EffectSettings &) const
{
   using namespace Steinberg;

   if(mComponentHandler != nullptr)
   {
      for(int i = 0, count = mEditController->getParameterCount(); i < count; ++i)
      {
         Vst::ParameterInfo parameterInfo { };
         if(mEditController->getParameterInfo(i, parameterInfo) == kResultOk)
         {
            if(parameterInfo.flags & Vst::ParameterInfo::kIsReadOnly)
               continue;

            if(mComponentHandler->beginEdit(parameterInfo.id) == kResultOk)
            {
               auto cleanup = finally([&]{ mComponentHandler->endEdit(parameterInfo.id); });
               mComponentHandler->performEdit(parameterInfo.id, mEditController->getParamNormalized(parameterInfo.id));
            }
         }
      }
   }
}

bool VST3Effect::LoadPreset(const wxString& path)
{
   using namespace Steinberg;

   auto dialogPlacement = wxWidgetsWindowPlacement { mParent };

   auto fileStream = owned(Vst::FileStream::open(path.c_str(), "rb"));
   if(!fileStream)
   {
      BasicUI::ShowMessageBox(
         XO("Cannot open VST3 preset file %s").Format(path),
         BasicUI::MessageBoxOptions()
            .Caption(XO("Error"))
            .Parent(&dialogPlacement)
      );
      return false;
   }

   if(!Vst::PresetFile::loadPreset(
      fileStream,
      FUID::fromTUID(mEffectClassInfo.ID().data()),
      mEffectComponent.get(),
      mEditController.get()))
   {
      BasicUI::ShowMessageBox(
         XO("Unable to apply VST3 preset file %s").Format(path),
         BasicUI::MessageBoxOptions()
            .Caption(XO("Error"))
            .Parent(&dialogPlacement)
      );
      return false;
   }

   if(mPlainUI != nullptr)
      mPlainUI->ReloadParameters();

   return true;
}

void VST3Effect::ReloadUserOptions()
{
   // Reinitialize configuration settings
   int userBlockSize;
   GetConfig(*this, PluginSettings::Shared, wxT("Options"),
      wxT("BufferSize"), userBlockSize, 8192);
   mUserBlockSize = std::max( 1, userBlockSize );
   GetConfig(*this, PluginSettings::Shared, wxT("Options"),
      wxT("UseLatency"), mUseLatency, true);

   SetBlockSize(mUserBlockSize);
}
