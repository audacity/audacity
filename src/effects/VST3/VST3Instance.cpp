#include "VST3Instance.h"

#include "VST3Wrapper.h"

#include <public.sdk/source/vst/utility/uid.h>

#include "AudacityException.h"
#include "ConfigInterface.h"
#include "SelectFile.h"
#include "VST3ParametersWindow.h"
#include "widgets/NumericTextCtrl.h"
#include "ShuttleGui.h"
#include "VST3OptionsDialog.h"
#include "internal/PlugFrame.h"
#include "VST3Utils.h"
#include "widgets/wxWidgetsWindowPlacement.h"

#include "memorystream.h"


namespace
{
   // define some shared registry keys
   constexpr auto processorStateKey  = wxT("ProcessorState");
   constexpr auto controllerStateKey = wxT("ControllerState");

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

VST3Instance::VST3Instance(const PerTrackEffect& effect, VST3::Hosting::Module& module, VST3::UID effectUID)
   : Instance(effect), mEffectUID(effectUID)
{
   ReloadUserOptions();
   mWrapper = std::make_unique<VST3Wrapper>(module, mEffectUID);
}

VST3Instance::~VST3Instance() = default;

size_t VST3Instance::GetTailSize() const
{
   return Instance::GetTailSize();
}

bool VST3Instance::Init()
{
   return Instance::Init();
}

bool VST3Instance::RealtimeAddProcessor(EffectSettings& settings, unsigned, float sampleRate)
{
   return true;
}

bool VST3Instance::RealtimeFinalize(EffectSettings& settings) noexcept
{
   return true;
}

bool VST3Instance::RealtimeInitialize(EffectSettings& settings, double sampleRate)
{
   if(mWrapper->Initialize(sampleRate, Steinberg::Vst::kRealtime, mProcessingBlockSize))
   {
      mInitialDelay = mWrapper->GetLatencySamples();
      return true;
   }
   return false;
}

size_t VST3Instance::RealtimeProcess(size_t group, EffectSettings& settings, const float* const* inBuf,
   float* const* outBuf, size_t numSamples)
{
   if(group == 0)
      return mWrapper->Process(settings, inBuf, outBuf, numSamples);
   return 0;
}

bool VST3Instance::RealtimeProcessEnd(EffectSettings& settings) noexcept
{
   return true;
}

bool VST3Instance::RealtimeProcessStart(EffectSettings& settings)
{
   return true;
}

bool VST3Instance::RealtimeResume()
{
   mWrapper->ResumeProcessing();
   return true;
}

bool VST3Instance::RealtimeSuspend()
{
   mWrapper->SuspendProcessing();
   return true;
}

sampleCount VST3Instance::GetLatency(const EffectSettings& settings, double sampleRate) const
{
   if(mUseLatency)
      return mInitialDelay;
   return { 0u };
}

bool VST3Instance::ProcessFinalize() noexcept
{
   return GuardedCall<bool>([&]
   {
      mWrapper->Finalize();
      return true;
   });
}

bool VST3Instance::ProcessInitialize(EffectSettings &settings, double sampleRate, ChannelNames chanMap)
{
   if(mWrapper->Initialize(sampleRate, Steinberg::Vst::kRealtime, mProcessingBlockSize))
   {
      mInitialDelay = mWrapper->GetLatencySamples();
      return true;
   }
   return false;;
}

size_t VST3Instance::GetBlockSize() const
{
   return mProcessingBlockSize;
}

size_t VST3Instance::SetBlockSize(size_t maxBlockSize)
{
   mProcessingBlockSize = 
      static_cast<Steinberg::int32>(std::min(maxBlockSize, mUserBlockSize));
   return mProcessingBlockSize;
}

size_t VST3Instance::ProcessBlock(EffectSettings& settings, const float* const* inBlock, float* const* outBlock,
   size_t blockLen)
{
   return mWrapper->Process(settings, inBlock, outBlock, blockLen);
}

void VST3Instance::PopulateUI(ShuttleGui& S, EffectSettingsAccess& access)
{
   mWrapper->mComponentHandler->SetAccess(access.shared_from_this());

   auto parent = S.GetParent();
   if(mProcessor.GetType() == EffectTypeGenerate)
   {
      auto vSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);
      auto controlsRoot = safenew wxWindow(parent, wxID_ANY);
      if(!LoadVSTUI(controlsRoot))
         mPlainUI = VST3ParametersWindow::Setup(*controlsRoot, *mWrapper->mEditController, *mWrapper->mComponentHandler);
      vSizer->Add(controlsRoot);

      auto &extra = access.Get().extra;
      mDuration = safenew NumericTextCtrl(
            parent, wxID_ANY,
            NumericConverter::TIME,
            extra.GetDurationFormat(),
            extra.GetDuration(),
            mWrapper->mSetup.sampleRate,
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
         *mWrapper->mEditController,
         *mWrapper->mComponentHandler
      );
   }
}

void VST3Instance::CloseUI()
{
   using namespace Steinberg;

   mPlainUI = nullptr;
   mParent = nullptr;
   if(mPlugView)
   {
      mPlugView->setFrame(nullptr);
      mPlugView->removed();
      mPlugView = nullptr;
      mPlugFrame = nullptr;
   }
   else if(mWrapper->mComponentHandler)
   {
      //Slave processors don't have a component handler...
      if(auto access = mWrapper->mComponentHandler->GetAccess())
      {
         mWrapper->FlushSettings(access->Get());
         mWrapper->mComponentHandler->SetAccess(nullptr);
      }
   }
}

bool VST3Instance::IsGraphicalUI() const
{
   return mPlugView != nullptr;
}

bool VST3Instance::ValidateUI(EffectSettings& settings)
{
   if (mDuration != nullptr)
      settings.extra.SetDuration(mDuration->GetValue());

   return true;
}

bool VST3Instance::SaveUserPreset(const RegistryPath& name) const
{
   using namespace Steinberg;

   auto processorState = owned(safenew PresetsBufferStream);
   if(mWrapper->mEffectComponent->getState(processorState) != kResultOk)
      return false;

   SetConfig(mProcessor, PluginSettings::Private, name, processorStateKey, processorState->toString());

   auto controllerState = owned(safenew PresetsBufferStream);
   if(mWrapper->mEditController->getState(controllerState) == kResultOk)
      SetConfig(mProcessor, PluginSettings::Private, name, controllerStateKey, controllerState->toString());

   return true;
}

bool VST3Instance::LoadUserPreset(const RegistryPath& name)
{
   using namespace Steinberg;

   if(!PluginSettings::HasConfigValue(mProcessor, PluginSettings::Private, name, processorStateKey))
      return false;

   wxString processorStateStr;
   if(!GetConfig(mProcessor, PluginSettings::Private, name, processorStateKey, processorStateStr, wxEmptyString))
      return false;
   auto processorState = PresetsBufferStream::fromString(processorStateStr);
   if(mWrapper->mEffectComponent->setState(processorState) != kResultOk)
      return false;

   mWrapper->mEditController->setComponentState(processorState);

   if(PluginSettings::HasConfigValue(mProcessor, PluginSettings::Private, name, controllerStateKey))
   {
      wxString controllerStateStr;
      if(!GetConfig(mProcessor, PluginSettings::Private, name, controllerStateKey, controllerStateStr, wxEmptyString))
         return false;
      auto controllerState = PresetsBufferStream::fromString(controllerStateStr);

      return mWrapper->mEditController->setState(controllerState) == kResultOk;
   }

   return true;
}

void VST3Instance::ExportPresets() const
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

   if (!mWrapper->SavePreset(fileStream))
   {
      BasicUI::ShowMessageBox(
         XO("Failed to save VST3 preset to file"),
         BasicUI::MessageBoxOptions()
            .Caption(XO("Error"))
            .Parent(&dialogPlacement)
      );
   }
}

bool VST3Instance::LoadPreset(const wxString& path)
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


   if (!mWrapper->LoadPreset(fileStream))
   {
      BasicUI::ShowMessageBox(
         XO("Unable to apply VST3 preset file %s").Format(path),
         BasicUI::MessageBoxOptions()
            .Caption(XO("Error"))
            .Parent(&dialogPlacement)
      );
      return false;
   }

   return true;
}

void VST3Instance::ShowOptions()
{
   VST3OptionsDialog dlg(mParent, const_cast<PerTrackEffect&>(mProcessor));
   dlg.ShowModal();
   ReloadUserOptions();
}

bool VST3Instance::TransferDataToWindow(const EffectSettings& settings)
{
   mWrapper->FlushSettings(settings);

   if (mPlainUI != nullptr)
      mPlainUI->ReloadParameters();
   else
   {
      SyncComponentStates(*mWrapper->mEffectComponent, *mWrapper->mEditController);
      const auto& vst3settings = VST3Wrapper::GetSettings(settings);
      for(const auto& p : vst3settings.parameterIndexMap)
         mWrapper->mEditController->setParamNormalized(p.first, vst3settings.parameters[p.second]);
   }

   return false;
}

unsigned VST3Instance::GetAudioInCount() const
{
   //setupProcessing should be called first
   return CountChannels(
      mWrapper->mEffectComponent,
      Steinberg::Vst::kAudio,
      Steinberg::Vst::kInput,
      Steinberg::Vst::kMain
   );
}

unsigned VST3Instance::GetAudioOutCount() const
{
   //setupProcessing should be called first
   return CountChannels(
      mWrapper->mEffectComponent,
      Steinberg::Vst::kAudio,
      Steinberg::Vst::kOutput,
      Steinberg::Vst::kMain
   );
}

void VST3Instance::ReloadUserOptions()
{
   // Reinitialize configuration settings
   int userBlockSize;
   GetConfig(mProcessor, PluginSettings::Shared, wxT("Options"),
      wxT("BufferSize"), userBlockSize, 8192);
   mUserBlockSize = std::max( 1, userBlockSize );
   GetConfig(mProcessor, PluginSettings::Shared, wxT("Options"),
      wxT("UseLatency"), mUseLatency, true);

   SetBlockSize(mUserBlockSize);
}

bool VST3Instance::LoadVSTUI(wxWindow* parent)
{
   using namespace Steinberg;
   if(mWrapper->mEditController == nullptr)
      return false;

   bool useGUI { true };
   GetConfig(mProcessor, PluginSettings::Shared, wxT("Options"),
            wxT("UseGUI"),
            useGUI,
            useGUI);
   if(!useGUI)
      return false;

   if(const auto view = owned (mWrapper->mEditController->createView (Vst::ViewType::kEditor))) 
   {  
      parent->Bind(wxEVT_SIZE, &VST3Instance::OnEffectWindowResize, this);

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
      mPlugFrame = plugFrame;

      SyncComponentStates(*mWrapper->mEffectComponent.get(), *mWrapper->mEditController.get());

      return true;
#endif

      
   }
   return false;
}

void VST3Instance::OnEffectWindowResize(wxSizeEvent& evt)
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
