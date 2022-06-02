/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioUnitEffect.cpp

  Dominic Mazzoni
  Leland Lucius

*******************************************************************//**

\class AudioUnitEffect
\brief An Effect class that handles a wide range of effects.  ??Mac only??

*//*******************************************************************/

#if USE_AUDIO_UNITS
#include "AudioUnitEffect.h"
#include "AudioUnitEffectOptionsDialog.h"
#include "AudacityException.h"
#include "AUControl.h"
#include "SampleCount.h"
#include "ConfigInterface.h"

#include <optional>
#include <wx/defs.h>
#include <wx/base64.h>
#include <wx/button.h>
#include <wx/control.h>
#include <wx/crt.h>
#include <wx/dir.h>
#include <wx/ffile.h>

#ifdef __WXMAC__
#include <wx/evtloop.h>
#endif

#include <wx/filename.h>
#include <wx/frame.h>
#include <wx/listctrl.h>
#include <wx/log.h>
#include <wx/sizer.h>
#include <wx/settings.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>

#include "../../SelectFile.h"
#include "../../ShuttleGui.h"
#include "../../widgets/AudacityMessageBox.h"
#include "../../widgets/valnum.h"

//
// When a plug-in's state is saved to the settings file (as a preset),
// it is in binary and gets base64 encoded before storing.
//
// When exporting, save as XML without base64 encoding.
//
// The advantages of XML format is less chance of failures occurring
// when exporting.  But, it can take a bit more space per preset int
// the Audacity settings file.
//
// Using binary for now.  Use kCFPropertyListXMLFormat_v1_0 if XML
// format is desired.
//
#define PRESET_FORMAT kCFPropertyListBinaryFormat_v1_0

// Name of the settings key to use for the above value
#define PRESET_KEY wxT("Data")

// Where the presets are located
#define PRESET_LOCAL_PATH wxT("/Library/Audio/Presets")
#define PRESET_USER_PATH wxT("~/Library/Audio/Presets")

TranslatableString AudioUnitEffect::SaveBlobToConfig(
   const RegistryPath &group, const wxString &path,
   const void *blob, size_t len, bool allowEmpty) const
{
   // Base64 encode the returned binary property list
   auto parms = wxBase64Encode(blob, len);
   if (!allowEmpty && parms.IsEmpty())
      return XO("Failed to encode preset from \"%s\"").Format(path);

   // And write it to the config
   if (!SetConfig(*this, PluginSettings::Private, group, PRESET_KEY, parms))
      return XO("Unable to store preset in config file");
   return {};
}

///////////////////////////////////////////////////////////////////////////////
//
// AudioUnitEffect
//
///////////////////////////////////////////////////////////////////////////////

AudioUnitEffect::AudioUnitEffect(const PluginPath & path,
   const wxString & name, AudioComponent component,
   Parameters *pParameters, AudioUnitEffect *master
)  : AudioUnitWrapper{ component, pParameters }
   , mPath{ path }
   , mName{ name.AfterFirst(wxT(':')).Trim(true).Trim(false) }
   , mVendor{ name.BeforeFirst(wxT(':')).Trim(true).Trim(false) }
   , mMaster{ master }
{
}

AudioUnitEffect::~AudioUnitEffect()
{
}

// ============================================================================
// ComponentInterface implementation
// ============================================================================

PluginPath AudioUnitEffect::GetPath() const
{
   return mPath;
}

ComponentInterfaceSymbol AudioUnitEffect::GetSymbol() const
{
   return mName;
}

VendorSymbol AudioUnitEffect::GetVendor() const
{
   return { mVendor };
}

wxString AudioUnitEffect::GetVersion() const
{
   UInt32 version;

   OSStatus result = AudioComponentGetVersion(mComponent, &version);

   return wxString::Format(wxT("%d.%d.%d"),
                           (version >> 16) & 0xffff,
                           (version >> 8) & 0xff,
                           version & 0xff);
}

TranslatableString AudioUnitEffect::GetDescription() const
{
   /* i18n-hint: Can mean "not available," "not applicable," "no answer" */
   return XO("n/a");
}

// ============================================================================
// EffectDefinitionInterface implementation
// ============================================================================

EffectType AudioUnitEffect::GetType() const
{
   if (mAudioIns == 0 && mAudioOuts == 0)
   {
      return EffectTypeNone;
   }

   if (mAudioIns == 0)
   {
      return EffectTypeGenerate;
   }

   if (mAudioOuts == 0)
   {
      return EffectTypeAnalyze;
   }

   return EffectTypeProcess;
}

EffectFamilySymbol AudioUnitEffect::GetFamily() const
{
   return AUDIOUNITEFFECTS_FAMILY;
}

bool AudioUnitEffect::IsInteractive() const
{
   return mInteractive;
}

bool AudioUnitEffect::IsDefault() const
{
   return false;
}

bool AudioUnitEffect::SupportsRealtime() const
{
   // TODO reenable after achieving statelessness
   return false;

//   return GetType() == EffectTypeProcess;
}

bool AudioUnitEffect::SupportsAutomation() const
{
   bool supports = false;
   ForEachParameter(
   [&supports](const ParameterInfo &pi, AudioUnitParameterID) {
      if (pi.mInfo.flags & kAudioUnitParameterFlag_IsWritable)
         supports = true;
      // Search only until we find one, that's all we need to know
      return !supports;
   });
   return supports;
}

class AudioUnitInstance : public StatefulPerTrackEffect::Instance
   , public AudioUnitWrapper
{
public:
   AudioUnitInstance(StatefulPerTrackEffect &effect,
      AudioComponent component, Parameters &parameters);
   ~AudioUnitInstance() override;

   static void EventListenerCallback(void *inCallbackRefCon,
      void *inObject, const AudioUnitEvent *inEvent,
      UInt64 inEventHostTime, AudioUnitParameterValue inParameterValue);
   void EventListener(const AudioUnitEvent *inEvent,
      AudioUnitParameterValue inParameterValue);
};

AudioUnitInstance::AudioUnitInstance(StatefulPerTrackEffect &effect,
   AudioComponent component, Parameters &parameters
)  : StatefulPerTrackEffect::Instance{ effect }
   , AudioUnitWrapper{ component, &parameters }
{
   CreateAudioUnit();
}

AudioUnitInstance::~AudioUnitInstance()
{
}

bool AudioUnitEffect::InitializeInstance()
{
   if (!CreateAudioUnit())
      return false;

   mSampleRate = 44100;
   GetChannelCounts();
   SetRateAndChannels();

   // Retrieve the desired number of frames per slice
   if (GetFixedSizeProperty(
      kAudioUnitProperty_MaximumFramesPerSlice, mBlockSize))
      // Call failed?  Then supply a default:
      mBlockSize = 512;
   return true;
}

std::shared_ptr<EffectInstance> AudioUnitEffect::MakeInstance() const
{
   return const_cast<AudioUnitEffect*>(this)->DoMakeInstance();
}

std::shared_ptr<EffectInstance> AudioUnitEffect::DoMakeInstance()
{
   if (mMaster)
      // This is a slave
      InitializeInstance();
   return std::make_shared<AudioUnitInstance>(*this, mComponent, mParameters);
}

constexpr auto OptionsKey = L"Options";
constexpr auto UseLatencyKey = L"UseLatency";
constexpr auto UITypeKey = L"UIType";

bool AudioUnitEffect::InitializePlugin()
{
   // To implement the services of EffectPlugin -- such as, a query of the
   // set of effect parameters, so that we can implement MakeSettings -- we
   // also need what is called an AudioComponentInstance, also called an
   // AudioUnit.
   // It's not just for implementing EffectInstance.  AudioUnits is unlike other
   // third party effect families that distinguish the notions of plug-in and
   // instance.

   // When AudioUnitEffect implements its own proper Instance class, this
   // should call CreateAudioUnit() directly and not do the rest of
   // InitializeInstance.
   if (!InitializeInstance())
      return false;

   // Determine interactivity
   mInteractive = (Count(mParameters) > 0);
   if (!mInteractive) {
      // Check for a Cocoa UI
      // This could retrieve a variable-size property, but we only look at
      // the first element.
      AudioUnitCocoaViewInfo cocoaViewInfo;
      mInteractive =
         !GetFixedSizeProperty(kAudioUnitProperty_CocoaUI, cocoaViewInfo);
      if (!mInteractive) {
         // Check for a Carbon UI
         // This could retrieve a variable sized array but we only need the
         // first
         AudioComponentDescription compDesc;
         mInteractive = !GetFixedSizeProperty(
            kAudioUnitProperty_GetUIComponentList, compDesc);
      }
   }
   return true;
}

bool AudioUnitEffect::FullyInitializePlugin()
{
   if (!InitializePlugin())
      return false;

   // Reading these values from the config file can't be done in the PluginHost
   // process but isn't needed only for plugin discovery.

   // Consult preferences
   // Decide mUseLatency, which affects GetLatency(), which is actually used
   // so far only in destructive effect processing
   GetConfig(*this, PluginSettings::Shared, OptionsKey, UseLatencyKey,
      mUseLatency, true);
   // Decide whether to build plain or fancy user interfaces
   GetConfig(*this, PluginSettings::Shared, OptionsKey, UITypeKey,
      mUIType, FullValue.MSGID().GET() /* Config stores un-localized string */);

   return true;
}

class AudioUnitValidator : public EffectUIValidator {
   struct CreateToken{};
public:
   static std::unique_ptr<EffectUIValidator> Create(
      EffectUIClientInterface &effect, ShuttleGui &S,
      const wxString &uiType,
      EffectInstance &instance, EffectSettingsAccess &access);

   AudioUnitValidator(CreateToken,
      EffectUIClientInterface &effect, EffectSettingsAccess &access,
      AudioUnitInstance &instance, AUControl *pControl);

   ~AudioUnitValidator() override;

   bool UpdateUI() override;
   bool ValidateUI() override;

private:
   bool FetchSettingsFromInstance(EffectSettings &settings);
   bool StoreSettingsToInstance(const EffectSettings &settings);

   void Notify();

   using EventListenerPtr =
      AudioUnitCleanup<AUEventListenerRef, AUListenerDispose>;

   static EventListenerPtr MakeListener(AudioUnitInstance &instance);

   // The lifetime guarantee is assumed to be provided by the instance.
   // See contract of PopulateUI
   AudioUnitInstance &mInstance;
   const EventListenerPtr mEventListenerRef;
   AUControl *const mpControl{};
};

AudioUnitValidator::AudioUnitValidator(CreateToken,
   EffectUIClientInterface &effect,
   EffectSettingsAccess &access, AudioUnitInstance &instance,
   AUControl *pControl
)  : EffectUIValidator{ effect, access }
   , mInstance{ instance }
   , mEventListenerRef{ MakeListener(instance) }
   , mpControl{ pControl }
{
   // Make the settings of the instance up to date before using it to
   // build a UI
   StoreSettingsToInstance(mAccess.Get());
}

AudioUnitValidator::~AudioUnitValidator()
{
   if (mpControl)
      mpControl->Close();
}

auto AudioUnitValidator::MakeListener(AudioUnitInstance &instance)
   -> EventListenerPtr
{
   const auto unit = instance.GetAudioUnit();
   EventListenerPtr result;

   // Register a callback with the audio unit
   AUEventListenerRef eventListenerRef{};
   if (AUEventListenerCreate(AudioUnitInstance::EventListenerCallback,
      &instance,
      static_cast<CFRunLoopRef>( const_cast<void*>(
         GetCFRunLoopFromEventLoop(GetCurrentEventLoop()))),
      kCFRunLoopDefaultMode, 0.0, 0.0, &eventListenerRef))
      return nullptr;
   result.reset(eventListenerRef);

   // AudioUnitEvent is a struct with a discriminator field and a union
   AudioUnitEvent event{ kAudioUnitEvent_ParameterValueChange };
   // Initialize union member -- the ID (second field) reassigned later
   auto &parameter = event.mArgument.mParameter;
   parameter = AudioUnitUtils::Parameter{ unit, kAudioUnitScope_Global };

   // Register each parameter as something we're interested in
   if (instance.GetParameters())
      for (const auto &ID : instance.GetParameters()) {
         parameter.mParameterID = ID;
         if (AUEventListenerAddEventType(result.get(), &instance, &event))
            return nullptr;
      }

   // Now set up the other union member
   event = { kAudioUnitEvent_PropertyChange };
   event.mArgument.mProperty = AudioUnitUtils::Property{
      unit, kAudioUnitProperty_Latency, kAudioUnitScope_Global };
   if (AUEventListenerAddEventType(result.get(), &instance, &event))
      return nullptr;

   return result;
}

unsigned AudioUnitEffect::GetAudioInCount() const
{
   return mAudioIns;
}

unsigned AudioUnitEffect::GetAudioOutCount() const
{
   return mAudioOuts;
}

int AudioUnitEffect::GetMidiInCount() const
{
   return 0;
}

int AudioUnitEffect::GetMidiOutCount() const
{
   return 0;
}

void AudioUnitEffect::SetSampleRate(double rate)
{
   mSampleRate = rate;
}

size_t AudioUnitEffect::SetBlockSize(size_t maxBlockSize)
{
   return mBlockSize;
}

size_t AudioUnitEffect::GetBlockSize() const
{
   return mBlockSize;
}

sampleCount AudioUnitEffect::GetLatency()
{
   // Retrieve the latency (can be updated via an event)
   if (mUseLatency && !mLatencyDone) {
      Float64 latency = 0.0;
      if (!GetFixedSizeProperty(kAudioUnitProperty_Latency, latency)) {
         mLatencyDone = true;
         return sampleCount{ latency * mSampleRate };
      }
   }
   return 0;
}

#if 0
// TODO move to AudioUnitEffect::Instance when that class exists
size_t AudioUnitEffect::GetTailSize() const
{
   // Retrieve the tail time
   Float64 tailTime = 0.0;
   if (!GetFixedSizeProperty(kAudioUnitProperty_TailTime, tailTime))
      return tailTime * mSampleRate;
   return 0;
}
#endif

bool AudioUnitEffect::ProcessInitialize(
   EffectSettings &, sampleCount, ChannelNames chanMap)
{
   mInputList =
      PackedArray::AllocateCount<AudioBufferList>(mAudioIns)(mAudioIns);
   mOutputList =
      PackedArray::AllocateCount<AudioBufferList>(mAudioOuts)(mAudioOuts);

   memset(&mTimeStamp, 0, sizeof(AudioTimeStamp));
   mTimeStamp.mSampleTime = 0; // This is a double-precision number that should
                               // accumulate the number of frames processed so far
   mTimeStamp.mFlags = kAudioTimeStampSampleTimeValid;

   if (!SetRateAndChannels())
      return false;

   if (SetProperty(kAudioUnitProperty_SetRenderCallback,
      AudioUnitUtils::RenderCallback{ RenderCallback, this },
      kAudioUnitScope_Input)) {
      wxLogError("Setting input render callback failed.\n");
      return false;
   }

   if (AudioUnitReset(mUnit.get(), kAudioUnitScope_Global, 0))
      return false;

   if (!BypassEffect(false))
      return false;

   mLatencyDone = false;
   return true;
}

bool AudioUnitEffect::ProcessFinalize()
{
   mOutputList.reset();
   mInputList.reset();
   return true;
}

size_t AudioUnitEffect::ProcessBlock(EffectSettings &,
   const float *const *inBlock, float *const *outBlock, size_t blockLen)
{
   // mAudioIns and mAudioOuts don't change after plugin initialization,
   // so ProcessInitialize() made sufficient allocations
   assert(PackedArray::Count(mInputList) >= mAudioIns);
   for (size_t i = 0; i < mAudioIns; ++i)
      mInputList[i] = { 1, static_cast<UInt32>(sizeof(float) * blockLen),
         const_cast<float*>(inBlock[i]) };

   // See previous comment
   assert(PackedArray::Count(mOutputList) >= mAudioOuts);
   for (size_t i = 0; i < mAudioOuts; ++i)
      mOutputList[i] = { 1, static_cast<UInt32>(sizeof(float) * blockLen),
         outBlock[i] };

   AudioUnitRenderActionFlags flags = 0;
   OSStatus result;

   result = AudioUnitRender(mUnit.get(),
                            &flags,
                            &mTimeStamp,
                            0,
                            blockLen,
                            mOutputList.get());
   if (result != noErr) {
      wxLogError("Render failed: %d %4.4s\n",
         static_cast<int>(result), reinterpret_cast<char *>(&result));
      return 0;
   }

   mTimeStamp.mSampleTime += blockLen;
   return blockLen;
}

bool AudioUnitEffect::RealtimeInitialize(EffectSettings &settings)
{
   return ProcessInitialize(settings, 0, nullptr);
}

bool AudioUnitEffect::RealtimeAddProcessor(
   EffectSettings &settings, unsigned, float sampleRate)
{
   auto slave = std::make_unique<AudioUnitEffect>(
      mPath, mName, mComponent, &mParameters, this);
   if (!slave->InitializeInstance())
      return false;

   slave->SetBlockSize(mBlockSize);
   slave->SetSampleRate(sampleRate);

   if (!slave->StoreSettings(GetSettings(settings)))
      return false;

   if (!slave->ProcessInitialize(settings, 0, nullptr))
      return false;

   mSlaves.push_back(std::move(slave));
   return true;
}

bool AudioUnitEffect::RealtimeFinalize(EffectSettings &) noexcept
{
return GuardedCall<bool>([&]{
   for (size_t i = 0, cnt = mSlaves.size(); i < cnt; i++)
   {
      mSlaves[i]->ProcessFinalize();
   }
   mSlaves.clear();
   return ProcessFinalize();
});
}

bool AudioUnitEffect::RealtimeSuspend()
{
   if (!BypassEffect(true))
   {
      return false;
   }

   for (size_t i = 0, cnt = mSlaves.size(); i < cnt; i++)
   {
      if (!mSlaves[i]->BypassEffect(true))
      {
         return false;
      }
   }

   return true;
}

bool AudioUnitEffect::RealtimeResume() noexcept
{
return GuardedCall<bool>([&]{
   if (!BypassEffect(false))
   {
      return false;
   }

   for (size_t i = 0, cnt = mSlaves.size(); i < cnt; i++)
   {
      if (!mSlaves[i]->BypassEffect(false))
      {
         return false;
      }
   }

   return true;
});
}

bool AudioUnitEffect::RealtimeProcessStart(EffectSettings &)
{
   return true;
}

size_t AudioUnitEffect::RealtimeProcess(size_t group, EffectSettings &settings,
   const float *const *inbuf, float *const *outbuf, size_t numSamples)
{
   wxASSERT(numSamples <= mBlockSize);
   if (group >= mSlaves.size())
      return 0;
   return mSlaves[group]->ProcessBlock(settings, inbuf, outbuf, numSamples);
}

bool AudioUnitEffect::RealtimeProcessEnd(EffectSettings &) noexcept
{
   return true;
}

int AudioUnitEffect::ShowClientInterface(
   wxWindow &parent, wxDialog &dialog, bool forceModal)
{
   if ((SupportsRealtime() || GetType() == EffectTypeAnalyze) && !forceModal) {
      dialog.Show();
      return 0;
   }

   return dialog.ShowModal();
}

EffectSettings AudioUnitEffect::MakeSettings() const
{
   auto result = StatefulPerTrackEffect::MakeSettings();
   // Cause initial population of the map stored in the stateful effect
   if (!mInitialFetchDone) {
      FetchSettings(GetSettings(result));
      mInitialFetchDone = true;
   }
   return result;
}

bool AudioUnitEffect::SaveSettings(
   const EffectSettings &settings, CommandParameters & parms) const
{
   auto &map = GetSettings(settings).values;
   const auto end = map.end();

   // Save settings into CommandParameters
   // Assume settings is an up-to-date cache of AudioUnit state
   ForEachParameter([this, &parms, &map, end](
      const ParameterInfo &pi, AudioUnitParameterID ID
   ){
      if (pi.mName) {
         // Write names, not numbers, as keys in the config file
         if (auto iter = map.find(ID); iter != end)
            parms.Write(*pi.mName, iter->second);
      }
      return true;
   });
   return true;
}

bool AudioUnitEffect::LoadSettings(
   const CommandParameters & parms, EffectSettings &settings) const
{
   // First clean all settings, in case any are not defined in parms
   auto &mySettings = GetSettings(settings);
   mySettings.ResetValues();

   auto &map = mySettings.values;
   const auto end = map.end();

   // Load settings from CommandParameters
   ForEachParameter([this, &parms, &map, end](
      const ParameterInfo &pi, AudioUnitParameterID ID
   ){
      if (pi.mName) {
         double d = 0.0;
         if (auto iter = map.find(ID);
             iter != end && parms.Read(*pi.mName, &d))
            iter->second = d;
      }
      return true;
   });
   return true;
}

bool AudioUnitValidator::UpdateUI()
{
   // Update parameter values in AudioUnit, and propagate to any listeners
   if (StoreSettingsToInstance(mAccess.Get())) {
      // See AUView::viewWillDraw
      if (mpControl)
         mpControl->ForceRedraw();
   
      // This will be the AudioUnit of a stateful instance, not of the effect
      Notify();

      return true;
   }
   return false;
}

bool AudioUnitValidator::ValidateUI()
{
   mAccess.ModifySettings([this](EffectSettings &settings){
#if 0
      // This analogy with other generators doesn't seem to fit AudioUnits
      // How can we define the control mDuration?
      if (GetType() == EffectTypeGenerate)
         settings.extra.SetDuration(mDuration->GetValue());
#endif
      FetchSettingsFromInstance(settings);
   });
   return true;
}

bool AudioUnitValidator::FetchSettingsFromInstance(EffectSettings &settings)
{
   return mInstance.FetchSettings(
      // Change this when GetSettings becomes a static function
      static_cast<const AudioUnitEffect&>(mEffect).GetSettings(settings));
}

bool AudioUnitValidator::StoreSettingsToInstance(const EffectSettings &settings)
{
   return mInstance.StoreSettings(
      // Change this when GetSettings becomes a static function
      static_cast<const AudioUnitEffect&>(mEffect).GetSettings(settings));
}

bool AudioUnitEffect::LoadUserPreset(
   const RegistryPath & name, EffectSettings &settings) const
{
   // To do: externalize state so const_cast isn't needed
   return const_cast<AudioUnitEffect*>(this)->LoadPreset(name, settings);
}

bool AudioUnitEffect::SaveUserPreset(
   const RegistryPath & name, const EffectSettings &settings) const
{
   return SavePreset(name, GetSettings(settings));
}

bool AudioUnitEffect::LoadFactoryPreset(int id, EffectSettings &settings) const
{
   // Retrieve the list of factory presets
   CF_ptr<CFArrayRef> array;
   if (GetFixedSizeProperty(kAudioUnitProperty_FactoryPresets, array) ||
       id < 0 || id >= CFArrayGetCount(array.get()))
      return false;

   // Mutate the scratch pad AudioUnit in the effect
   if (!SetProperty(kAudioUnitProperty_PresentPreset,
      *static_cast<const AUPreset*>(CFArrayGetValueAtIndex(array.get(), id)))) {
      // Repopulate the AudioUnitEffectSettings from the change of state in
      // the AudioUnit
      FetchSettings(GetSettings(settings));
      return true;
   }
   return false;
}

RegistryPaths AudioUnitEffect::GetFactoryPresets() const
{
   RegistryPaths presets;

   // Retrieve the list of factory presets
   CF_ptr<CFArrayRef> array;
   if (!GetFixedSizeProperty(kAudioUnitProperty_FactoryPresets, array))
      for (CFIndex i = 0, cnt = CFArrayGetCount(array.get()); i < cnt; ++i)
         presets.push_back(wxCFStringRef::AsString(
            static_cast<const AUPreset*>(CFArrayGetValueAtIndex(array.get(), i))
               ->presetName));
   return presets;
}

// ============================================================================
// EffectUIClientInterface Implementation
// ============================================================================

std::unique_ptr<EffectUIValidator> AudioUnitValidator::Create(
   EffectUIClientInterface &effect, ShuttleGui &S,
   const wxString &uiType,
   EffectInstance &instance, EffectSettingsAccess &access)
{
   const auto parent = S.GetParent();
   // Cast is assumed to succeed because only this effect's own instances
   // are passed back by the framework
   auto &myInstance = dynamic_cast<AudioUnitInstance&>(instance);

   AUControl *pControl{};
   wxPanel *container{};
   {
      auto mainSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);
      wxASSERT(parent); // To justify safenew
      container = safenew wxPanelWrapper(parent, wxID_ANY);
      mainSizer->Add(container, 1, wxEXPAND);
      parent->SetSizer(mainSizer.release());
   }

#if defined(HAVE_AUDIOUNIT_BASIC_SUPPORT)
   if (uiType == BasicValue.MSGID().GET()) {
      if (!CreatePlain(mParent))
         return nullptr;
   }
   else
#endif
   {
      auto uControl = Destroy_ptr<AUControl>(safenew AUControl);
      if (!uControl)
         return nullptr;
      pControl = uControl.get();

      if (!pControl->Create(container, myInstance.GetComponent(),
         myInstance.GetAudioUnit(),
         uiType == FullValue.MSGID().GET()))
         return nullptr;

      {
         auto innerSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);

         innerSizer->Add(uControl.release(), 1, wxEXPAND);
         container->SetSizer(innerSizer.release());
      }

      parent->SetMinSize(wxDefaultSize);

#ifdef __WXMAC__
#ifdef __WX_EVTLOOP_BUSY_WAITING__
      wxEventLoop::SetBusyWaiting(true);
#endif
#endif
   }

   return std::make_unique<AudioUnitValidator>(
      CreateToken{}, effect, access, myInstance, pControl);
}

std::unique_ptr<EffectUIValidator> AudioUnitEffect::PopulateUI(ShuttleGui &S,
   EffectInstance &instance, EffectSettingsAccess &access)
{
   mParent = S.GetParent();
   return AudioUnitValidator::Create(*this, S, mUIType, instance, access);
}

bool AudioUnitEffect::IsGraphicalUI()
{
   return mUIType != wxT("Plain");
}

#if defined(HAVE_AUDIOUNIT_BASIC_SUPPORT)
bool AudioUnitEffect::CreatePlain(wxWindow *parent)
{
   // TODO???  Never implemented...
   return false;
}
#endif

bool AudioUnitEffect::CloseUI()
{
#ifdef __WXMAC__
#ifdef __WX_EVTLOOP_BUSY_WAITING__
   wxEventLoop::SetBusyWaiting(false);
#endif
#endif
   mParent = nullptr;
   return true;
}

bool AudioUnitEffect::CanExportPresets()
{
   return true;
}

void AudioUnitEffect::ExportPresets(const EffectSettings &settings) const
{
   // Generate the user domain path
   wxFileName fn;
   fn.SetPath(PRESET_USER_PATH);
   fn.AppendDir(mVendor);
   fn.AppendDir(mName);
   fn.Normalize();
   FilePath path = fn.GetFullPath();

   if (!fn.Mkdir(fn.GetFullPath(), 0755, wxPATH_MKDIR_FULL)) {
      wxLogError(wxT("Couldn't create the \"%s\" directory"), fn.GetPath());
      return;
   }

   // Ask the user for the name to use
   //
   // Passing a valid parent will cause some effects dialogs to malfunction
   // upon returning from the SelectFile().
   path = SelectFile(FileNames::Operation::_None,
      XO("Export Audio Unit Preset As %s:").Format(fn.GetFullPath()),
      fn.GetFullPath(),
      wxEmptyString,
      wxT("aupreset"),
      {
        { XO("Standard Audio Unit preset file"), { wxT("aupreset") }, true },
      },
      wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER,
      NULL);

   // User canceled...
   if (path.empty())
      return;

   auto msg = Export(GetSettings(settings), path);
   if (!msg.empty())
      AudacityMessageBox(
         XO("Could not export \"%s\" preset\n\n%s").Format(path, msg),
         XO("Export Audio Unit Presets"),
         wxOK | wxCENTRE,
         mParent);
}

void AudioUnitEffect::ImportPresets(EffectSettings &settings)
{
   // Generate the user domain path
   wxFileName fn;
   fn.SetPath(PRESET_USER_PATH);
   fn.AppendDir(mVendor);
   fn.AppendDir(mName);
   fn.Normalize();
   FilePath path = fn.GetFullPath();

   // Ask the user for the name to use
   //
   // Passing a valid parent will cause some effects dialogs to malfunction
   // upon returning from the SelectFile().
   path = SelectFile(FileNames::Operation::_None,
      XO("Import Audio Unit Preset As %s:").Format(fn.GetFullPath()),
      fn.GetFullPath(), wxEmptyString, wxT("aupreset"),
      {
        { XO("Standard Audio Unit preset file"), { wxT("aupreset") }, true },
      },
      wxFD_OPEN | wxRESIZE_BORDER,
      nullptr);

   // User canceled...
   if (path.empty())
      return;

   auto msg = Import(GetSettings(settings), path);
   if (!msg.empty())
      AudacityMessageBox(
         XO("Could not import \"%s\" preset\n\n%s").Format(path, msg),
         XO("Import Audio Unit Presets"),
         wxOK | wxCENTRE,
         mParent);
}

bool AudioUnitEffect::HasOptions()
{
   return true;
}

void AudioUnitEffect::ShowOptions()
{
   AudioUnitEffectOptionsDialog dlg(mParent, mUseLatency, mUIType);
   if (dlg.ShowModal()) {
      // Save changed values to the config file
      SetConfig(*this, PluginSettings::Shared, OptionsKey, UseLatencyKey,
         mUseLatency);
      SetConfig(*this, PluginSettings::Shared, OptionsKey, UITypeKey, mUIType);
   }
}

// ============================================================================
// AudioUnitEffect Implementation
// ============================================================================

bool AudioUnitEffect::MigrateOldConfigFile(
   const RegistryPath & group, EffectSettings &settings) const
{
   // Migration of very old format configuration file, should not normally
   // happen and perhaps this code can be abandoned
   // Attempt to load old preset parameters and resave using new method
   constexpr auto oldKey = L"Parameters";
   wxString parms;
   if (GetConfig(*this, PluginSettings::Private,
      group, oldKey, parms, wxEmptyString)) {
      CommandParameters eap;
      if (eap.SetParameters(parms))
         if (LoadSettings(eap, settings))
            if (SavePreset(group, GetSettings(settings)))
               RemoveConfig(*this, PluginSettings::Private, group, oldKey);
      return true;
   }
   return false;
}

bool AudioUnitEffect::LoadPreset(
   const RegistryPath & group, EffectSettings &settings) const
{
   if (MigrateOldConfigFile(group, settings))
      return true;

   // Retrieve the preset
   wxString parms;
   if (!GetConfig(*this, PluginSettings::Private, group, PRESET_KEY, parms,
      wxEmptyString)) {
      // Commented "CurrentSettings" gets tried a lot and useless messages appear
      // in the log
      //wxLogError(wxT("Preset key \"%s\" not found in group \"%s\""), PRESET_KEY, group);
      return false;
   }
   
   // Decode it, complementary to what SaveBlobToConfig did
   auto error =
      InterpretBlob(GetSettings(settings), group, wxBase64Decode(parms));
   if (!error.empty()) {
      wxLogError(error.Debug());
      return false;
   }

   return true;
}

bool AudioUnitEffect::SavePreset(
   const RegistryPath & group, const AudioUnitEffectSettings &settings) const
{
   wxCFStringRef cfname(wxFileNameFromPath(group));
   const auto &[data, _] = MakeBlob(settings, cfname, true);
   if (!data)
      return false;

   // Nothing to do if we don't have any data
   if (const auto length = CFDataGetLength(data.get())) {
      auto error =
         SaveBlobToConfig(group, {}, CFDataGetBytePtr(data.get()), length);
      if (!error.empty())
         return false;
   }
   return true;
}

bool AudioUnitEffect::SetRateAndChannels()
{
   mInitialization.reset();
   AudioUnitUtils::StreamBasicDescription streamFormat{
      // Float64 mSampleRate;
      mSampleRate,

      // UInt32  mFormatID;
      kAudioFormatLinearPCM,

      // UInt32  mFormatFlags;
      (kAudioFormatFlagsNativeFloatPacked |
          kAudioFormatFlagIsNonInterleaved),

      // UInt32  mBytesPerPacket;
      sizeof(float),

      // UInt32  mFramesPerPacket;
      1,

      // UInt32  mBytesPerFrame;
      sizeof(float),

      // UInt32  mChannelsPerFrame;
      0,

      // UInt32  mBitsPerChannel;
      sizeof(float) * 8,
   };

   const struct Info{
      unsigned nChannels;
      AudioUnitScope scope;
      const char *const msg; // used only in log messages
   } infos[]{
      { 1, kAudioUnitScope_Global, "global" },
      { mAudioIns, kAudioUnitScope_Input, "input" },
      { mAudioOuts, kAudioUnitScope_Output, "output" },
   };
   for (const auto &[nChannels, scope, msg] : infos) {
      if (nChannels) {
         if (SetProperty(kAudioUnitProperty_SampleRate, mSampleRate, scope)) {
            wxLogError("%ls Didn't accept sample rate on %s\n",
               // Exposing internal name only in logging
               GetSymbol().Internal().wx_str(), msg);
            return false;
         }
         if (scope != kAudioUnitScope_Global) {
            streamFormat.mChannelsPerFrame = nChannels;
            if (SetProperty(kAudioUnitProperty_StreamFormat,
               streamFormat, scope)) {
               wxLogError("%ls didn't accept stream format on %s\n",
                  // Exposing internal name only in logging
                  GetSymbol().Internal().wx_str(), msg);
               return false;
            }
         }
      }
   }

   if (AudioUnitInitialize(mUnit.get())) {
      wxLogError("Couldn't initialize audio unit\n");
      return false;
   }

   mInitialization.reset(mUnit.get());
   return true;
}

TranslatableString AudioUnitEffect::Export(
   const AudioUnitEffectSettings &settings, const wxString & path) const
{
   // Create the file
   wxFFile f(path, wxT("wb"));
   if (!f.IsOpened())
      return XO("Couldn't open \"%s\"").Format(path);

   // First set the name of the preset
   wxCFStringRef cfname(wxFileName(path).GetName());

   const auto &[data, message] = MakeBlob(settings, cfname, false);
   if (!data || !message.empty())
      return message;

   // Write XML data
   auto length = CFDataGetLength(data.get());
   if (f.Write(CFDataGetBytePtr(data.get()), length) != length || f.Error())
      return XO("Failed to write XML preset to \"%s\"").Format(path);

   f.Close();
   return {};
}

TranslatableString AudioUnitEffect::Import(
   AudioUnitEffectSettings &settings, const wxString & path) const
{
   // Open the preset
   wxFFile f(path, wxT("r"));
   if (!f.IsOpened())
      return XO("Couldn't open \"%s\"").Format(path);

   // Load it into the buffer
   size_t len = f.Length();
   wxMemoryBuffer buf(len);
   if (f.Read(buf.GetData(), len) != len || f.Error())
      return XO("Unable to read the preset from \"%s\"").Format(path);
   buf.SetDataLen(len);

   const auto error = InterpretBlob(settings, path, buf);
   if (!error.empty())
      return error;

   return {};
}

void AudioUnitValidator::Notify()
{
   AudioUnitParameter aup = {};
   aup.mAudioUnit = mInstance.GetAudioUnit();
   aup.mParameterID = kAUParameterListener_AnyParameter;
   aup.mScope = kAudioUnitScope_Global;
   aup.mElement = 0;
   AUParameterListenerNotify(NULL, NULL, &aup);
}

OSStatus AudioUnitEffect::Render(AudioUnitRenderActionFlags *inActionFlags,
                                 const AudioTimeStamp *inTimeStamp,
                                 UInt32 inBusNumber,
                                 UInt32 inNumFrames,
                                 AudioBufferList *ioData)
{
   size_t i = 0;
   auto size =
      std::min<size_t>(ioData->mNumberBuffers, PackedArray::Count(mInputList));
   for (; i < size; ++i)
      ioData->mBuffers[i].mData = mInputList[i].mData;
   // Some defensive code here just in case SDK requests from us an unexpectedly
   // large number of buffers:
   for (; i < ioData->mNumberBuffers; ++i)
      ioData->mBuffers[i].mData = nullptr;
   return 0;
}

// static
OSStatus AudioUnitEffect::RenderCallback(void *inRefCon,
                                         AudioUnitRenderActionFlags *inActionFlags,
                                         const AudioTimeStamp *inTimeStamp,
                                         UInt32 inBusNumber,
                                         UInt32 inNumFrames,
                                         AudioBufferList *ioData)
{
   return static_cast<AudioUnitEffect *>(inRefCon)->Render(inActionFlags,
      inTimeStamp, inBusNumber, inNumFrames, ioData);
}

void AudioUnitInstance::EventListener(const AudioUnitEvent *inEvent,
   AudioUnitParameterValue inParameterValue)
{
   // Handle property changes
   if (inEvent->mEventType == kAudioUnitEvent_PropertyChange) {
      // Handle latency changes
      if (inEvent->mArgument.mProperty.mPropertyID ==
          kAudioUnitProperty_Latency) {
         // Allow change to be used
         //mLatencyDone = false;
      }
      return;
   }

   // Only parameter changes at this point
   // Propagate the parameter
   // TODO store slaves in the instance
   auto &slaves = static_cast<const AudioUnitEffect&>(GetEffect()).mSlaves;
   for (auto &worker : slaves)
      AudioUnitSetParameter(worker->GetAudioUnit(),
         inEvent->mArgument.mParameter.mParameterID,
         kAudioUnitScope_Global, 0, inParameterValue, 0);
}

// static
void AudioUnitInstance::EventListenerCallback(void *inCallbackRefCon,
   void *inObject, const AudioUnitEvent *inEvent, UInt64 inEventHostTime,
   AudioUnitParameterValue inParameterValue)
{
   static_cast<AudioUnitInstance *>(inCallbackRefCon)
      ->EventListener(inEvent, inParameterValue);
}

void AudioUnitEffect::GetChannelCounts()
{
   // Does AU have channel info
   PackedArray::Ptr<AUChannelInfo> info;
   if (GetVariableSizeProperty(kAudioUnitProperty_SupportedNumChannels, info)) {
      // None supplied.  Apparently all FX type units can do any number of INs
      // and OUTs as long as they are the same number.  In this case, we'll
      // just say stereo.
      //
      // We should probably check to make sure we're dealing with an FX type.
      mAudioIns = 2;
      mAudioOuts = 2;
      return;
   }

   // This is where it gets weird...not sure what is the best
   // way to do this really.  If we knew how many ins/outs we
   // really needed, we could make a better choice.

   bool haven2m = false;   // nothing -> mono
   bool haven2s = false;   // nothing -> stereo
   bool havem2n = false;   // mono -> nothing
   bool haves2n = false;   // stereo -> nothing
   bool havem2m = false;   // mono -> mono
   bool haves2s = false;   // stereo -> stereo
   bool havem2s = false;   // mono -> stereo
   bool haves2m = false;   // stereo -> mono

   mAudioIns = 2;
   mAudioOuts = 2;

   // Look only for exact channel constraints
   for (auto &ci : info) {
      int ic = ci.inChannels;
      int oc = ci.outChannels;

      if (ic < 0 && oc >= 0)
         ic = 2;
      else if (ic >= 0 && oc < 0)
         oc = 2;
      else if (ic < 0 && oc < 0) {
         ic = 2;
         oc = 2;
      }

      if (ic == 2 && oc == 2)
         haves2s = true;
      else if (ic == 1 && oc == 1)
         havem2m = true;
      else if (ic == 1 && oc == 2)
         havem2s = true;
      else if (ic == 2 && oc == 1)
         haves2m = true;
      else if (ic == 0 && oc == 2)
         haven2s = true;
      else if (ic == 0 && oc == 1)
         haven2m = true;
      else if (ic == 1 && oc == 0)
         havem2n = true;
      else if (ic == 2 && oc == 0)
         haves2n = true;
   }

   if (haves2s) {
      mAudioIns = 2;
      mAudioOuts = 2;
   }
   else if (havem2m) {
      mAudioIns = 1;
      mAudioOuts = 1;
   }
   else if (havem2s) {
      mAudioIns = 1;
      mAudioOuts = 2;
   }
   else if (haves2m) {
      mAudioIns = 2;
      mAudioOuts = 1;
   }
   else if (haven2m) {
      mAudioIns = 0;
      mAudioOuts = 1;
   }
   else if (haven2s) {
      mAudioIns = 0;
      mAudioOuts = 2;
   }
   else if (haves2n) {
      mAudioIns = 2;
      mAudioOuts = 0;
   }
   else if (havem2n) {
      mAudioIns = 1;
      mAudioOuts = 0;
   }

   return;
}

bool AudioUnitEffect::BypassEffect(bool bypass)
{
   UInt32 value = (bypass ? 1 : 0);
   return !SetProperty(kAudioUnitProperty_BypassEffect, value);
}

#endif
