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
                                 const wxString & name,
                                 AudioComponent component,
                                 AudioUnitEffect *master)
   : AudioUnitWrapper{ component }
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
   return GetType() == EffectTypeProcess;
}

bool AudioUnitEffect::SupportsAutomation() const
{
   bool supports = false;
   return ForEachParameter(
   [&supports](const ParameterInfo &pi, AudioUnitParameterID) {
      if (pi.mInfo.flags & kAudioUnitParameterFlag_IsWritable)
         supports = true;
      // Search only until we find one, that's all we need to know
      return !supports;
   }) && supports;
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

   // Is this really needed here or can it be done in MakeInstance()
   // only?  I think it can, but this is more a conservative change for now,
   // preserving what SetHost() did
   return MakeListener();
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
   return std::make_shared<Instance>(*this);
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

bool AudioUnitEffect::MakeListener()
{
   if (!mMaster)
   {
      // Don't have a master -- so this IS the master.
      OSStatus result;
      AUEventListenerRef eventListenerRef{};
      result = AUEventListenerCreate(AudioUnitEffect::EventListenerCallback,
                                    this,
                                    (CFRunLoopRef)GetCFRunLoopFromEventLoop(GetCurrentEventLoop()),
                                    kCFRunLoopDefaultMode,
                                    0.0,
                                    0.0,
                                    &eventListenerRef);
      if (result != noErr)
         return false;
      mEventListenerRef.reset(eventListenerRef);

      AudioUnitEvent event;
 
      event.mEventType = kAudioUnitEvent_ParameterValueChange;
      event.mArgument.mParameter.mAudioUnit = mUnit.get();
      event.mArgument.mParameter.mScope = kAudioUnitScope_Global;
      event.mArgument.mParameter.mElement = 0;

      // Retrieve the list of parameters
      PackedArray::Ptr<AudioUnitParameterID> array;
      if (GetVariableSizeProperty(kAudioUnitProperty_ParameterList, array))
         return false;

      // Register them as something we're interested in
      for (const auto &ID : array) {
         event.mArgument.mParameter.mParameterID = ID;
         if (AUEventListenerAddEventType(mEventListenerRef.get(), this, &event))
            return false;
      }

      event.mEventType = kAudioUnitEvent_PropertyChange;
      event.mArgument.mProperty.mAudioUnit = mUnit.get();
      event.mArgument.mProperty.mPropertyID = kAudioUnitProperty_Latency;
      event.mArgument.mProperty.mScope = kAudioUnitScope_Global;
      event.mArgument.mProperty.mElement = 0;

      result = AUEventListenerAddEventType(mEventListenerRef.get(),
                                           this,
                                           &event);
      if (result != noErr)
      {
         return false;
      }
   
      // Check for a Cocoa UI
      // This could retrieve a variable-size property, but we only look at
      // the first element.
      AudioUnitCocoaViewInfo cocoaViewInfo;
      bool hasCocoa =
         !GetFixedSizeProperty(kAudioUnitProperty_CocoaUI, cocoaViewInfo);

      // Check for a Carbon UI
      // This could retrieve a variable sized array but we only need the first
      AudioComponentDescription compDesc;
      bool hasCarbon =
         !GetFixedSizeProperty(kAudioUnitProperty_GetUIComponentList, compDesc);

      mInteractive = (PackedArray::Count(array) > 0) || hasCocoa || hasCarbon;
   }

   return true;
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
   auto slave = std::make_unique<AudioUnitEffect>(mPath, mName, mComponent, this);
   if (!slave->InitializeInstance())
      return false;

   slave->SetBlockSize(mBlockSize);
   slave->SetSampleRate(sampleRate);

   if (!CopyParameters(mUnit.get(), slave->mUnit.get()))
   {
      return false;
   }

   auto pSlave = slave.get();
   mSlaves.push_back(std::move(slave));

   return pSlave->ProcessInitialize(settings, 0, nullptr);
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

size_t AudioUnitEffect::RealtimeProcess(int group, EffectSettings &settings,
   const float *const *inbuf, float *const *outbuf, size_t numSamples)
{
   wxASSERT(numSamples <= mBlockSize);
   return mSlaves[group]->ProcessBlock(settings, inbuf, outbuf, numSamples);
}

bool AudioUnitEffect::RealtimeProcessEnd(EffectSettings &) noexcept
{
   return true;
}

int AudioUnitEffect::ShowClientInterface(
   wxWindow &parent, wxDialog &dialog, bool forceModal)
{
   // Remember the dialog with a weak pointer, but don't control its lifetime
   mDialog = &dialog;
   if ((SupportsRealtime() || GetType() == EffectTypeAnalyze) && !forceModal)
   {
      mDialog->Show();
      return 0;
   }

   return mDialog->ShowModal();
}

bool AudioUnitEffect::SaveSettings(
   const EffectSettings &, CommandParameters & parms) const
{
   return ForEachParameter(
   [this, &parms](const ParameterInfo &pi, AudioUnitParameterID ID) {
      AudioUnitParameterValue value;
      if (!pi.mName ||
         AudioUnitGetParameter(
            mUnit.get(), ID, kAudioUnitScope_Global, 0, &value))
         // Probably failed because of invalid parameter which can happen
         // if a plug-in is in a certain mode that doesn't contain the
         // parameter.  In any case, just ignore it.
         {}
      else
         parms.Write(*pi.mName, value);
      return true;
   });
}

bool AudioUnitEffect::LoadSettings(
   const CommandParameters & parms, EffectSettings &settings) const
{
   bool success = true;
   return ForEachParameter(
   [this, &parms, &success](const ParameterInfo &pi, AudioUnitParameterID ID) {
      double d = 0.0;
      if (pi.mName &&
         parms.Read(*pi.mName, &d)
      ) {
         if (AudioUnitSetParameter(
            mUnit.get(), ID, kAudioUnitScope_Global, 0, d, 0))
            success = false;
         else
            Notify(mUnit.get(), ID);
      }
      return success;
   }) && success;
}

bool AudioUnitEffect::LoadUserPreset(
   const RegistryPath & name, EffectSettings &settings) const
{
   // To do: externalize state so const_cast isn't needed
   return const_cast<AudioUnitEffect*>(this)->LoadPreset(name, settings);
}

bool AudioUnitEffect::SaveUserPreset(
   const RegistryPath & name, const EffectSettings &) const
{
   return SavePreset(name);
}

bool AudioUnitEffect::LoadFactoryPreset(int id, EffectSettings &) const
{
   // Retrieve the list of factory presets
   CF_ptr<CFArrayRef> array;
   if (GetFixedSizeProperty(kAudioUnitProperty_FactoryPresets, array) ||
       id < 0 || id >= CFArrayGetCount(array.get()))
      return false;

   if (!SetProperty(kAudioUnitProperty_PresentPreset,
      *static_cast<const AUPreset*>(CFArrayGetValueAtIndex(array.get(), id)))) {
      // Notify interested parties of change and propagate to slaves
      Notify(mUnit.get(), kAUParameterListener_AnyParameter);
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

std::unique_ptr<EffectUIValidator> AudioUnitEffect::PopulateUI(ShuttleGui &S,
   EffectInstance &, EffectSettingsAccess &access)
{
   // OSStatus result;

   auto parent = S.GetParent();
   mDialog = static_cast<wxDialog *>(wxGetTopLevelParent(parent));
   mParent = parent;
   mpControl = NULL;

   wxPanel *container;
   {
      auto mainSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);

      wxASSERT(mParent); // To justify safenew
      container = safenew wxPanelWrapper(mParent, wxID_ANY);
      mainSizer->Add(container, 1, wxEXPAND);

      mParent->SetSizer(mainSizer.release());
   }

#if defined(HAVE_AUDIOUNIT_BASIC_SUPPORT)
   if (mUIType == BasicValue.MSGID().GET()) {
      if (!CreatePlain(mParent))
         return nullptr;
   }
   else
#endif
   {
      auto pControl = Destroy_ptr<AUControl>(safenew AUControl);
      if (!pControl)
      {
         return nullptr;
      }

      if (!pControl->Create(container, mComponent, mUnit.get(),
         mUIType == FullValue.MSGID().GET()))
         return nullptr;

      {
         auto innerSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);

         innerSizer->Add((mpControl = pControl.release()), 1, wxEXPAND);
         container->SetSizer(innerSizer.release());
      }

      mParent->SetMinSize(wxDefaultSize);

#ifdef __WXMAC__
#ifdef __WX_EVTLOOP_BUSY_WAITING__
      wxEventLoop::SetBusyWaiting(true);
#endif
#endif
   }

   if (mpControl)
   {
      mParent->PushEventHandler(this);
   }

   return std::make_unique<DefaultEffectUIValidator>(*this, access);
}

bool AudioUnitEffect::IsGraphicalUI()
{
   return mUIType != wxT("Plain");
}

bool AudioUnitEffect::ValidateUI([[maybe_unused]] EffectSettings &settings)
{
#if 0
   if (GetType() == EffectTypeGenerate)
      settings.extra.SetDuration(mDuration->GetValue());
#endif
   return true;
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
   if (mpControl)
   {
      mParent->RemoveEventHandler(this);

      mpControl->Close();
      mpControl = nullptr;
   }
#endif

   mParent = NULL;
   mDialog = NULL;

   return true;
}

bool AudioUnitEffect::CanExportPresets()
{
   return true;
}

void AudioUnitEffect::ExportPresets(const EffectSettings &) const
{
   // Generate the user domain path
   wxFileName fn;
   fn.SetPath(PRESET_USER_PATH);
   fn.AppendDir(mVendor);
   fn.AppendDir(mName);
   fn.Normalize();
   FilePath path = fn.GetFullPath();

   if (!fn.Mkdir(fn.GetFullPath(), 0755, wxPATH_MKDIR_FULL))
   {
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
   {
      return;
   }

   auto msg = Export(path);
   if (!msg.empty())
   {
      AudacityMessageBox(
         XO("Could not export \"%s\" preset\n\n%s").Format(path, msg),
         XO("Export Audio Unit Presets"),
         wxOK | wxCENTRE,
         mParent);
   }
}

void AudioUnitEffect::ImportPresets(EffectSettings &)
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
      fn.GetFullPath(),
      wxEmptyString,
      wxT("aupreset"),
      {
        { XO("Standard Audio Unit preset file"), { wxT("aupreset") }, true },
      },
      wxFD_OPEN | wxRESIZE_BORDER,
      NULL);

   // User canceled...
   if (path.empty())
   {
      return;
   }

   auto msg = Import(path);
   if (!msg.empty())
   {
      AudacityMessageBox(
         XO("Could not import \"%s\" preset\n\n%s").Format(path, msg),
         XO("Import Audio Unit Presets"),
         wxOK | wxCENTRE,
         mParent);
   }
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

bool AudioUnitEffect::LoadPreset(
   const RegistryPath & group, EffectSettings &settings) const
{
   wxString parms;

   // Attempt to load old preset parameters and resave using new method
   if (GetConfig(*this, PluginSettings::Private, group, wxT("Parameters"),
      parms, wxEmptyString)) {
      CommandParameters eap;
      if (eap.SetParameters(parms))
         if (LoadSettings(eap, settings))
            if (SavePreset(group))
               RemoveConfig(*this, PluginSettings::Private,
                  group, wxT("Parameters"));
      return true;
   }

   // Retrieve the preset
   if (!GetConfig(*this, PluginSettings::Private, group, PRESET_KEY, parms,
      wxEmptyString)) {
      // Commented "CurrentSettings" gets tried a lot and useless messages appear
      // in the log
      //wxLogError(wxT("Preset key \"%s\" not found in group \"%s\""), PRESET_KEY, group);
      return false;
   }
   
   // Decode it, complementary to what SaveBlobToConfig did
   auto error = InterpretBlob(group, wxBase64Decode(parms));
   if (!error.empty()) {
      wxLogError(error.Debug());
      return false;
   }

   // See AUView::viewWillDraw
   if (mpControl)
      mpControl->ForceRedraw();

   // Notify interested parties of change and propagate to slaves
   Notify(mUnit.get(), kAUParameterListener_AnyParameter);
   return true;
}

bool AudioUnitEffect::SavePreset(const RegistryPath & group) const
{
   wxCFStringRef cfname(wxFileNameFromPath(group));
   const auto &[data, _] = MakeBlob(cfname, true);
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

bool AudioUnitEffect::CopyParameters(AudioUnit srcUnit, AudioUnit dstUnit)
{
   // Retrieve the class state from the source AU
   CF_ptr<CFPropertyListRef> content;
   if (AudioUnitUtils::GetFixedSizeProperty(srcUnit,
      kAudioUnitProperty_ClassInfo, content))
      return false;

   // Set the destination AUs state from the source AU's content
   if (AudioUnitUtils::SetProperty(dstUnit,
      kAudioUnitProperty_ClassInfo, content))
      return false;

   // Notify interested parties
   Notify(dstUnit, kAUParameterListener_AnyParameter);

   return true;
}

TranslatableString AudioUnitEffect::Export(const wxString & path) const
{
   // Create the file
   wxFFile f(path, wxT("wb"));
   if (!f.IsOpened())
      return XO("Couldn't open \"%s\"").Format(path);

   // First set the name of the preset
   wxCFStringRef cfname(wxFileName(path).GetName());

   const auto &[data, message] = MakeBlob(cfname, false);
   if (!data || !message.empty())
      return message;

   // Write XML data
   auto length = CFDataGetLength(data.get());
   if (f.Write(CFDataGetBytePtr(data.get()), length) != length || f.Error())
      return XO("Failed to write XML preset to \"%s\"").Format(path);

   f.Close();
   return {};
}

TranslatableString AudioUnitEffect::Import(const wxString & path)
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

   const auto error = InterpretBlob(path, buf);
   if (!error.empty())
      return error;

   // Notify interested parties of change and propagate to slaves
   Notify(mUnit.get(), kAUParameterListener_AnyParameter);

   return {};
}

void AudioUnitEffect::Notify(AudioUnit unit, AudioUnitParameterID parm) const
{
   // Notify any interested parties
   AudioUnitParameter aup = {};
   aup.mAudioUnit = unit;
   aup.mParameterID = parm;
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

void AudioUnitEffect::EventListener(const AudioUnitEvent *inEvent,
                                    AudioUnitParameterValue inParameterValue)
{
   // Handle property changes
   if (inEvent->mEventType == kAudioUnitEvent_PropertyChange)
   {
      // Handle latency changes
      if (inEvent->mArgument.mProperty.mPropertyID == kAudioUnitProperty_Latency)
      {
         // Allow change to be used
         //mLatencyDone = false;
      }

      return;
   }

   // Only parameter changes at this point

   if (mMaster)
   {
      // We're a slave, so just set the parameter
      AudioUnitSetParameter(mUnit.get(),
         inEvent->mArgument.mParameter.mParameterID,
         kAudioUnitScope_Global, 0, inParameterValue, 0);
   }
   else
   {
      // We're the master, so propagate 
      for (size_t i = 0, cnt = mSlaves.size(); i < cnt; i++)
      {
         mSlaves[i]->EventListener(inEvent, inParameterValue);
      }
   }
}

// static
void AudioUnitEffect::EventListenerCallback(void *inCallbackRefCon,
                                            void *inObject,
                                            const AudioUnitEvent *inEvent,
                                            UInt64 inEventHostTime,
                                            AudioUnitParameterValue inParameterValue)
{
   static_cast<AudioUnitEffect *>(inCallbackRefCon)
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
