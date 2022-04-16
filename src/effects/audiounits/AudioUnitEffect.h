/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioUnitEffect.h

  Dominic Mazzoni
  Leland Lucius

**********************************************************************/
#ifndef AUDACITY_AUDIOUNIT_EFFECT_H



#if USE_AUDIO_UNITS

#include "MemoryX.h"
#include <functional>
#include <type_traits>
#include <unordered_map>
#include <vector>

#include <AudioToolbox/AudioUnitUtilities.h>
#include "AudioUnitUtils.h"
#include <AudioUnit/AudioUnitProperties.h>

#include "../StatefulPerTrackEffect.h"
#include "CFResources.h"
#include "PluginProvider.h"
#include "PluginInterface.h"

#include <wx/weakref.h>

#define AUDIOUNITEFFECTS_VERSION wxT("1.0.0.0")
/* i18n-hint: the name of an Apple audio software protocol */
#define AUDIOUNITEFFECTS_FAMILY EffectFamilySymbol{ wxT("AudioUnit"), XO("Audio Unit") }
class AudioUnitEffect;

using AudioUnitEffectArray = std::vector<std::unique_ptr<AudioUnitEffect>>;

class AudioUnitEffectExportDialog;
class AudioUnitEffectImportDialog;
class AUControl;
class wxCFStringRef;
class wxMemoryBuffer;

//! Generates deleters for std::unique_ptr that clean up AU plugin state
template<typename T, OSStatus(*fn)(T*)> struct AudioUnitCleaner {
   // Let this have non-void return type, though ~unique_ptr() will ignore it
   auto operator () (T *p) noexcept { return fn(p); }
};
//! RAII for cleaning up AU plugin state
template<typename Ptr, OSStatus(*fn)(Ptr),
   typename T = std::remove_pointer_t<Ptr> /* deduced */ >
using AudioUnitCleanup = std::unique_ptr<T, AudioUnitCleaner<T, fn>>;

template<> struct PackedArrayTraits<AudioBufferList> {
   struct header_type { UInt32 mNumberBuffers; };
   using element_type = AudioBuffer;
   using iterated_type = element_type;
};

struct AudioUnitEffectSettings {
   std::unordered_map<wxString, AudioUnitParameterValue> values;
};

class AudioUnitEffect final : public StatefulPerTrackEffect
{
public:
   AudioUnitEffect(const PluginPath & path,
                   const wxString & name,
                   AudioComponent component,
                   AudioUnitEffect *master = NULL);
   virtual ~AudioUnitEffect();

   // ComponentInterface implementation

   PluginPath GetPath() const override;
   ComponentInterfaceSymbol GetSymbol() const override;
   VendorSymbol GetVendor() const override;
   wxString GetVersion() const override;
   TranslatableString GetDescription() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;
   EffectFamilySymbol GetFamily() const override;
   bool IsInteractive() const override;
   bool IsDefault() const override;
   bool SupportsRealtime() const override;
   bool SupportsAutomation() const override;

   bool FetchSettings(AudioUnitEffectSettings &settings) const;
   bool SaveSettings(
      const EffectSettings &settings, CommandParameters & parms) const override;
   bool LoadSettings(
      const CommandParameters & parms, EffectSettings &settings) const override;

   bool LoadUserPreset(
      const RegistryPath & name, EffectSettings &settings) const override;
   bool SaveUserPreset(
      const RegistryPath & name, const EffectSettings &settings) const override;

   RegistryPaths GetFactoryPresets() const override;
   bool LoadFactoryPreset(int id, EffectSettings &settings) const override;
   bool LoadFactoryDefaults(EffectSettings &settings) const override;

   unsigned GetAudioInCount() const override;
   unsigned GetAudioOutCount() const override;

   int GetMidiInCount() const override;
   int GetMidiOutCount() const override;

   void SetSampleRate(double rate) override;
   size_t SetBlockSize(size_t maxBlockSize) override;
   size_t GetBlockSize() const override;

   sampleCount GetLatency() override;
   // size_t GetTailSize() const override;

   bool ProcessInitialize(EffectSettings &settings,
      sampleCount totalLen, ChannelNames chanMap) override;
   bool ProcessFinalize() override;
   size_t ProcessBlock(EffectSettings &settings,
      const float *const *inBlock, float *const *outBlock, size_t blockLen)
      override;

   bool RealtimeInitialize(EffectSettings &settings) override;
   bool RealtimeAddProcessor(EffectSettings &settings,
      unsigned numChannels, float sampleRate) override;
   bool RealtimeFinalize(EffectSettings &settings) noexcept override;
   bool RealtimeSuspend() override;
   bool RealtimeResume() noexcept override;
   bool RealtimeProcessStart(EffectSettings &settings) override;
   size_t RealtimeProcess(int group,  EffectSettings &settings,
      const float *const *inbuf, float *const *outbuf, size_t numSamples)
      override;
   bool RealtimeProcessEnd(EffectSettings &settings) noexcept override;

   int ShowClientInterface(
      wxWindow &parent, wxDialog &dialog, bool forceModal) override;

   bool MakeListener();
   bool CreateAudioUnit();
   bool InitializeInstance();
   bool InitializePlugin();

   // EffectUIClientInterface implementation

   std::shared_ptr<EffectInstance> MakeInstance(EffectSettings &settings)
      const override;
   std::shared_ptr<EffectInstance> DoMakeInstance(EffectSettings &settings);
   std::unique_ptr<EffectUIValidator> PopulateUI(
      ShuttleGui &S, EffectInstance &instance, EffectSettingsAccess &access)
   override;
   bool IsGraphicalUI() override;
   bool ValidateUI(EffectSettings &) override;
   bool CloseUI() override;

   bool CanExportPresets() override;
   void ExportPresets(const EffectSettings &settings) const override;
   void ImportPresets(EffectSettings &settings) override;

   bool HasOptions() override;
   void ShowOptions() override;

   // AudioUnitEffect implementation
   
private:
   class ParameterInfo;
   //! Return value: if true, continue visiting
   using ParameterVisitor =
      std::function< bool(const ParameterInfo &pi, AudioUnitParameterID ID) >;
   //! @return false if parameters could not be retrieved at all, else true
   bool ForEachParameter(ParameterVisitor visitor) const;

   bool SetRateAndChannels();

   bool CopyParameters(AudioUnit srcUnit, AudioUnit dstUnit);

   //! Obtain dump of the setting state of an AudioUnit instance
   /*!
    @param binary if false, then produce XML serialization instead; but
    AudioUnits does not need to be told the format again to reinterpret the blob
    @return smart pointer to data, and an error message
    */
   std::pair<CF_ptr<CFDataRef>, TranslatableString>
   MakeBlob(const wxCFStringRef &cfname, bool binary) const;

   TranslatableString Export(const wxString & path) const;
   TranslatableString Import(
      AudioUnitEffectSettings &settings, const wxString & path) const;
   /*!
    @param path only for formatting error messages
    @return error message
    */
   TranslatableString SaveBlobToConfig(const RegistryPath &group,
      const wxString &path, const void *blob, size_t len,
      bool allowEmpty = true) const;
   void Notify(AudioUnit unit, AudioUnitParameterID parm) const;

   static OSStatus RenderCallback(void *inRefCon,
                                  AudioUnitRenderActionFlags *inActionFlags,
                                  const AudioTimeStamp *inTimeStamp,
                                  UInt32 inBusNumber,
                                  UInt32 inNumFrames,
                                  AudioBufferList *ioData);
   OSStatus Render(AudioUnitRenderActionFlags *inActionFlags,
                   const AudioTimeStamp *inTimeStamp,
                   UInt32 inBusNumber,
                   UInt32 inNumFrames,
                   AudioBufferList *ioData);

   static void EventListenerCallback(void *inCallbackRefCon,
                                     void *inObject,
                                     const AudioUnitEvent *inEvent,
                                     UInt64 inEventHostTime,
                                     AudioUnitParameterValue inParameterValue);
   void EventListener(const AudioUnitEvent *inEvent,
                      AudioUnitParameterValue inParameterValue);

   void GetChannelCounts();

   bool LoadPreset(const RegistryPath & group, EffectSettings &settings) const;

   //! Interpret the dump made before by MakeBlob
   /*!
    @param group only for formatting error messages
    @return an error message
    */
   TranslatableString InterpretBlob(AudioUnitEffectSettings &settings,
      const wxString &group, const wxMemoryBuffer &buf) const;

   bool SavePreset(const RegistryPath & group) const;

#if defined(HAVE_AUDIOUNIT_BASIC_SUPPORT)
   bool CreatePlain(wxWindow *parent);
#endif

   bool BypassEffect(bool bypass);

private:
   // Supply most often used values as defaults for scope and element
   template<typename T>
   OSStatus GetFixedSizeProperty(AudioUnitPropertyID inID, T &property,
      AudioUnitScope inScope = kAudioUnitScope_Global,
      AudioUnitElement inElement = 0) const
   {
      // Supply mUnit.get() to the non-member function
      return AudioUnitUtils::GetFixedSizeProperty(mUnit.get(),
         inID, property, inScope, inElement);
   }

   // Supply most often used values as defaults for scope and element
   template<typename T>
   OSStatus GetVariableSizeProperty(AudioUnitPropertyID inID,
      PackedArrayPtr<T> &pObject,
      AudioUnitScope inScope = kAudioUnitScope_Global,
      AudioUnitElement inElement = 0) const
   {
      return AudioUnitUtils::GetVariableSizeProperty(mUnit.get(),
         inID, pObject, inScope, inElement);
   }

   template<typename T>
   OSStatus SetProperty(AudioUnitPropertyID inID, const T &property,
      AudioUnitScope inScope = kAudioUnitScope_Global,
      AudioUnitElement inElement = 0) const
   {
      // Supply mUnit.get() to the non-member function
      return AudioUnitUtils::SetProperty(mUnit.get(),
         inID, property, inScope, inElement);
   }
   
   AudioUnitEffectSettings mSettings;
   //! This function will be rewritten when the effect is really stateless
   AudioUnitEffectSettings &GetSettings(EffectSettings &) const
      { return const_cast<AudioUnitEffect*>(this)->mSettings; }
   //! This function will be rewritten when the effect is really stateless
   const AudioUnitEffectSettings &GetSettings(const EffectSettings &) const
      { return mSettings; }

   const PluginPath mPath;
   const wxString mName;
   const wxString mVendor;
   const AudioComponent mComponent;

   // The sequence of these three members matters in the destructor
   AudioUnitCleanup<AudioUnit, AudioComponentInstanceDispose> mUnit;
   AudioUnitCleanup<AUEventListenerRef, AUListenerDispose> mEventListenerRef;
   AudioUnitCleanup<AudioUnit, AudioUnitUninitialize> mInitialization;

   // Initialized in GetChannelCounts()
   unsigned mAudioIns{ 2 };
   unsigned mAudioOuts{ 2 };

   bool mInteractive{ false };
   bool mLatencyDone{ false };
   UInt32 mBlockSize{ 0 };
   Float64 mSampleRate{ 44100.0 };

   bool mUseLatency{ true };

   AudioTimeStamp mTimeStamp{};

   PackedArrayPtr<AudioBufferList> mInputList;
   PackedArrayPtr<AudioBufferList> mOutputList;

   wxWindow *mParent{};
   wxWeakRef<wxDialog> mDialog;
   wxString mUIType; // NOT translated, "Full", "Generic", or "Basic"
   bool mIsGraphical{ false };

   AudioUnitEffect *const mMaster;     // non-NULL if a slave
   AudioUnitEffectArray mSlaves;

   AUControl *mpControl{};

   friend class AudioUnitEffectExportDialog;
   friend class AudioUnitEffectImportDialog;
};

///////////////////////////////////////////////////////////////////////////////
//
// AudioUnitEffectsModule
//
///////////////////////////////////////////////////////////////////////////////

class AudioUnitEffectsModule final : public PluginProvider
{
public:
   AudioUnitEffectsModule();
   virtual ~AudioUnitEffectsModule();

   // ComponentInterface implementation

   PluginPath GetPath() const override;
   ComponentInterfaceSymbol GetSymbol() const override;
   VendorSymbol GetVendor() const override;
   wxString GetVersion() const override;
   TranslatableString GetDescription() const override;

   // PluginProvider implementation

   bool Initialize() override;
   void Terminate() override;
   EffectFamilySymbol GetOptionalFamilySymbol() override;

   const FileExtensions &GetFileExtensions() override;
   FilePath InstallPath() override { return {}; }

   void AutoRegisterPlugins(PluginManagerInterface & pm) override;
   PluginPaths FindModulePaths(PluginManagerInterface & pm) override;
   unsigned DiscoverPluginsAtPath(
      const PluginPath & path, TranslatableString &errMsg,
      const RegistrationCallback &callback)
         override;

   bool IsPluginValid(const PluginPath & path, bool bFast) override;

   std::unique_ptr<ComponentInterface>
      LoadPlugin(const PluginPath & path) override;

   // AudioUnitEffectModule implementation

   void LoadAudioUnitsOfType(OSType inAUType, PluginPaths & effects);
   AudioComponent FindAudioUnit(const PluginPath & path, wxString & name);

   wxString FromOSType(OSType type);
   OSType ToOSType(const wxString & type);
};

#endif

#endif
