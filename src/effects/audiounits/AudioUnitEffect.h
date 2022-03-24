/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioUnitEffect.h

  Dominic Mazzoni
  Leland Lucius

**********************************************************************/
#ifndef AUDACITY_AUDIOUNIT_EFFECT_H



#if USE_AUDIO_UNITS

#include "MemoryX.h"
#include <vector>

#include <AudioToolbox/AudioUnitUtilities.h>
#include <AudioUnit/AudioUnit.h>
#include <AudioUnit/AudioUnitProperties.h>

#include "EffectInterface.h"
#include "PluginProvider.h"
#include "PluginInterface.h"

#include "AUControl.h"
#include <wx/weakref.h>

#define AUDIOUNITEFFECTS_VERSION wxT("1.0.0.0")
/* i18n-hint: the name of an Apple audio software protocol */
#define AUDIOUNITEFFECTS_FAMILY EffectFamilySymbol{ wxT("AudioUnit"), XO("Audio Unit") }
class AudioUnitEffect;

using AudioUnitEffectArray = std::vector<std::unique_ptr<AudioUnitEffect>>;

class AudioUnitEffectExportDialog;
class AudioUnitEffectImportDialog;

class AudioUnitEffect : public wxEvtHandler,
                        public EffectUIClientInterface
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

   bool SaveSettings(
      const EffectSettings &settings, CommandParameters & parms) const override;
   bool LoadSettings(
      const CommandParameters & parms, Settings &settings) const override;

   bool LoadUserPreset(
      const RegistryPath & name, Settings &settings) const override;
   bool SaveUserPreset(
      const RegistryPath & name, const Settings &settings) const override;

   RegistryPaths GetFactoryPresets() const override;
   bool LoadFactoryPreset(int id, EffectSettings &settings) const override;
   bool LoadFactoryDefaults(EffectSettings &settings) const override;

   // EffectProcessor implementation

   unsigned GetAudioInCount() const override;
   unsigned GetAudioOutCount() const override;

   int GetMidiInCount() override;
   int GetMidiOutCount() override;

   void SetSampleRate(double rate) override;
   size_t SetBlockSize(size_t maxBlockSize) override;
   size_t GetBlockSize() const override;

   sampleCount GetLatency() override;
   size_t GetTailSize() override;

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
   bool InitializePlugin();

   // EffectUIClientInterface implementation

   bool InitializeInstance(
      EffectHostInterface *host, EffectSettings &settings) override;
   std::unique_ptr<EffectUIValidator> PopulateUI(
      ShuttleGui &S, EffectSettingsAccess &access) override;
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
   bool SetRateAndChannels();

   bool CopyParameters(AudioUnit srcUnit, AudioUnit dstUnit);
   TranslatableString Export(const wxString & path) const;
   TranslatableString Import(const wxString & path);
   void Notify(AudioUnit unit, AudioUnitParameterID parm) const;

   // Realtime
   unsigned GetChannelCount();
   void SetChannelCount(unsigned numChannels);
   
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

   bool LoadPreset(const RegistryPath & group, EffectSettings &settings);
   bool SavePreset(const RegistryPath & group) const;

#if defined(HAVE_AUDIOUNIT_BASIC_SUPPORT)
   bool CreatePlain(wxWindow *parent);
#endif

   bool BypassEffect(bool bypass);

private:

   PluginPath mPath;
   wxString mName;
   wxString mVendor;
   AudioComponent mComponent;
   AudioUnit mUnit;
   bool mUnitInitialized;

   bool mSupportsMono;
   bool mSupportsStereo;

   EffectHostInterface *mHost{};
   unsigned mAudioIns;
   unsigned mAudioOuts;
   bool mInteractive;
   bool mLatencyDone;
   UInt32 mBlockSize;
   double mSampleRate;

   int mBufferSize;
   bool mUseLatency;

   AudioTimeStamp mTimeStamp;

   ArrayOf<AudioBufferList> mInputList;
   ArrayOf<AudioBufferList> mOutputList;

   wxWindow *mParent;
   wxWeakRef<wxDialog> mDialog;
   wxString mUIType; // NOT translated, "Full", "Generic", or "Basic"
   bool mIsGraphical;

   AudioUnitEffect *mMaster;     // non-NULL if a slave
   AudioUnitEffectArray mSlaves;
   unsigned mNumChannels;

   AUEventListenerRef mEventListenerRef;

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
