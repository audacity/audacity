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
#include "ModuleInterface.h"
#include "PluginInterface.h"

#include "AUControl.h"

#define AUDIOUNITEFFECTS_VERSION wxT("1.0.0.0")
/* i18n-hint: the name of an Apple audio software protocol */
#define AUDIOUNITEFFECTS_FAMILY EffectFamilySymbol{ wxT("AudioUnit"), XO("Audio Unit") }
class AudioUnitEffect;

using AudioUnitEffectArray = std::vector<std::unique_ptr<AudioUnitEffect>>;

class AudioUnitEffectExportDialog;
class AudioUnitEffectImportDialog;

class AudioUnitEffect : public wxEvtHandler,
                        public EffectClientInterface,
                        public EffectUIClientInterface
{
public:
   AudioUnitEffect(const PluginPath & path,
                   const wxString & name,
                   AudioComponent component,
                   AudioUnitEffect *master = NULL);
   virtual ~AudioUnitEffect();

   // ComponentInterface implementation

   PluginPath GetPath() override;
   ComponentInterfaceSymbol GetSymbol() override;
   VendorSymbol GetVendor() override;
   wxString GetVersion() override;
   TranslatableString GetDescription() override;

   // EffectComponentInterface implementation

   EffectType GetType() override;
   EffectFamilySymbol GetFamily() override;
   bool IsInteractive() override;
   bool IsDefault() override;
   bool IsLegacy() override;
   bool SupportsRealtime() override;
   bool SupportsAutomation() override;

   // EffectClientInterface implementation

   bool SetHost(EffectHostInterface *host) override;

   unsigned GetAudioInCount() override;
   unsigned GetAudioOutCount() override;

   int GetMidiInCount() override;
   int GetMidiOutCount() override;

   void SetSampleRate(double rate) override;
   size_t SetBlockSize(size_t maxBlockSize) override;
   size_t GetBlockSize() const override;

   sampleCount GetLatency() override;
   size_t GetTailSize() override;

   bool IsReady() override;
   bool ProcessInitialize(sampleCount totalLen, ChannelNames chanMap = NULL) override;
   bool ProcessFinalize() override;
   size_t ProcessBlock(float **inBlock, float **outBlock, size_t blockLen) override;

   bool RealtimeInitialize() override;
   bool RealtimeAddProcessor(unsigned numChannels, float sampleRate) override;
   bool RealtimeFinalize() override;
   bool RealtimeSuspend() override;
   bool RealtimeResume() override;
   bool RealtimeProcessStart() override;
   size_t RealtimeProcess(int group,
                                       float **inbuf,
                                       float **outbuf,
                                       size_t numSamples) override;
   bool RealtimeProcessEnd() override;

   bool ShowInterface( wxWindow &parent,
      const EffectDialogFactory &factory, bool forceModal = false) override;

   bool GetAutomationParameters(CommandParameters & parms) override;
   bool SetAutomationParameters(CommandParameters & parms) override;

   bool LoadUserPreset(const RegistryPath & name) override;
   bool SaveUserPreset(const RegistryPath & name) override;

   bool LoadFactoryPreset(int id) override;
   bool LoadFactoryDefaults() override;
   RegistryPaths GetFactoryPresets() override;

   // EffectUIClientInterface implementation

   void SetHostUI(EffectUIHostInterface *host) override;
   bool PopulateUI(ShuttleGui &S) override;
   bool IsGraphicalUI() override;
   bool ValidateUI() override;
   bool HideUI() override;
   bool CloseUI() override;

   bool CanExportPresets() override;
   void ExportPresets() override;
   void ImportPresets() override;

   bool HasOptions() override;
   void ShowOptions() override;

   // AudioUnitEffect implementation
   
private:
   bool SetRateAndChannels();

   bool CopyParameters(AudioUnit srcUnit, AudioUnit dstUnit);
   TranslatableString Export(const wxString & path);
   TranslatableString Import(const wxString & path);
   void Notify(AudioUnit unit, AudioUnitParameterID parm);

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

   bool LoadPreset(const RegistryPath & group);
   bool SavePreset(const RegistryPath & group);

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

   EffectHostInterface *mHost;
   unsigned mAudioIns;
   unsigned mAudioOuts;
   bool mInteractive;
   bool mLatencyDone;
   UInt32 mBlockSize;
   double mSampleRate;

   int mBufferSize;
   bool mUseLatency;

   AudioTimeStamp mTimeStamp;
   bool mReady;

   ArrayOf<AudioBufferList> mInputList;
   ArrayOf<AudioBufferList> mOutputList;

   EffectUIHostInterface *mUIHost;
   wxWindow *mParent;
   wxDialog *mDialog;
   wxString mUIType; // NOT translated, "Full", "Generic", or "Basic"
   bool mIsGraphical;

   AudioUnitEffect *mMaster;     // non-NULL if a slave
   AudioUnitEffectArray mSlaves;
   unsigned mNumChannels;
   ArraysOf<float> mMasterIn, mMasterOut;
   size_t mNumSamples;

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

class AudioUnitEffectsModule final : public ModuleInterface
{
public:
   AudioUnitEffectsModule();
   virtual ~AudioUnitEffectsModule();

   // ComponentInterface implementation

   PluginPath GetPath() override;
   ComponentInterfaceSymbol GetSymbol() override;
   VendorSymbol GetVendor() override;
   wxString GetVersion() override;
   TranslatableString GetDescription() override;

   // ModuleInterface implementation

   bool Initialize() override;
   void Terminate() override;
   EffectFamilySymbol GetOptionalFamilySymbol() override;

   const FileExtensions &GetFileExtensions() override;
   FilePath InstallPath() override { return {}; }

   bool AutoRegisterPlugins(PluginManagerInterface & pm) override;
   PluginPaths FindPluginPaths(PluginManagerInterface & pm) override;
   unsigned DiscoverPluginsAtPath(
      const PluginPath & path, TranslatableString &errMsg,
      const RegistrationCallback &callback)
         override;

   bool IsPluginValid(const PluginPath & path, bool bFast) override;

   std::unique_ptr<ComponentInterface>
      CreateInstance(const PluginPath & path) override;

   // AudioUnitEffectModule implementation

   void LoadAudioUnitsOfType(OSType inAUType, PluginPaths & effects);
   AudioComponent FindAudioUnit(const PluginPath & path, wxString & name);

   wxString FromOSType(OSType type);
   OSType ToOSType(const wxString & type);
};

#endif

#endif
