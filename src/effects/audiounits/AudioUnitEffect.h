/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioUnitEffect.h

  Dominic Mazzoni
  Leland Lucius

**********************************************************************/
#ifndef AUDACITY_AUDIOUNIT_EFFECT_H

#include "../../Audacity.h"

#if USE_AUDIO_UNITS

#include <wx/dialog.h>

#include <AudioToolbox/AudioUnitUtilities.h>
#include <AudioUnit/AudioUnit.h>
#include <AudioUnit/AudioUnitProperties.h>

#include "audacity/EffectInterface.h"
#include "audacity/ModuleInterface.h"
#include "audacity/PluginInterface.h"

#include "AUControl.h"

#define AUDIOUNITEFFECTS_VERSION wxT("1.0.0.0")
#define AUDIOUNITEFFECTS_FAMILY wxT("AudioUnit")

class AudioUnitEffect;

WX_DEFINE_ARRAY_PTR(AudioUnitEffect *, AudioUnitEffectArray);

class AudioUnitEffectExportDialog;
class AudioUnitEffectImportDialog;

class AudioUnitEffect : public wxEvtHandler,
                        public EffectClientInterface,
                        public EffectUIClientInterface
{
public:
   AudioUnitEffect(const wxString & path,
                   const wxString & name,
                   AudioComponent component,
                   AudioUnitEffect *master = NULL);
   virtual ~AudioUnitEffect();

   // IdentInterface implementation

   virtual wxString GetPath();
   virtual wxString GetSymbol();
   virtual wxString GetName();
   virtual wxString GetVendor();
   virtual wxString GetVersion();
   virtual wxString GetDescription();

   // EffectIdentInterface implementation

   virtual EffectType GetType();
   virtual wxString GetFamily();
   virtual bool IsInteractive();
   virtual bool IsDefault();
   virtual bool IsLegacy();
   virtual bool SupportsRealtime();
   virtual bool SupportsAutomation();

   // EffectClientInterface implementation

   virtual bool SetHost(EffectHostInterface *host);

   virtual int GetAudioInCount();
   virtual int GetAudioOutCount();

   virtual int GetMidiInCount();
   virtual int GetMidiOutCount();

   virtual void SetSampleRate(sampleCount rate);
   virtual sampleCount SetBlockSize(sampleCount maxBlockSize);

   virtual sampleCount GetLatency();
   virtual sampleCount GetTailSize();

   virtual bool IsReady();
   virtual bool ProcessInitialize(sampleCount totalLen, ChannelNames chanMap = NULL);
   virtual bool ProcessFinalize();
   virtual sampleCount ProcessBlock(float **inBlock, float **outBlock, sampleCount blockLen);

   virtual bool RealtimeInitialize();
   virtual bool RealtimeAddProcessor(int numChannels, float sampleRate);
   virtual bool RealtimeFinalize();
   virtual bool RealtimeSuspend();
   virtual bool RealtimeResume();
   virtual bool RealtimeProcessStart();
   virtual sampleCount RealtimeProcess(int group,
                                       float **inbuf,
                                       float **outbuf,
                                       sampleCount numSamples);
   virtual bool RealtimeProcessEnd();

   virtual bool ShowInterface(wxWindow *parent, bool forceModal = false);

   virtual bool GetAutomationParameters(EffectAutomationParameters & parms);
   virtual bool SetAutomationParameters(EffectAutomationParameters & parms);

   virtual bool LoadUserPreset(const wxString & name);
   virtual bool SaveUserPreset(const wxString & name);

   virtual bool LoadFactoryPreset(int id);
   virtual bool LoadFactoryDefaults();
   virtual wxArrayString GetFactoryPresets();

   // EffectUIClientInterface implementation

   virtual void SetHostUI(EffectUIHostInterface *host);
   virtual bool PopulateUI(wxWindow *parent);
   virtual bool IsGraphicalUI();
   virtual bool ValidateUI();
   virtual bool HideUI();
   virtual bool CloseUI();

   virtual bool CanExportPresets();
   virtual void ExportPresets();
   virtual void ImportPresets();

   virtual bool HasOptions();
   virtual void ShowOptions();

   // AudioUnitEffect implementation
   
private:
   bool SetRateAndChannels();

   bool CopyParameters(AudioUnit srcUnit, AudioUnit dstUnit);

   // Realtime
   int GetChannelCount();
   void SetChannelCount(int numChannels);
   
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

   bool LoadParameters(const wxString & group);
   bool SaveParameters(const wxString & group);


   bool CreatePlain(wxWindow *parent);

private:

   wxString mPath;
   wxString mName;
   wxString mVendor;
   AudioComponent mComponent;
   AudioUnit mUnit;
   bool mUnitInitialized;

   bool mSupportsMono;
   bool mSupportsStereo;

   EffectHostInterface *mHost;
   int mAudioIns;
   int mAudioOuts;
   bool mInteractive;
   bool mLatencyDone;
   UInt32 mBlockSize;
   double mSampleRate;

   int mBufferSize;
   bool mUseLatency;

   AudioTimeStamp mTimeStamp;
   bool mReady;

   AudioBufferList *mInputList;
   AudioBufferList *mOutputList;

   EffectUIHostInterface *mUIHost;
   wxWindow *mParent;
   wxDialog *mDialog;
   AUControl *mControl;
   wxString mUIType;
   bool mIsGraphical;

   AudioUnitEffect *mMaster;     // non-NULL if a slave
   AudioUnitEffectArray mSlaves;
   int mNumChannels;
   float **mMasterIn;
   float **mMasterOut;
   sampleCount mNumSamples;
   
   AUEventListenerRef mEventListenerRef;

   friend class AudioUnitEffectExportDialog;
   friend class AudioUnitEffectImportDialog;
};

///////////////////////////////////////////////////////////////////////////////
//
// AudioUnitEffectsModule
//
///////////////////////////////////////////////////////////////////////////////

class AudioUnitEffectsModule : public ModuleInterface
{
public:
   AudioUnitEffectsModule(ModuleManagerInterface *moduleManager, const wxString *path);
   virtual ~AudioUnitEffectsModule();

   // IdentInterface implementatino

   virtual wxString GetPath();
   virtual wxString GetSymbol();
   virtual wxString GetName();
   virtual wxString GetVendor();
   virtual wxString GetVersion();
   virtual wxString GetDescription();

   // ModuleInterface implementation

   virtual bool Initialize();
   virtual void Terminate();

   virtual bool AutoRegisterPlugins(PluginManagerInterface & pm);
   virtual wxArrayString FindPlugins(PluginManagerInterface & pm);
   virtual bool RegisterPlugin(PluginManagerInterface & pm, const wxString & path);

   virtual bool IsPluginValid(const wxString & path);

   virtual IdentInterface *CreateInstance(const wxString & path);
   virtual void DeleteInstance(IdentInterface *instance);

   // AudioUnitEffectModule implementation

   void LoadAudioUnitsOfType(OSType inAUType, wxArrayString & effects);
   AudioComponent FindAudioUnit(const wxString & path, wxString & name);

   wxString FromOSType(OSType type);
   OSType ToOSType(const wxString & type);

private:
   ModuleManagerInterface *mModMan;
   wxString mPath;
};

#endif

#endif
