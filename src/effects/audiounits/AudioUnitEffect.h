/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioUnitEffect.h

  Dominic Mazzoni
  Leland Lucius

**********************************************************************/

#include <wx/dialog.h>

#include "../Effect.h"

#include <ApplicationServices/ApplicationServices.h>
#include <CoreServices/CoreServices.h>
#include <Carbon/Carbon.h>
#include <AudioUnit/AudioUnitProperties.h>
#include <AudioUnit/AudioUnitCarbonView.h>
#include <AudioToolbox/AudioUnitUtilities.h>

#include "audacity/EffectInterface.h"
#include "audacity/ModuleInterface.h"
#include "audacity/PluginInterface.h"

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
                   Component component,
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

   static pascal OSStatus WindowEventHandlerCallback(EventHandlerCallRef handler,
                                                     EventRef event,
                                                     void *data);
   OSStatus WindowEventHandler(EventRef event);

   static pascal OSStatus ControlEventHandlerCallback(EventHandlerCallRef handler,
                                                      EventRef event,
                                                      void *data);
   OSStatus ControlEventHandler(EventRef event);

   static pascal OSStatus TrackingEventHandler(EventHandlerCallRef handler,
                                               EventRef event,
                                               void *data);
   OSStatus OnTrackingEvent(EventRef event);

   void RemoveHandler();

   void GetChannelCounts();

   bool LoadParameters(const wxString & group);
   bool SaveParameters(const wxString & group);

   void OnSize(wxSizeEvent & evt);

private:

   wxString    mPath;
   wxString    mName;
   wxString    mVendor;
   Component   mComponent;
   AudioUnit   mUnit;
   bool        mSupportsMono;
   bool        mSupportsStereo;

   EffectHostInterface *mHost;
   int         mAudioIns;
   int         mAudioOuts;
   bool        mInteractive;
   bool        mLatencyDone;
   UInt32      mBlockSize;
   double      mSampleRate;

   int         mBufferSize;
   bool        mUseLatency;

   AudioTimeStamp mTimeStamp;
   bool        mReady;

   AudioBufferList *mInputList;
   AudioBufferList *mOutputList;

   EffectUIHostInterface *mUIHost;
   wxWindow *mParent;
   wxDialog *mDialog;
   wxSizerItem *mContainer;
   AudioUnitCarbonView mCarbonView;
   bool mUseGUI;
   HIViewRef mAUView;
   EventTargetRef mEventRef;
   bool mIsCocoa;
   bool mIsCarbon;
   bool mIsGeneric;

   AudioUnitEffect *mMaster;     // non-NULL if a slave
   AudioUnitEffectArray mSlaves;
   int mNumChannels;
   float **mMasterIn;
   float **mMasterOut;
   sampleCount mNumSamples;
   
   AUEventListenerRef mEventListenerRef;

   EventHandlerRef mHandlerRef;
   EventHandlerUPP mHandlerUPP;
   EventHandlerRef mControlHandlerRef;
   EventHandlerUPP mControlHandlerUPP;

   EventHandlerUPP mTrackingHandlerUPP;
   EventHandlerRef mRootTrackingHandlerRef;
   EventHandlerRef mContentTrackingHandlerRef;
   EventHandlerRef mAUTrackingHandlerRef;

   DECLARE_EVENT_TABLE();

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
   Component FindAudioUnit(const wxString & path, wxString & name);

   wxString FromOSType(OSType type);
   OSType ToOSType(const wxString & type);

private:
   ModuleManagerInterface *mModMan;
   wxString mPath;
};

