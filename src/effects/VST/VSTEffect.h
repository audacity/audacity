/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTEffect.h

  Dominic Mazzoni

**********************************************************************/

#if USE_VST

#include "audacity/EffectInterface.h"
#include "audacity/ModuleInterface.h"
#include "audacity/PluginInterface.h"

#include <wx/wx.h>

#include "aeffectx.h"

#define VSTCMDKEY L"-checkvst"
#define VSTPLUGINTYPE L"VST"

#define audacityVSTID CCONST('a', 'u', 'D', 'y');

typedef intptr_t (*dispatcherFn)(AEffect * effect,
                                 int opCode,
                                 int index,
                                 intptr_t value,
                                 void *ptr,
                                 float opt);

typedef void (*processFn)(AEffect * effect,
                          float **inputs,
                          float **outputs,
                          int sampleframes);

typedef void (*setParameterFn)(AEffect * effect,
                               int index,
                               float parameter);

typedef float (*getParameterFn)(AEffect * effect,
                                int index);

typedef AEffect *(*vstPluginMain)(audioMasterCallback audioMaster);

class VSTEffectTimer;
class VSTEffectDialog;
class VSTEffect;

WX_DEFINE_ARRAY_PTR(VSTEffect *, VSTEffectArray);

class VSTEffect : public EffectClientInterface
{
 public:
   VSTEffect(const wxString & path, VSTEffect *master = NULL);
   virtual ~VSTEffect();

   // IdentInterface implementation
   virtual PluginID GetID();
   virtual wxString GetPath();
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
   virtual bool IsRealtimeCapable();

   // EffectClientInterface implementation
   virtual void SetHost(EffectHostInterface *host);
   virtual bool Startup();
   virtual bool Shutdown();

   virtual int GetAudioInCount();
   virtual int GetAudioOutCount();

   virtual int GetMidiInCount();
   virtual int GetMidiOutCount();

   virtual sampleCount GetLatency();
   virtual sampleCount GetTailSize();

   virtual void SetSampleRate(sampleCount rate);
   virtual sampleCount GetBlockSize(sampleCount maxBlockSize);

   virtual bool IsReady();
   virtual bool ProcessInitialize();
   virtual bool ProcessFinalize();
   virtual sampleCount ProcessBlock(float **inbuf, float **outbuf, sampleCount size);

   virtual bool RealtimeInitialize();
   virtual bool RealtimeAddProcessor(int numChannels, float sampleRate);
   virtual bool RealtimeFinalize();
   virtual bool RealtimeSuspend();
   virtual bool RealtimeResume();
   virtual sampleCount RealtimeProcess(int index, float **inbuf, float **outbuf, sampleCount size);

   virtual bool ShowInterface(void *parent);

   // VSTEffect implementation

   // VST plugin -> host callback
   static intptr_t AudioMaster(AEffect *effect,
                               int32_t opcode,
                               int32_t index,
                               intptr_t value,
                               void * ptr,
                               float opt);

   void OnTimer();

private:
   // Plugin loading and unloading
   bool Load();
   void Unload();

   // Parameter loading and saving
   void LoadParameters(const wxString & group);
   void SaveParameters(const wxString & group);

   // Base64 encoding and decoding
   static wxString b64encode(const void *in, int len);
   static int b64decode(wxString in, void *out);

   // Realtime
   bool IsSlave();


   // Utility methods

   VstTimeInfo *GetTimeInfo();
   float GetSampleRate();
   int GetProcessLevel();
   void SetBufferDelay(int samples);
   void NeedIdle();
   void NeedEditIdle(bool state);
   void SizeWindow(int w, int h);
   void UpdateDisplay();
   void Automate(int index, float value);
   void PowerOn();
   void PowerOff();
   void InterfaceClosed();

   int GetString(wxString & outstr, int opcode, int index = 0);
   wxString GetString(int opcode, int index = 0);
   void SetString(int opcode, const wxString & str, int index = 0);

   // VST methods

   intptr_t callDispatcher(int opcode, int index, intptr_t value, void *ptr, float opt);
   void callProcessReplacing(float **inputs, float **outputs, int sampleframes);
   void callSetParameter(int index, float value);
   float callGetParameter(int index);
   void callSetProgram(int index);

 private:
   EffectHostInterface *mHost;
   PluginID mID;
   wxString mPath;
   int mAudioIns;
   int mAudioOuts;
   int mMidiIns;
   int mMidiOuts;
   float mSampleRate;
   sampleCount mUserBlockSize;
   wxString mName;
   wxString mVendor;
   wxString mDescription;
   int mVersion;
   bool mInteractive;

   bool mReady;

#if defined(__WXMAC__)
   void *mBundleRef;       // Cheating a little ... type is really CFBundle
   int mResource;          // Cheating a little ... type is really CFBundle
#endif
   void *mModule;
   AEffect *mAEffect;

   VSTEffectDialog *mDlg;

   VstTimeInfo mTimeInfo;

   bool mUseBufferDelay;
   int mBufferDelay;

   sampleCount mBlockSize;

   int mProcessLevel;
   bool mHasPower;
   bool mWantsIdle;
   bool mWantsEditIdle;

   wxCRIT_SECT_DECLARE_MEMBER(mDispatcherLock);

   VSTEffectTimer *mTimer;
   int mTimerGuard;

   // Realtime processing
   VSTEffect *mMaster;     // non-NULL if a slave
   VSTEffectArray mSlaves;

   friend class VSTEffectDialog;
   friend class VSTEffectsModule;
};

void RegisterVSTEffects();

class VSTEffectsModule : public ModuleInterface
{
public:
   VSTEffectsModule(ModuleManagerInterface *moduleManager, const wxString *path);
   virtual ~VSTEffectsModule();

   // IdentInterface implementatino

   virtual wxString GetID();
   virtual wxString GetPath();
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

   virtual void *CreateInstance(const PluginID & ID, const wxString & path);

   // VSTEffectModule implementation

   static void Check(const wxChar *path);

private:
   ModuleManagerInterface *mModMan;
   wxString mPath;
};

#endif // USE_VST
