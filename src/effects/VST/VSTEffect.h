/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTEffect.h

  Dominic Mazzoni

**********************************************************************/

#if USE_VST

#include <wx/wx.h>

#include "audacity/EffectInterface.h"
#include "audacity/ModuleInterface.h"
#include "audacity/PluginInterface.h"

#include "../../widgets/NumericTextCtrl.h"

#include "aeffectx.h"

#define VSTCMDKEY wxT("-checkvst")
#define VSTPLUGINTYPE wxT("VST")

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

///////////////////////////////////////////////////////////////////////////////
//
// VSTEffect
//
///////////////////////////////////////////////////////////////////////////////

WX_DEFINE_ARRAY_PTR(VSTEffect *, VSTEffectArray);

DECLARE_LOCAL_EVENT_TYPE(EVT_SIZEWINDOW, -1);
DECLARE_LOCAL_EVENT_TYPE(EVT_UPDATEDISPLAY, -1);

class VSTEffect : public wxEvtHandler, 
                  public EffectClientInterface,
                  public EffectUIClientInterface,
                  public XMLTagHandler
{
 public:
   VSTEffect(const wxString & path, VSTEffect *master = NULL);
   virtual ~VSTEffect();

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

   virtual sampleCount GetLatency();
   virtual sampleCount GetTailSize();

   virtual void SetSampleRate(sampleCount rate);
   virtual sampleCount SetBlockSize(sampleCount maxBlockSize);

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

   virtual wxArrayString GetFactoryPresets();
   virtual bool LoadFactoryPreset(int id);
   virtual bool LoadFactoryDefaults();

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
   wxArrayInt GetEffectIDs();

   // Parameter loading and saving
   bool LoadParameters(const wxString & group);
   bool SaveParameters(const wxString & group);

   // Base64 encoding and decoding
   static wxString b64encode(const void *in, int len);
   static int b64decode(wxString in, void *out);

   // Realtime
   int GetChannelCount();
   void SetChannelCount(int numChannels);

   // UI
   void OnSlider(wxCommandEvent & evt);
   void OnSizeWindow(wxCommandEvent & evt);
   void OnUpdateDisplay(wxCommandEvent & evt);

   void RemoveHandler();

   void OnProgram(wxCommandEvent & evt);
   void OnProgramText(wxCommandEvent & evt);
   void OnLoad(wxCommandEvent & evt);
   void OnSave(wxCommandEvent & evt);
   void OnSettings(wxCommandEvent & evt);

   void BuildPlain();
   void BuildFancy();
   wxSizer *BuildProgramBar();
   void RefreshParameters(int skip = -1);

   // Program/Bank loading/saving
   bool LoadFXB(const wxFileName & fn);
   bool LoadFXP(const wxFileName & fn);
   bool LoadXML(const wxFileName & fn);
   bool LoadFXProgram(unsigned char **bptr, ssize_t & len, int index, bool dryrun);
   void SaveFXB(const wxFileName & fn);
   void SaveFXP(const wxFileName & fn);
   void SaveXML(const wxFileName & fn);
   void SaveFXProgram(wxMemoryBuffer & buf, int index);

   virtual bool HandleXMLTag(const wxChar *tag, const wxChar **attrs);
   virtual void HandleXMLEndTag(const wxChar *tag);
   virtual void HandleXMLContent(const wxString & content);
   virtual XMLTagHandler *HandleXMLChild(const wxChar *tag);

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

   int GetString(wxString & outstr, int opcode, int index = 0);
   wxString GetString(int opcode, int index = 0);
   void SetString(int opcode, const wxString & str, int index = 0);

   // VST methods

   intptr_t callDispatcher(int opcode, int index, intptr_t value, void *ptr, float opt);
   void callProcessReplacing(float **inputs, float **outputs, int sampleframes);
   void callSetParameter(int index, float value);
   float callGetParameter(int index);
   void callSetProgram(int index);
   void callSetChunk(bool isPgm, int len, void *buf);
   void callSetChunk(bool isPgm, int len, void *buf, VstPatchChunkInfo *info);

 private:
   EffectHostInterface *mHost;
   PluginID mID;
   wxString mPath;
   int mAudioIns;
   int mAudioOuts;
   int mMidiIns;
   int mMidiOuts;
   bool mAutomatable;
   float mSampleRate;
   sampleCount mUserBlockSize;
   wxString mName;
   wxString mVendor;
   wxString mDescription;
   int mVersion;
   bool mInteractive;

   static intptr_t mCurrentEffectID;

   bool mReady;

#if defined(__WXMAC__)
   void *mBundleRef;       // Cheating a little ... type is really CFBundle
   int mResource;          // Cheating a little ... type is really CFBundle
#endif
   void *mModule;
   AEffect *mAEffect;

   VstTimeInfo mTimeInfo;

   bool mUseLatency;
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
   int mNumChannels;
   float **mMasterIn;
   float **mMasterOut;
   sampleCount mNumSamples;

   // UI
   wxDialog *mDialog;
   wxWindow *mParent;
   EffectUIHostInterface *mUIHost;
   wxSizerItem *mContainer;
   bool mGui;

   NumericTextCtrl *mDuration;
   wxStaticText **mNames;
   wxSlider **mSliders;
   wxStaticText **mDisplays;
   wxStaticText **mLabels;

   bool mInSet;
   bool mInChunk;
   wxString mChunk;
   long mXMLVersion;
   VstPatchChunkInfo mXMLInfo;

#if defined(__WXMAC__)
   static pascal OSStatus OverlayEventHandler(EventHandlerCallRef handler, EventRef event, void *data);
   OSStatus OnOverlayEvent(EventHandlerCallRef handler, EventRef event);
   static pascal OSStatus WindowEventHandler(EventHandlerCallRef handler, EventRef event, void *data);
   OSStatus OnWindowEvent(EventHandlerCallRef handler, EventRef event);
   static pascal OSStatus TrackingEventHandler(EventHandlerCallRef handler, EventRef event, void *data);
   OSStatus OnTrackingEvent(EventRef event);

   WindowRef mOverlayRef;
   EventHandlerUPP mOverlayEventHandlerUPP;
   EventHandlerRef mOverlayEventHandlerRef;

   WindowRef mWindowRef;
   WindowRef mPreviousRef;
   EventHandlerUPP mWindowEventHandlerUPP;
   EventHandlerRef mWindowEventHandlerRef;

   EventHandlerUPP mTrackingHandlerUPP;
   EventHandlerRef mRootTrackingHandlerRef;
   EventHandlerRef mViewTrackingHandlerRef;
   EventHandlerRef mSubviewTrackingHandlerRef;
   EventHandlerRef mOverlayRootTrackingHandlerRef;
   EventHandlerRef mOverlayViewTrackingHandlerRef;
   
#elif defined(__WXMSW__)

   HANDLE mHwnd;

#else

   Display *mXdisp;
   Window mXwin;

#endif

   DECLARE_EVENT_TABLE();

   friend class VSTEffectsModule;
};

///////////////////////////////////////////////////////////////////////////////
//
// VSTEffectsModule
//
///////////////////////////////////////////////////////////////////////////////

class VSTEffectsModule : public ModuleInterface
{
public:
   VSTEffectsModule(ModuleManagerInterface *moduleManager, const wxString *path);
   virtual ~VSTEffectsModule();

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

   // VSTEffectModule implementation

   static void Check(const wxChar *path);

private:
   ModuleManagerInterface *mModMan;
   wxString mPath;
};

#endif // USE_VST
