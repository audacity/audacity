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

#include "VSTControl.h"

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
class wxDynamicLibrary;

#if defined(__WXMAC__)
struct __CFBundle;
#endif

///////////////////////////////////////////////////////////////////////////////
//
// VSTEffect
//
///////////////////////////////////////////////////////////////////////////////

WX_DEFINE_ARRAY_PTR(VSTEffect *, VSTEffectArray);

DECLARE_LOCAL_EVENT_TYPE(EVT_SIZEWINDOW, -1);
DECLARE_LOCAL_EVENT_TYPE(EVT_UPDATEDISPLAY, -1);

class VSTEffect final : public wxEvtHandler,
                  public EffectClientInterface,
                  public EffectUIClientInterface,
                  public XMLTagHandler,
                  public VSTEffectLink
{
 public:
   VSTEffect(const wxString & path, VSTEffect *master = NULL);
   virtual ~VSTEffect();

   // IdentInterface implementation

   wxString GetPath() override;
   wxString GetSymbol() override;
   wxString GetName() override;
   wxString GetVendor() override;
   wxString GetVersion() override;
   wxString GetDescription() override;

   // EffectIdentInterface implementation

   EffectType GetType() override;
   wxString GetFamily() override;
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

   sampleCount GetLatency() override;
   size_t GetTailSize() override;

   void SetSampleRate(double rate) override;
   size_t SetBlockSize(size_t maxBlockSize) override;

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

   bool ShowInterface(wxWindow *parent, bool forceModal = false) override;

   bool GetAutomationParameters(EffectAutomationParameters & parms) override;
   bool SetAutomationParameters(EffectAutomationParameters & parms) override;

   bool LoadUserPreset(const wxString & name) override;
   bool SaveUserPreset(const wxString & name) override;

   wxArrayString GetFactoryPresets() override;
   bool LoadFactoryPreset(int id) override;
   bool LoadFactoryDefaults() override;

   // EffectUIClientInterface implementation

   void SetHostUI(EffectUIHostInterface *host) override;
   bool PopulateUI(wxWindow *parent) override;
   bool IsGraphicalUI() override;
   bool ValidateUI() override;
   bool HideUI() override;
   bool CloseUI() override;

   bool CanExportPresets() override;
   void ExportPresets() override;
   void ImportPresets() override;

   bool HasOptions() override;
   void ShowOptions() override;

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
   static int b64decode(const wxString &in, void *out);

   // Realtime
   unsigned GetChannelCount();
   void SetChannelCount(unsigned numChannels);

   // UI
   void OnSlider(wxCommandEvent & evt);
   void OnSize(wxSizeEvent & evt);
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

   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs) override;
   void HandleXMLEndTag(const wxChar *tag) override;
   void HandleXMLContent(const wxString & content) override;
   XMLTagHandler *HandleXMLChild(const wxChar *tag) override;

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
    // Define a manager class for a handle to a module
#if defined(__WXMSW__)
   using ModuleHandle = std::unique_ptr<wxDynamicLibrary>;
#else
   struct ModuleDeleter {
      void operator() (void*) const;
   };
   using ModuleHandle = std::unique_ptr < char, ModuleDeleter > ;
#endif

   EffectHostInterface *mHost;
   PluginID mID;
   wxString mPath;
   unsigned mAudioIns;
   unsigned mAudioOuts;
   int mMidiIns;
   int mMidiOuts;
   bool mAutomatable;
   float mSampleRate;
   int mUserBlockSize;
   wxString mName;
   wxString mVendor;
   wxString mDescription;
   int mVersion;
   bool mInteractive;
   int mVstVersion;

   static intptr_t mCurrentEffectID;

   bool mReady;

   ModuleHandle mModule;

#if defined(__WXMAC__)
   // These members must be ordered after mModule

   struct BundleDeleter {
      void operator() (void*) const;
   };
   using BundleHandle = std::unique_ptr<
      __CFBundle, BundleDeleter
   >;

   BundleHandle mBundleRef;

   struct ResourceDeleter {
      const BundleHandle *mpHandle;
      ResourceDeleter(const BundleHandle *pHandle = nullptr)
         : mpHandle(pHandle) {}
      void operator() (void*) const;
   };
   using ResourceHandle = std::unique_ptr<
      char, ResourceDeleter
   >;
   ResourceHandle mResource;
#endif

   AEffect *mAEffect;

   VstTimeInfo mTimeInfo;

   bool mUseLatency;
   int mBufferDelay;

   int mBlockSize;

   int mProcessLevel;
   bool mHasPower;
   bool mWantsIdle;
   bool mWantsEditIdle;

   wxCRIT_SECT_DECLARE_MEMBER(mDispatcherLock);

   std::unique_ptr<VSTEffectTimer> mTimer;
   int mTimerGuard;

   // Realtime processing
   VSTEffect *mMaster;     // non-NULL if a slave
   VSTEffectArray mSlaves;
   unsigned mNumChannels;
   float **mMasterIn;
   float **mMasterOut;
   size_t mNumSamples;

   // UI
   wxDialog *mDialog;
   wxWindow *mParent;
   EffectUIHostInterface *mUIHost;
   wxSizerItem *mContainer;
   bool mGui;

   VSTControl *mControl;

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
   
   DECLARE_EVENT_TABLE()

   friend class VSTEffectsModule;
};

///////////////////////////////////////////////////////////////////////////////
//
// VSTEffectsModule
//
///////////////////////////////////////////////////////////////////////////////

class VSTEffectsModule final : public ModuleInterface
{
public:
   VSTEffectsModule(ModuleManagerInterface *moduleManager, const wxString *path);
   virtual ~VSTEffectsModule();

   // IdentInterface implementatino

   wxString GetPath() override;
   wxString GetSymbol() override;
   wxString GetName() override;
   wxString GetVendor() override;
   wxString GetVersion() override;
   wxString GetDescription() override;

   // ModuleInterface implementation

   bool Initialize() override;
   void Terminate() override;

   bool AutoRegisterPlugins(PluginManagerInterface & pm) override;
   wxArrayString FindPlugins(PluginManagerInterface & pm) override;
   bool RegisterPlugin(PluginManagerInterface & pm, const wxString & path) override;

   bool IsPluginValid(const wxString & path) override;

   IdentInterface *CreateInstance(const wxString & path) override;
   void DeleteInstance(IdentInterface *instance) override;

   // VSTEffectModule implementation

   static void Check(const wxChar *path);

private:
   ModuleManagerInterface *mModMan;
   wxString mPath;
};

#endif // USE_VST
