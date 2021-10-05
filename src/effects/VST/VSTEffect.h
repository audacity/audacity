/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTEffect.h

  Dominic Mazzoni

**********************************************************************/



#if USE_VST

#include "EffectInterface.h"
#include "ModuleInterface.h"
#include "PluginInterface.h"

#include "SampleFormat.h"
#include "XMLTagHandler.h"

class wxSizerItem;
class wxSlider;
class wxStaticText;

class NumericTextCtrl;

class VSTControl;
#include "VSTControl.h"

#define VSTCMDKEY wxT("-checkvst")
/* i18n-hint: Abbreviates Virtual Studio Technology, an audio software protocol
   developed by Steinberg GmbH */
#define VSTPLUGINTYPE XO("VST")

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
typedef struct __CFBundle *CFBundleRef;
#if __LP64__
typedef int CFBundleRefNum;
#else
typedef signed short                    SInt16;
typedef SInt16 CFBundleRefNum;
#endif
#endif

///////////////////////////////////////////////////////////////////////////////
//
// VSTEffect
//
///////////////////////////////////////////////////////////////////////////////

using VSTEffectArray = std::vector < std::unique_ptr<VSTEffect> > ;

DECLARE_LOCAL_EVENT_TYPE(EVT_SIZEWINDOW, -1);
DECLARE_LOCAL_EVENT_TYPE(EVT_UPDATEDISPLAY, -1);

///////////////////////////////////////////////////////////////////////////////
///
/// VSTEffect is an Audacity EffectClientInterface that forwards actual 
/// audio processing via a VSTEffectLink
///
///////////////////////////////////////////////////////////////////////////////
class VSTEffect final : public wxEvtHandler,
                  public EffectClientInterface,
                  public EffectUIClientInterface,
                  public XMLTagHandler,
                  public VSTEffectLink
{
 public:
   VSTEffect(const PluginPath & path, VSTEffect *master = NULL);
   virtual ~VSTEffect();

   // ComponentInterface implementation

   PluginPath GetPath() override;
   ComponentInterfaceSymbol GetSymbol() override;
   VendorSymbol GetVendor() override;
   wxString GetVersion() override;
   TranslatableString GetDescription() override;

   // EffectDefinitionInterface implementation

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

   sampleCount GetLatency() override;
   size_t GetTailSize() override;

   void SetSampleRate(double rate) override;
   size_t SetBlockSize(size_t maxBlockSize) override;
   size_t GetBlockSize() const override;

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

   RegistryPaths GetFactoryPresets() override;
   bool LoadFactoryPreset(int id) override;
   bool LoadFactoryDefaults() override;

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
   std::vector<int> GetEffectIDs();

   // Parameter loading and saving
   bool LoadParameters(const RegistryPath & group);
   bool SaveParameters(const RegistryPath & group);

   // Base64 encoding and decoding
   static wxString b64encode(const void *in, int len);
   static int b64decode(const wxString &in, void *out);

   // Realtime
   unsigned GetChannelCount();
   void SetChannelCount(unsigned numChannels);

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

   intptr_t callDispatcher(int opcode, int index,
                           intptr_t value, void *ptr, float opt) override;
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
   PluginPath mPath;
   unsigned mAudioIns;
   unsigned mAudioOuts;
   int mMidiIns;
   int mMidiOuts;
   bool mAutomatable;
   float mSampleRate;
   size_t mUserBlockSize;
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

   struct ResourceHandle {
      ResourceHandle(
         CFBundleRef pHandle = nullptr, CFBundleRefNum num = 0)
      : mpHandle{ pHandle }, mNum{ num }
      {}
      ResourceHandle& operator=( ResourceHandle &&other )
      {
         if (this != &other) {
            mpHandle = other.mpHandle;
            mNum = other.mNum;
            other.mpHandle = nullptr;
            other.mNum = 0;
         }
         return *this;
      }
      ~ResourceHandle() { reset(); }
      void reset();

      CFBundleRef mpHandle{};
      CFBundleRefNum mNum{};
   };
   ResourceHandle mResource;
#endif

   AEffect *mAEffect;

   VstTimeInfo mTimeInfo;

   bool mUseLatency;
   int mBufferDelay;

   unsigned mBlockSize;

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
   FloatBuffers mMasterIn, mMasterOut;
   size_t mNumSamples;

   // UI
   wxDialog *mDialog;
   wxWindow *mParent;
   EffectUIHostInterface *mUIHost;
   wxSizerItem *mContainer;
   bool mGui;

   VSTControl *mControl;

   NumericTextCtrl *mDuration;
   ArrayOf<wxStaticText *> mNames;
   ArrayOf<wxSlider *> mSliders;
   ArrayOf<wxStaticText *> mDisplays;
   ArrayOf<wxStaticText *> mLabels;

   bool mInSet;
   bool mInChunk;
   wxString mChunk;
   long mXMLVersion;
   VstPatchChunkInfo mXMLInfo;
   
   DECLARE_EVENT_TABLE()

   friend class VSTEffectsModule;
};

///////////////////////////////////////////////////////////////////////////////
///
/// VSTEffectsModule is an Audacity ModuleInterface, in other words it 
/// represents one plug in.
///
///////////////////////////////////////////////////////////////////////////////
class VSTEffectsModule final : public ModuleInterface
{
public:
   VSTEffectsModule();
   virtual ~VSTEffectsModule();

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
   FilePath InstallPath() override;

   bool AutoRegisterPlugins(PluginManagerInterface & pm) override;
   PluginPaths FindPluginPaths(PluginManagerInterface & pm) override;
   unsigned DiscoverPluginsAtPath(
      const PluginPath & path, TranslatableString &errMsg,
      const RegistrationCallback &callback)
         override;

   bool IsPluginValid(const PluginPath & path, bool bFast) override;

   std::unique_ptr<ComponentInterface>
      CreateInstance(const PluginPath & path) override;

   // VSTEffectModule implementation

   static void Check(const wxChar *path);
};

#endif // USE_VST
