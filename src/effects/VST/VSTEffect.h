/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTEffect.h

  Dominic Mazzoni

**********************************************************************/



#if USE_VST

#include "../PerTrackEffect.h"
#include "CFResources.h"
#include "PluginProvider.h"
#include "PluginInterface.h"

#include "SampleFormat.h"
#include "XMLTagHandler.h"
#include <wx/weakref.h>

#include <unordered_map>
#include <optional>
#include <mutex>
#include <thread>

class wxSizerItem;
class wxSlider;
class wxStaticText;

class NumericTextCtrl;

class VSTControl;
#include "VSTControl.h"

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


struct VSTEffectSettings
{
   // These are saved in the Config and checked against when loading a preset, to make sure
   // that we are loading a Config  which is compatible.
   //
   int32_t mUniqueID{};
   int32_t mVersion{};
   int32_t mNumParams{};

   // When loading a preset, the preferred way is to use the chunk; when not present in
   // the Config or failing to load, we fall back to loading single parameters (ID, value) pairs.
   //
   // It looks like a plugin might not support this (if their effFlagsProgramChunks bit is off)
   // If not, then hold an empty vector
   //
   std::vector<char> mChunk;

   // Fallback data used when the chunk is not available.
   std::unordered_map<wxString, std::optional<double> > mParamsMap;
};


struct VSTEffectUIWrapper
{
   virtual void NeedIdle();
   virtual void SizeWindow(int w, int h);
   virtual void Automate(int index, float value);
};


struct VSTEffectWrapper : public VSTEffectLink, public XMLTagHandler, public VSTEffectUIWrapper
{
   static inline VSTEffectSettings& GetSettings(EffectSettings& settings)
   {
      auto pSettings = settings.cast<VSTEffectSettings>();
      assert(pSettings);
      return *pSettings;
   }

   static inline const VSTEffectSettings& GetSettings(const EffectSettings& settings)
   {
      auto pSettings = settings.cast<VSTEffectSettings>();
      assert(pSettings);
      return *pSettings;
   }

   explicit VSTEffectWrapper(const PluginPath& path)
      : mPath(path)
      , mMainThreadId{ std::this_thread::get_id() }
   {}

   ~VSTEffectWrapper();

   AEffect* mAEffect = nullptr;
   std::thread::id mMainThreadId;

   intptr_t callDispatcher(int opcode, int index,
      intptr_t value, void* ptr, float opt) override;

   intptr_t constCallDispatcher(int opcode, int index,
      intptr_t value, void* ptr, float opt) const;

   wxCRIT_SECT_DECLARE_MEMBER(mDispatcherLock);

   float callGetParameter(int index) const;

   void callSetChunk(bool isPgm, int len, void* buf);
   void callSetChunk(bool isPgm, int len, void* buf, VstPatchChunkInfo* info) const;


   int      GetString(wxString& outstr, int opcode, int index = 0) const;
   wxString GetString(int opcode, int index = 0) const;

   struct ParameterInfo
   {
      int      mID;
      wxString mName;
   };

   //! @return true  continue visiting
   //! @return false stop     visiting
   using ParameterVisitor = std::function< bool(const ParameterInfo& pi) >;

   void ForEachParameter(ParameterVisitor visitor) const;

   bool FetchSettings(VSTEffectSettings& vst3Settings, bool doFetch=true) const;

   bool StoreSettings(const VSTEffectSettings& vst3settings) const;

   VstPatchChunkInfo GetChunkInfo() const;

   bool IsCompatible(const VstPatchChunkInfo&) const;

   // These are here because they are used by the import/export methods
   int mVstVersion;
   wxString mName;

   // XML load/save
   bool mInSet;
   bool mInChunk;
   wxString mChunk;
   long mXMLVersion;
   VstPatchChunkInfo mXMLInfo;

   bool LoadXML(const wxFileName& fn);
   bool HandleXMLTag(const std::string_view& tag, const AttributesList& attrs) override;
   void HandleXMLEndTag(const std::string_view& tag) override;
   void HandleXMLContent(const std::string_view& content) override;
   XMLTagHandler* HandleXMLChild(const std::string_view& tag) override;

   void SetString(int opcode, const wxString& str, int index = 0);

   ComponentInterfaceSymbol GetSymbol() const;

   void callSetParameter(int index, float value) const;

   void SaveXML(const wxFileName& fn) const;

   // Other formats for import/export
   bool LoadFXB(const wxFileName& fn);
   bool LoadFXP(const wxFileName& fn);
   bool LoadFXProgram(unsigned char** bptr, ssize_t& len, int index, bool dryrun);
   void callSetProgram(int index);

   void SaveFXB(const wxFileName& fn) const;
   void SaveFXP(const wxFileName& fn) const;
   void SaveFXProgram(wxMemoryBuffer& buf, int index) const;


   intptr_t mCurrentEffectID {};
   
   bool Load();
   PluginPath   mPath;

   // Define a manager class for a handle to a module
#if defined(__WXMSW__)
   using ModuleHandle = std::unique_ptr<wxDynamicLibrary>;
#else
   struct ModuleDeleter {
      void operator() (void*) const;
   };
   using ModuleHandle = std::unique_ptr < char, ModuleDeleter >;
#endif

   ModuleHandle mModule{};

   wxString mVendor;
   wxString mDescription;
   int      mVersion;
   bool     mInteractive{ false };
   unsigned mAudioIns{ 0 };
   unsigned mAudioOuts{ 0 };
   int      mMidiIns{ 0 };
   int      mMidiOuts{ 0 };
   bool     mAutomatable;

   void Unload();

   void ResetModuleAndHandle();

#if defined(__WXMAC__)
   // These members must be ordered after mModule

   using BundleHandle = CF_ptr<CFBundleRef>;

   BundleHandle mBundleRef;

   struct ResourceHandle {
      ResourceHandle(
         CFBundleRef pHandle = nullptr, CFBundleRefNum num = 0)
         : mpHandle{ pHandle }, mNum{ num }
      {}
      ResourceHandle& operator=(ResourceHandle&& other)
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

   
   VstTimeInfo* GetTimeInfo();
   float        GetSampleRate();
   VstTimeInfo  mTimeInfo;

   int mBufferDelay{ 0 };

   virtual int GetProcessLevel();

   bool   mUseLatency{ true };

   // The vst callback is currently called both for the effect and for instances.
   //
   static intptr_t AudioMaster(AEffect *effect,
                               int32_t opcode,
                               int32_t index,
                               intptr_t value,
                               void * ptr,
                               float opt);

   // Some of the methods called by the callback make sense for the Effect:
   //
   // - All GUI-related stuff 
   
   virtual void UpdateDisplay();
   

   // Some other methods called by the callback make sense for Instances:
   void         SetBufferDelay(int samples);


   // Make message carrying all the information in settings, including chunks
   // This is called only on the main thread
   std::unique_ptr<EffectInstance::Message>
      MakeMessageFS(const VSTEffectSettings& settings) const;
};

class VSTEffectInstance;
using VSTInstanceArray = std::vector < std::unique_ptr<VSTEffectInstance> >;

///////////////////////////////////////////////////////////////////////////////
//
// VSTEffect
//
///////////////////////////////////////////////////////////////////////////////


wxDECLARE_EVENT(EVT_SIZEWINDOW, wxCommandEvent);
DECLARE_LOCAL_EVENT_TYPE(EVT_UPDATEDISPLAY, -1);


class VSTEffectValidator;

///////////////////////////////////////////////////////////////////////////////
///
/// VSTEffect is an Audacity effect that forwards actual
/// audio processing via a VSTEffectLink
///
///////////////////////////////////////////////////////////////////////////////
class VSTEffect final
   :  public VSTEffectWrapper
     ,public PerTrackEffect
   
{
 public:
   VSTEffect(const PluginPath & path);
   virtual ~VSTEffect();

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
   RealtimeSince RealtimeSupport() const override;
   bool SupportsAutomation() const override;

   bool SaveSettings(
      const EffectSettings &settings, CommandParameters & parms) const override;
   bool LoadSettings(
      const CommandParameters & parms, EffectSettings &settings) const override;

   OptionalMessage LoadUserPreset(
      const RegistryPath & name, EffectSettings &settings) const override;
   
   bool SaveUserPreset(
      const RegistryPath & name, const EffectSettings &settings) const override;

   RegistryPaths GetFactoryPresets() const override;
   OptionalMessage LoadFactoryPreset(int id, EffectSettings &settings)
      const override;
   bool DoLoadFactoryPreset(int id);

   int ShowClientInterface(wxWindow &parent, wxDialog &dialog,
      EffectUIValidator *pValidator, bool forceModal) override;

   bool InitializePlugin();


   // EffectUIClientInterface implementation

   std::shared_ptr<EffectInstance> MakeInstance() const override;
   std::shared_ptr<EffectInstance> DoMakeInstance();
   std::unique_ptr<EffectUIValidator> PopulateUI(
      ShuttleGui &S, EffectInstance &instance, EffectSettingsAccess &access,
      const EffectOutputs *pOutputs) override;
   bool IsGraphicalUI() override;
   
   bool CloseUI() override;

   bool CanExportPresets() override;
   void ExportPresets(const EffectSettings &settings) const override;
   OptionalMessage ImportPresets(EffectSettings &settings) override;

   bool HasOptions() override;
   void ShowOptions() override;

   // VSTEffect implementation


   

   EffectSettings MakeSettings() const override;

   virtual int GetProcessLevel() override;

protected:
   
   void UpdateDisplay() override;
   

private:


   // Plugin loading and unloading
   
   std::vector<int> GetEffectIDs();

   // UI
   
   
   void OnUpdateDisplay(wxCommandEvent & evt);


   void OnProgram(wxCommandEvent & evt);
   void OnProgramText(wxCommandEvent & evt);
   void OnLoad(wxCommandEvent & evt);
   void OnSave(wxCommandEvent & evt);
   void OnSettings(wxCommandEvent & evt);

   
   wxSizer *BuildProgramBar();

 private:

   PluginID mID;
     
   wxSizerItem* mContainer{};
   bool mGui{false};
   
   friend class VSTEffectsModule;

   //mutable bool mInitialFetchDone{ false };
};

class VSTEffectsModule final : public PluginProvider
{
public:
   VSTEffectsModule();
   virtual ~VSTEffectsModule();

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
   FilePath InstallPath() override;

   void AutoRegisterPlugins(PluginManagerInterface & pm) override;
   PluginPaths FindModulePaths(PluginManagerInterface & pm) override;
   unsigned DiscoverPluginsAtPath(
      const PluginPath & path, TranslatableString &errMsg,
      const RegistrationCallback &callback)
         override;
   
   bool CheckPluginExist(const PluginPath& path) const override;

   std::unique_ptr<ComponentInterface>
      LoadPlugin(const PluginPath & path) override;
};



class VSTEffectInstance final : public PerTrackEffect::Instance,
   public VSTEffectWrapper
{
public:

   VSTEffectInstance(PerTrackEffect&   effect,
                     const PluginPath& path,
                     size_t            blockSize,
                     size_t            userBlockSize,
                     bool              useLatency
                    );

   ~VSTEffectInstance() override;


   bool ProcessInitialize(EffectSettings& settings, double sampleRate,
                          ChannelNames chanMap) override;
   
   bool ProcessFinalize() noexcept override;

   size_t SetBlockSize(size_t maxBlockSize) override;
   size_t GetBlockSize() const override;

   bool RealtimeInitialize(EffectSettings& settings, double sampleRate)
      override;
   bool RealtimeAddProcessor(EffectSettings& settings, EffectOutputs *pOutputs,
      unsigned numChannels, float sampleRate) override;
   bool RealtimeFinalize(EffectSettings& settings) noexcept override;
   bool RealtimeSuspend() override;
   bool RealtimeResume() override;
   bool RealtimeProcessStart(MessagePackage& package) override;
   size_t RealtimeProcess(size_t group, EffectSettings& settings,
      const float* const* inbuf, float* const* outbuf, size_t numSamples)
      override;
   bool RealtimeProcessEnd(EffectSettings& settings) noexcept override;




   size_t ProcessBlock(EffectSettings& settings,
      const float* const* inBlock, float* const* outBlock, size_t blockLen) override;

   SampleCount GetLatency(const EffectSettings& settings, double sampleRate)
      const override;

   bool IsReady();

   unsigned GetAudioInCount() const override;

   unsigned GetAudioOutCount() const override;

   bool DoProcessInitialize(double sampleRate);

   void PowerOn();
   void PowerOff();

   size_t mBlockSize{ 8192 };

   std::unique_ptr<Message> MakeMessage() const override;

   std::unique_ptr<Message> MakeMessage(int id, double value) const;

   // VSTEffectUIWrapper overrides

   void Automate(int index, float value) override;
   void NeedIdle()                       override;
   void SizeWindow(int w, int h)         override;

   // The overrides above will forward calls to them to the corresponding
   // overrides in the Validator which owns the instance - this sets it.
   void SetOwningValidator(VSTEffectUIWrapper* vi);

   virtual int GetProcessLevel() override;

private:

   void callProcessReplacing(
      const float* const* inputs, float* const* outputs, int sampleframes);

   VSTInstanceArray mSlaves;

   bool mHasPower{ false };

   size_t mUserBlockSize{ mBlockSize };

   bool mReady{ false };

   bool mRecruited{ false };

   VSTEffectUIWrapper* mpOwningValidator{};
};


class VSTEffectValidator final
   : public wxEvtHandler
   , public EffectUIValidator
   , public VSTEffectUIWrapper
{
public:

    VSTEffectValidator(VSTEffectInstance&       instance,
                       EffectUIClientInterface& effect,
                       EffectSettingsAccess&    access,
                       wxWindow*                pParent,
                       int                      numParams
                      );

   ~VSTEffectValidator() override;

   VSTEffectInstance& GetInstance() const;

   bool ValidateUI() override;
   bool UpdateUI() override;

   void OnClose() override;

   void BuildPlain(EffectSettingsAccess& access, EffectType effectType, double projectRate);
   void BuildFancy(EffectInstance& instance);

   void OnTimer();

   std::unique_ptr<VSTEffectTimer> mTimer;   

   void RefreshParameters(int skip = -1) const;

   void Automate(int index, float value) override;

   void OnSlider(wxCommandEvent& evt);    

   int ShowDialog(bool nonModal);

   bool IsGraphicalUI() override;

protected:
   void SizeWindow(int w, int h) override;

private:
   void NotifyParameterChanged(int index, float value);
   void OnIdle(wxIdleEvent &evt);

   VSTEffectInstance& mInstance;

   bool FetchSettingsFromInstance(EffectSettings& settings);
   bool StoreSettingsToInstance(const EffectSettings& settings);
   void NeedEditIdle(bool state);
   void NeedIdle() override;

   void OnSizeWindow(wxCommandEvent& evt);

   int  mTimerGuard{ 0 };

   bool mWantsEditIdle{ false };
   bool mWantsIdle{ false };

   // Remembers last slider movements until idle time
   std::vector<std::pair<int, double>> mLastMovements{};

   ArrayOf<wxStaticText*> mNames;
   ArrayOf<wxSlider*> mSliders;
   ArrayOf<wxStaticText*> mDisplays;
   ArrayOf<wxStaticText*> mLabels;
   NumericTextCtrl* mDuration;

   wxWindow* mParent;
   wxWeakRef<wxDialog> mDialog;
   
   VSTControl* mControl;

   // Mapping from parameter ID to string
   std::vector<wxString> mParamNames;

   int mNumParams{ 0 };
};



#endif // USE_VST
