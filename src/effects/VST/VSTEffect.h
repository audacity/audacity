/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTEffect.h

  Dominic Mazzoni

**********************************************************************/



#if USE_VST

#include "../StatefulPerTrackEffect.h"
#include "CFResources.h"
#include "PluginProvider.h"
#include "PluginInterface.h"

#include "SampleFormat.h"
#include "XMLTagHandler.h"
#include <wx/weakref.h>

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
/// VSTEffect is an Audacity effect that forwards actual
/// audio processing via a VSTEffectLink
///
///////////////////////////////////////////////////////////////////////////////
class VSTEffect final
   : public StatefulPerTrackEffect
   , public XMLTagHandler
   , public VSTEffectLink
{
 public:
   VSTEffect(const PluginPath & path, VSTEffect *master = NULL);
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

   bool LoadUserPreset(
      const RegistryPath & name, EffectSettings &settings) const override;
   bool DoLoadUserPreset(const RegistryPath & name, EffectSettings &settings);
   bool SaveUserPreset(
      const RegistryPath & name, const EffectSettings &settings) const override;

   RegistryPaths GetFactoryPresets() const override;
   bool LoadFactoryPreset(int id, EffectSettings &settings) const override;
   bool DoLoadFactoryPreset(int id);

   unsigned GetAudioInCount() const override;
   unsigned GetAudioOutCount() const override;

   int GetMidiInCount() const override;
   int GetMidiOutCount() const override;

   sampleCount GetLatency() override;

   size_t SetBlockSize(size_t maxBlockSize) override;
   size_t GetBlockSize() const override;

   bool IsReady();
   bool ProcessInitialize(EffectSettings &settings, double sampleRate,
      sampleCount totalLen, ChannelNames chanMap) override;
   bool DoProcessInitialize(double sampleRate);
   bool ProcessFinalize() override;
   size_t ProcessBlock(EffectSettings &settings,
      const float *const *inBlock, float *const *outBlock, size_t blockLen)
      override;

   bool RealtimeInitialize(EffectSettings &settings, double sampleRate)
      override;
   bool RealtimeAddProcessor(EffectSettings &settings,
      unsigned numChannels, float sampleRate) override;
   bool RealtimeFinalize(EffectSettings &settings) noexcept override;
   bool RealtimeSuspend() override;
   bool RealtimeResume() override;
   bool RealtimeProcessStart(EffectSettings &settings) override;
   size_t RealtimeProcess(size_t group,  EffectSettings &settings,
      const float *const *inbuf, float *const *outbuf, size_t numSamples)
      override;
   bool RealtimeProcessEnd(EffectSettings &settings) noexcept override;

   int ShowClientInterface(
      wxWindow &parent, wxDialog &dialog, bool forceModal) override;

   bool InitializePlugin();

   // EffectUIClientInterface implementation

   std::shared_ptr<EffectInstance> MakeInstance() const override;
   std::shared_ptr<EffectInstance> DoMakeInstance();
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
   bool LoadParameters(const RegistryPath & group, EffectSettings &settings);
   bool SaveParameters(
       const RegistryPath & group, const EffectSettings &settings) const;

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

   void BuildPlain(EffectSettingsAccess &access);
   void BuildFancy();
   wxSizer *BuildProgramBar();
   void RefreshParameters(int skip = -1);

   // Program/Bank loading/saving
   bool LoadFXB(const wxFileName & fn);
   bool LoadFXP(const wxFileName & fn);
   bool LoadXML(const wxFileName & fn);
   bool LoadFXProgram(unsigned char **bptr, ssize_t & len, int index, bool dryrun);
   void SaveFXB(const wxFileName & fn) const;
   void SaveFXP(const wxFileName & fn) const;
   void SaveXML(const wxFileName & fn) const;
   void SaveFXProgram(wxMemoryBuffer & buf, int index) const;

   bool HandleXMLTag(const std::string_view& tag, const AttributesList &attrs) override;
   void HandleXMLEndTag(const std::string_view& tag) override;
   void HandleXMLContent(const std::string_view& content) override;
   XMLTagHandler *HandleXMLChild(const std::string_view& tag) override;

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

   int GetString(wxString & outstr, int opcode, int index = 0) const;
   wxString GetString(int opcode, int index = 0) const;
   void SetString(int opcode, const wxString & str, int index = 0);

   // VST methods

   intptr_t callDispatcher(int opcode, int index,
                           intptr_t value, void *ptr, float opt) override;
   intptr_t constCallDispatcher(int opcode, int index,
                           intptr_t value, void *ptr, float opt) const;
   void callProcessReplacing(
      const float *const *inputs, float *const *outputs, int sampleframes);
   void callSetParameter(int index, float value);
   float callGetParameter(int index) const;
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

   PluginID mID;
   PluginPath mPath;
   unsigned mAudioIns;
   unsigned mAudioOuts;
   int mMidiIns;
   int mMidiOuts;
   bool mAutomatable;
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

   using BundleHandle = CF_ptr<CFBundleRef>;

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

   // UI
   wxWeakRef<wxDialog> mDialog;
   wxWindow *mParent;
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

   bool IsPluginValid(const PluginPath & path, bool bFast) override;

   std::unique_ptr<ComponentInterface>
      LoadPlugin(const PluginPath & path) override;
};

#endif // USE_VST
