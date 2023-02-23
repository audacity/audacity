/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTEffect.h

  Dominic Mazzoni

**********************************************************************/
#if USE_VST

#include "../StatelessPerTrackEffect.h"
#include "../EffectEditor.h"
#include "PluginProvider.h"
#include "PluginInterface.h"

#include "SampleFormat.h"
#include <wx/weakref.h>

#include <optional>
#include <atomic>

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

class VSTTimer;
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

class VSTInstance;
using VSTInstanceArray = std::vector < std::unique_ptr<VSTInstance> >;

///////////////////////////////////////////////////////////////////////////////
//
// VSTEffect
//
///////////////////////////////////////////////////////////////////////////////


wxDECLARE_EVENT(EVT_SIZEWINDOW, wxCommandEvent);
DECLARE_LOCAL_EVENT_TYPE(EVT_UPDATEDISPLAY, -1);


class VSTEditor;

class VSTEffect final
   : public VSTWrapper
   , public StatelessPerTrackEffect
   
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

   int ShowClientInterface(const EffectPlugin &plugin, wxWindow &parent,
      wxDialog &dialog, EffectEditor *pEditor, bool forceModal)
   const override;

   bool InitializePlugin();


   std::shared_ptr<EffectInstance> MakeInstance() const override;
   std::shared_ptr<EffectInstance> DoMakeInstance();
   std::unique_ptr<EffectEditor> PopulateUI(const EffectPlugin &plugin,
      ShuttleGui &S, EffectInstance &instance, EffectSettingsAccess &access,
      const EffectOutputs *pOutputs) const override;
   bool CanExportPresets() const override;
   void ExportPresets(
      const EffectPlugin &plugin, const EffectSettings &settings)
   const override;
   OptionalMessage ImportPresets(
      const EffectPlugin &plugin, EffectSettings &settings) const override;
   // Non-const and non-virtual function:
   OptionalMessage ImportPresetsNC(EffectSettings &settings);

   bool HasOptions() const override;
   void ShowOptions(const EffectPlugin &plugin) const override;

   // VSTEffect implementation


   

   EffectSettings MakeSettings() const override;

protected:

   //! Will never be called
   virtual std::unique_ptr<EffectEditor> MakeEditor(
      ShuttleGui & S, EffectInstance &instance, EffectSettingsAccess &access,
      const EffectOutputs *pOutputs) const final;
   
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



class VSTInstance final : public PerTrackEffect::Instance,
   public VSTWrapper
{
public:

   VSTInstance(PerTrackEffect&   effect,
                     const PluginPath& path,
                     size_t            blockSize,
                     size_t            userBlockSize,
                     bool              useLatency
                    );

   ~VSTInstance() override;


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
   bool UsesMessages() const noexcept override;
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

   const bool mUseLatency;

   size_t mBlockSize{ 8192 };

   std::unique_ptr<Message> MakeMessage() const override;

   std::unique_ptr<Message> MakeMessage(int id, double value) const;

   // VSTUIWrapper overrides

   void Automate(int index, float value) override;
   void NeedIdle()                       override;
   void SizeWindow(int w, int h)         override;
   void SetBufferDelay(int samples)      override;

   // The overrides above will forward calls to them to the corresponding
   // overrides in the Validator which owns the instance - this sets it.
   void SetOwningValidator(VSTUIWrapper* vi);

   bool OnePresetWasLoadedWhilePlaying();

   void DeferChunkApplication();

   bool HasGUI() const { return mGui; }

private:

   void callProcessReplacing(
      const float* const* inputs, float* const* outputs, int sampleframes);

   VSTInstanceArray mSlaves;

   bool mHasPower{ false };

   size_t mUserBlockSize{ mBlockSize };

   bool mReady{ false };

   bool mRecruited{ false };

   VSTUIWrapper* mpOwningValidator{};

   std::atomic_bool mPresetLoadedWhilePlaying{ false };

   std::mutex mDeferredChunkMutex;
   std::vector<char> mChunkToSetAtIdleTime{};

   void ApplyChunk(std::vector<char>& chunk);

   bool ChunkMustBeAppliedInMainThread() const;

   bool mIsMeldaPlugin{ false };
};


class VSTEditor final
   : public wxEvtHandler
   , public EffectEditor
   , public VSTUIWrapper
{
public:

   VSTEditor(VSTInstance&       instance, bool gui,
      const EffectUIServices&  services,
      EffectSettingsAccess&    access,
      wxWindow*                pParent,
      int                      numParams
   );

   ~VSTEditor() override;

   VSTInstance& GetInstance() const;

   bool ValidateUI() override;
   bool UpdateUI() override;

   void OnClose() override;

   void BuildPlain(EffectSettingsAccess& access, EffectType effectType, double projectRate);
   void BuildFancy(EffectInstance& instance);

   void OnTimer();

   std::unique_ptr<VSTTimer> mTimer;

   void RefreshParameters(int skip = -1) const;

   void Automate(int index, float value) override;

   void OnSlider(wxCommandEvent& evt);    

   int ShowDialog(bool nonModal);

   bool IsGraphicalUI() override;

   void Flush() override;

protected:
   void SizeWindow(int w, int h) override;

private:
   void NotifyParameterChanged(int index, float value);
   void OnIdle(wxIdleEvent &evt);

   VSTInstance& mInstance;
   const bool mGui;

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
   
   VSTControl* mControl{};

   // Mapping from parameter ID to string
   std::vector<wxString> mParamNames;

   int mNumParams{ 0 };
};



#endif // USE_VST
