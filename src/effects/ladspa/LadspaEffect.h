/**********************************************************************

  Audacity: A Digital Audio Editor

  LadspaEffect.h

  Dominic Mazzoni

**********************************************************************/

class wxSlider;
class wxStaticText;
class wxTextCtrl;
class wxCheckBox;

class NumericTextCtrl;

#include <wx/dynlib.h> // member variable
#include <wx/event.h> // to inherit
#include <wx/weakref.h>

#include "EffectInterface.h"
#include "PluginProvider.h"
#include "PluginInterface.h"

#include "ladspa.h"
#include "SampleFormat.h"

#define LADSPAEFFECTS_VERSION wxT("1.0.0.0")
/* i18n-hint: abbreviates "Linux Audio Developer's Simple Plugin API"
   (Application programming interface)
 */
#define LADSPAEFFECTS_FAMILY XO("LADSPA")

///////////////////////////////////////////////////////////////////////////////
//
// LadspaEffect
//
///////////////////////////////////////////////////////////////////////////////

class LadspaEffectMeter;

class LadspaEffect final : public wxEvtHandler,
                     public EffectUIClientInterface
{
public:
   LadspaEffect(const wxString & path, int index);
   virtual ~LadspaEffect();

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
   bool SupportsRealtime() const override;
   bool SupportsAutomation() const override;

   bool SaveSettings(
      const EffectSettings &settings, CommandParameters & parms) const override;
   bool LoadSettings(
      const CommandParameters & parms, Settings &settings) const override;

   bool LoadUserPreset(
      const RegistryPath & name, Settings &settings) const override;
   bool DoLoadUserPreset(const RegistryPath & name, Settings &settings);
   bool SaveUserPreset(
      const RegistryPath & name, const Settings &settings) const override;

   RegistryPaths GetFactoryPresets() const override;
   bool LoadFactoryPreset(int id, EffectSettings &settings) const override;
   bool LoadFactoryDefaults(EffectSettings &settings) const override;
   bool DoLoadFactoryDefaults(EffectSettings &settings);

   // EffectProcessor implementation

   unsigned GetAudioInCount() const override;
   unsigned GetAudioOutCount() const override;

   int GetMidiInCount() override;
   int GetMidiOutCount() override;

   void SetSampleRate(double rate) override;
   size_t SetBlockSize(size_t maxBlockSize) override;
   size_t GetBlockSize() const override;

   sampleCount GetLatency() override;
   size_t GetTailSize() override;

   bool ProcessInitialize(EffectSettings &settings,
      sampleCount totalLen, ChannelNames chanMap) override;
   bool ProcessFinalize() override;
   size_t ProcessBlock(EffectSettings &settings,
      const float *const *inBlock, float *const *outBlock, size_t blockLen)
      override;

   bool RealtimeInitialize(EffectSettings &settings) override;
   bool RealtimeAddProcessor(EffectSettings &settings,
      unsigned numChannels, float sampleRate) override;
   bool RealtimeFinalize(EffectSettings &settings) noexcept override;
   bool RealtimeSuspend() override;
   bool RealtimeResume() noexcept override;
   bool RealtimeProcessStart(EffectSettings &settings) override;
   size_t RealtimeProcess(int group,  EffectSettings &settings,
      const float *const *inbuf, float *const *outbuf, size_t numSamples)
      override;
   bool RealtimeProcessEnd(EffectSettings &) noexcept override;

   int ShowClientInterface(
      wxWindow &parent, wxDialog &dialog, bool forceModal) override;
   bool InitializePlugin();

   // EffectUIClientInterface implementation

   bool InitializeInstance(EffectSettings &settings) override;
   std::unique_ptr<EffectUIValidator> PopulateUI(
      ShuttleGui &S, EffectSettingsAccess &access) override;
   bool IsGraphicalUI() override;
   bool ValidateUI(EffectSettings &) override;
   bool CloseUI() override;

   bool CanExportPresets() override;
   void ExportPresets(const EffectSettings &settings) const override;
   void ImportPresets(EffectSettings &settings) override;

   bool HasOptions() override;
   void ShowOptions() override;

   // LadspaEffect implementation

private:
   bool Load();
   void Unload();

   bool LoadParameters(const RegistryPath & group, EffectSettings &settings);
   bool SaveParameters(
      const RegistryPath & group, const EffectSettings &settings) const;

   LADSPA_Handle InitInstance(float sampleRate);
   void FreeInstance(LADSPA_Handle handle);

   void OnCheckBox(wxCommandEvent & evt);
   void OnSlider(wxCommandEvent & evt);
   void OnTextCtrl(wxCommandEvent & evt);
   void RefreshControls(bool outputOnly = false);

private:

   wxString mPath;
   int mIndex;

   wxDynamicLibrary mLib;
   const LADSPA_Descriptor *mData;

   wxString pluginName;

   bool mReady;

   LADSPA_Handle mMaster;

   double mSampleRate;
   size_t mBlockSize;

   bool mInteractive;

   unsigned mAudioIns;
   ArrayOf<unsigned long> mInputPorts;

   unsigned mAudioOuts;
   ArrayOf<unsigned long> mOutputPorts;

   int mNumInputControls;
   Floats mInputControls;
   int mNumOutputControls;
   Floats mOutputControls;

   bool mUseLatency;
   int mLatencyPort;
   bool mLatencyDone;

   // Realtime processing
   std::vector<LADSPA_Handle> mSlaves;

   NumericTextCtrl *mDuration;
   wxWeakRef<wxDialog> mDialog;
   wxWindow *mParent;
   ArrayOf<wxSlider*> mSliders;
   ArrayOf<wxTextCtrl*> mFields;
   ArrayOf<wxStaticText*> mLabels;
   ArrayOf<wxCheckBox*> mToggles;
   ArrayOf<LadspaEffectMeter *> mMeters;

   DECLARE_EVENT_TABLE()

   friend class LadspaEffectsModule;
};

///////////////////////////////////////////////////////////////////////////////
//
// LadspaEffectsModule
//
///////////////////////////////////////////////////////////////////////////////

class LadspaEffectsModule final : public PluginProvider
{
public:
   LadspaEffectsModule();
   virtual ~LadspaEffectsModule();

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

   // LadspaEffectModule implementation

   FilePaths GetSearchPaths();
};

