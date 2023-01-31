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

#include "../StatelessPerTrackEffect.h"
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

struct LadspaEffectSettings {
   explicit LadspaEffectSettings(size_t nPorts = 0)
      : controls( nPorts )
   {}

   // Allocate as many slots as there are ports, although some may correspond
   // to audio, not control, ports and so rest unused
   std::vector<float> controls;
};

//! Carry output control port information back to main thread
struct LadspaEffectOutputs : EffectOutputs {
   ~LadspaEffectOutputs() override;
   std::unique_ptr<EffectOutputs> Clone() const override;

   void Assign(EffectOutputs &&src) override;

   // Allocate as many slots as there are ports, although some may correspond
   // to input and audio ports and remain unused
   std::vector<float> controls;
};

class LadspaEffect final
   : public EffectWithSettings<LadspaEffectSettings, StatelessPerTrackEffect>
{
public:
   LadspaEffect(const wxString & path, int index);
   virtual ~LadspaEffect();

   EffectSettings MakeSettings() const override;
   bool CopySettingsContents(
      const EffectSettings &src, EffectSettings &dst) const override;

   std::unique_ptr<EffectOutputs> MakeOutputs() const override;

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

   int ShowClientInterface(const EffectPlugin &plugin, wxWindow &parent,
      wxDialog &dialog, EffectEditor *pEditor, bool forceModal)
   const override;
   bool InitializePlugin();
   bool InitializeControls(LadspaEffectSettings &settings) const;

   struct Instance;
   std::shared_ptr<EffectInstance> MakeInstance() const override;
   struct Editor;
   std::unique_ptr<EffectEditor> MakeEditor(
      ShuttleGui & S, EffectInstance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs)
   const override;

   bool CanExportPresets() const override;
   void ExportPresets(
      const EffectPlugin &plugin, const EffectSettings &settings)
   const override;
   OptionalMessage ImportPresets(
      const EffectPlugin &plugin, EffectSettings &settings) const override;

   bool HasOptions() const override;
   void ShowOptions(const EffectPlugin &plugin) const override;

   // LadspaEffect implementation

private:
   bool Load();
   void Unload();

   OptionalMessage LoadParameters(
      const RegistryPath & group, EffectSettings &settings) const;
   bool SaveParameters(
      const RegistryPath & group, const EffectSettings &settings) const;

   LADSPA_Handle InitInstance(
      float sampleRate, LadspaEffectSettings &settings,
      LadspaEffectOutputs *pOutputs) const;
   void FreeInstance(LADSPA_Handle handle) const;

private:

   const wxString mPath;
   const int mIndex;

   wxDynamicLibrary mLib;
   const LADSPA_Descriptor *mData{};

   wxString pluginName;

   size_t mBlockSize{ 0 };

   bool mInteractive{ false };

   unsigned mAudioIns{ 0 };
   // Mapping from input channel number to audio port number
   ArrayOf<unsigned long> mInputPorts;

   unsigned mAudioOuts{ 0 };
   // Mapping from output channel number to audio port number
   ArrayOf<unsigned long> mOutputPorts;

   int mNumInputControls{ 0 };
   int mNumOutputControls{ 0 };

   int mLatencyPort{ -1 };

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
   
   bool CheckPluginExist(const PluginPath& path) const override;

   std::unique_ptr<ComponentInterface>
      LoadPlugin(const PluginPath & path) override;

   // LadspaEffectModule implementation

   FilePaths GetSearchPaths();
};
