/**********************************************************************

  Audacity: A Digital Audio Editor

  LadspaEffect.h

  Dominic Mazzoni

**********************************************************************/
#ifndef __AUDACITY_LADSPA_EFFECT__
#define __AUDACITY_LADSPA_EFFECT__

#include "LadspaInstance.h"

#include <wx/dynlib.h> // member variable
//#include <wx/event.h> // to inherit
//#include <wx/weakref.h>

#include "../StatelessPerTrackEffect.h"
#include "PluginProvider.h"
#include "PluginInterface.h"

//#include "SampleFormat.h"

#define LADSPAEFFECTS_VERSION wxT("1.0.0.0")
/* i18n-hint: abbreviates "Linux Audio Developer's Simple Plugin API"
   (Application programming interface)
 */
#define LADSPAEFFECTS_FAMILY XO("LADSPA")

class LadspaEffectBase
   : public EffectWithSettings<LadspaEffectSettings, PerTrackEffect>
{
public:
   LadspaEffectBase(const wxString & path, int index);
   ~LadspaEffectBase() override;

   bool InitializePlugin();

protected:
   EffectSettings MakeSettings() const override;
   bool CopySettingsContents(
      const EffectSettings &src, EffectSettings &dst) const override;

   std::unique_ptr<EffectOutputs> MakeOutputs() const override;

   PluginPath GetPath() const override;
   ComponentInterfaceSymbol GetSymbol() const override;
   VendorSymbol GetVendor() const override;
   wxString GetVersion() const override;
   TranslatableString GetDescription() const override;

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

   bool InitializeControls(LadspaEffectSettings &settings) const;

   std::shared_ptr<EffectInstance> MakeInstance() const override;

   bool CanExportPresets() const override;

   bool HasOptions() const override;

   bool Load();
   void Unload();

   OptionalMessage LoadParameters(
      const RegistryPath & group, EffectSettings &settings) const;
   bool SaveParameters(
      const RegistryPath & group, const EffectSettings &settings) const;

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

   unsigned mNumInputControls{ 0 };
   unsigned mNumOutputControls{ 0 };

   int mLatencyPort{ -1 };
};

class LadspaEffect final
   : public LadspaEffectBase
   , public StatelessEffectUIServices
{
public:
   using LadspaEffectBase::LadspaEffectBase;
   ~LadspaEffect() override;

private:
   int ShowClientInterface(const EffectPlugin &plugin, wxWindow &parent,
      wxDialog &dialog, EffectEditor *pEditor, bool forceModal)
   const override;

   std::unique_ptr<EffectEditor> MakeEditor(
      ShuttleGui & S, EffectInstance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs)
   const override;

   void ExportPresets(
      const EffectPlugin &plugin, const EffectSettings &settings)
   const override;
   OptionalMessage ImportPresets(
      const EffectPlugin &plugin, EffectSettings &settings) const override;

   void ShowOptions(const EffectPlugin &plugin) const override;
};
#endif
