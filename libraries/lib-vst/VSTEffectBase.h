/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTEffectBase.h

  Dominic Mazzoni

  Paul Licameli split from VSTEffect.h

**********************************************************************/
#if USE_VST

#ifndef __AUDACITY_VST_EFFECT_BASE__
#define __AUDACITY_VST_EFFECT_BASE__

#include "GlobalVariable.h"
#include "VSTWrapper.h"
#include "PerTrackEffect.h"
#include "PluginInterface.h"

/* i18n-hint: Abbreviates Virtual Studio Technology, an audio software protocol
   developed by Steinberg GmbH */
#define VSTPLUGINTYPE XO("VST")

#define audacityVSTID CCONST('a', 'u', 'D', 'y');

class VST_API VSTEffectBase
   : public VSTWrapper
   , public PerTrackEffect
{
 public:
   struct Factory : DefaultedGlobalHook< Factory,
      UniquePtrFactory<VSTEffectBase, const PluginPath&>::Function
   >{};

   VSTEffectBase(const PluginPath & path);
   ~VSTEffectBase() override;

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

   bool InitializePlugin();

   std::shared_ptr<EffectInstance> MakeInstance() const override;
   bool CanExportPresets() const override;

   bool HasOptions() const override;

   EffectSettings MakeSettings() const override;

   // Plugin loading and unloading
   std::vector<int> GetEffectIDs();

private:
   PluginID mID;
};

#endif

#endif // USE_VST
