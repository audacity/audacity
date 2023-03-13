/**********************************************************************

  Audacity: A Digital Audio Editor

  LV2EffectBase.h

  Paul Licameli split from LV2Effectg

  Audacity(R) is copyright (c) 1999-2013 Audacity Team.
  License: GPL v2 or later.  See License.txt.

*********************************************************************/
#ifndef __AUDACITY_LV2_EFFECT_BASE__
#define __AUDACITY_LV2_EFFECT_BASE__

#if USE_LV2

#include "LV2FeaturesList.h"
#include "LV2Ports.h"

#include "GlobalVariable.h"
#include "SampleFormat.h"
#include "PerTrackEffect.h"

#define LV2EFFECTS_VERSION wxT("1.0.0.0")
/* i18n-hint: abbreviates
   "Linux Audio Developer's Simple Plugin API (LADSPA) version 2" */
#define LV2EFFECTS_FAMILY XO("LV2")

class LV2_API LV2EffectBase : public PerTrackEffect
{
public:
   struct LV2_API Factory : DefaultedGlobalHook< Factory,
      UniquePtrFactory<LV2EffectBase, const LilvPlugin &>::Function
   >{};

   LV2EffectBase(const LilvPlugin &plug);
   ~LV2EffectBase() override;

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

   bool InitializePlugin();

   std::shared_ptr<EffectInstance> MakeInstance() const override;

   bool CanExportPresets() const override;

   bool HasOptions() const override;

   // LV2Effect implementation

   EffectSettings MakeSettings() const override;
   bool CopySettingsContents(
      const EffectSettings &src, EffectSettings &dst) const override;

   std::unique_ptr<EffectOutputs> MakeOutputs() const override;

   OptionalMessage LoadParameters(
      const RegistryPath & group, EffectSettings &settings) const;
   bool SaveParameters(
      const RegistryPath & group, const EffectSettings &settings) const;

   const LilvPlugin &mPlug;
   const LV2FeaturesList mFeatures{ mPlug };

   const LV2Ports mPorts{ mPlug };

   bool mWantsOptionsInterface{ false };
   bool mWantsStateInterface{ false };

   size_t mFramePos{};

   FloatBuffers mCVInBuffers;
   FloatBuffers mCVOutBuffers;

   double mLength{};

   // Mutable cache fields computed once on demand
   mutable bool mFactoryPresetsLoaded{ false };
   mutable RegistryPaths mFactoryPresetNames;
   mutable wxArrayString mFactoryPresetUris;
};

#endif
#endif
