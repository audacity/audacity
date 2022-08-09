/**********************************************************************

  Audacity: A Digital Audio Editor

  @file VST3Effect.h

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/


#pragma once

#include <wx/wx.h>

#include <public.sdk/source/vst/hosting/module.h>

#include "../StatefulPerTrackEffect.h"

class VST3Instance;

/**
 * \brief Objects of this class connect Audacity with VST3 effects
 */
class VST3Effect final : public PerTrackEffect
{
   // Keep strong reference to a module; this because it has to be destroyed in the destructor of this class,
   // otherwise the destruction of mEditController and mEffectComponent would trigger a memory fault.
   std::shared_ptr<VST3::Hosting::Module> mModule;

   wxWindow* mParent{nullptr};

   VST3::Hosting::ClassInfo mEffectClassInfo;

   std::weak_ptr<VST3Instance> mCurrentDisplayEffect;

   std::vector<std::shared_ptr<VST3Effect>> mRealtimeGroupProcessors;

   // Mutable cache fields computed once on demand
   mutable bool mRescanFactoryPresets { true };
   mutable RegistryPaths mFactoryPresets;

   mutable bool mInitialFetchDone{ false };

public:

   static EffectFamilySymbol GetFamilySymbol();

   VST3Effect(
      std::shared_ptr<VST3::Hosting::Module> module,
      VST3::Hosting::ClassInfo effectClassInfo);

   VST3Effect(const VST3Effect&) = delete;
   VST3Effect& operator=(const VST3Effect&) = delete;

   ~VST3Effect() override;

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
   ///Saves only changes recorded during session, not the complete state(see SaveUserPreset)
   bool SaveSettings(const EffectSettings &settings, CommandParameters & parms) const override;
   ///Applies previousely saved parameter changes
   bool LoadSettings( const CommandParameters & parms, EffectSettings &settings) const override;
   ///Attempts to restore previousely saved state to the currently edited(!) effect
   bool LoadUserPreset(const RegistryPath & name, EffectSettings &settings) const override;
   ///Attempts to store the complete state of the currently edited(!) effect
   bool SaveUserPreset(const RegistryPath & name, const EffectSettings &settings) const override;
   RegistryPaths GetFactoryPresets() const override;
   bool LoadFactoryPreset(int id, EffectSettings &settings) const override;

   int ShowClientInterface(wxWindow &parent, wxDialog &dialog,
      EffectUIValidator *pValidator, bool forceModal) override;
   bool CloseUI() override;
   std::shared_ptr<EffectInstance> MakeInstance() const override;
   std::unique_ptr<EffectUIValidator> PopulateUI(
      ShuttleGui &S, EffectInstance &instance, EffectSettingsAccess &access)
   override;
   bool CanExportPresets() override;
   void ExportPresets(const EffectSettings &settings) const override;
   void ImportPresets(EffectSettings &settings) override;
   bool HasOptions() override;
   void ShowOptions() override;

   EffectSettings MakeSettings() const override;
   bool CopySettingsContents(const EffectSettings& src, EffectSettings& dst, SettingsCopyDirection copyDirection) const override;

private:

   bool LoadPreset(const wxString& path, EffectSettings& settings);
};
