/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioUnitEffect.h

  Dominic Mazzoni
  Leland Lucius

**********************************************************************/
#ifndef AUDACITY_AUDIOUNIT_EFFECT_H
#define AUDACITY_AUDIOUNIT_EFFECT_H



#if USE_AUDIO_UNITS

#include "AudioUnitWrapper.h"

#include "MemoryX.h"
#include <functional>
#include <type_traits>
#include <vector>

#include <AudioToolbox/AudioUnitUtilities.h>
#include <AudioUnit/AudioUnitProperties.h>

#include "../StatelessPerTrackEffect.h"
#include "PluginInterface.h"

#include <wx/weakref.h>

#define AUDIOUNITEFFECTS_VERSION wxT("1.0.0.0")
/* i18n-hint: the name of an Apple audio software protocol */
#define AUDIOUNITEFFECTS_FAMILY EffectFamilySymbol{ wxT("AudioUnit"), XO("Audio Unit") }
class AudioUnitEffect;

class AUControl;

class AudioUnitEffect final
   : public StatelessPerTrackEffect
   , AudioUnitWrapper
{
public:
   using Parameters = PackedArray::Ptr<const AudioUnitParameterID>;

   AudioUnitEffect(const PluginPath & path,
      const wxString & name, AudioComponent component,
      Parameters *pParameters = nullptr,
      AudioUnitEffect *master = nullptr);
   virtual ~AudioUnitEffect();

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

   EffectSettings MakeSettings() const override;
   bool CopySettingsContents(
      const EffectSettings &src, EffectSettings &dst) const override;

   bool SaveSettings(
      const EffectSettings &settings, CommandParameters & parms) const override;
   //! May allocate memory, so should be called only in the main thread
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

   std::shared_ptr<EffectInstance> MakeInstance() const override;
   std::unique_ptr<EffectEditor> PopulateUI(const EffectPlugin &plugin,
      ShuttleGui &S, EffectInstance &instance, EffectSettingsAccess &access,
      const EffectOutputs *pOutputs) const override;
   bool CloseUI() const override;

   bool CanExportPresets() const override;
   void ExportPresets(
      const EffectPlugin &plugin, const EffectSettings &settings)
   const override;
   OptionalMessage ImportPresets(
      const EffectPlugin &plugin, EffectSettings &settings) const override;

   bool HasOptions() const override;
   void ShowOptions(const EffectPlugin &plugin) const override;

   // AudioUnitEffect implementation

private:
   static RegistryPath ChoosePresetKey(const EffectSettings &settings);
   static RegistryPath FindPresetKey(const CommandParameters & parms);

   TranslatableString Export(
      const AudioUnitEffectSettings &settings, const wxString & path) const;
   TranslatableString Import(
      AudioUnitEffectSettings &settings, const wxString & path) const;
   /*!
    @param path only for formatting error messages
    @return error message
    */
   TranslatableString SaveBlobToConfig(const RegistryPath &group,
      const wxString &path, const void *blob, size_t len,
      bool allowEmpty = true) const;

   void GetChannelCounts();

   bool MigrateOldConfigFile(
      const RegistryPath & group, EffectSettings &settings) const;
   OptionalMessage
      LoadPreset(const RegistryPath & group, EffectSettings &settings) const;
   bool SavePreset(const RegistryPath & group,
      const AudioUnitEffectSettings &settings) const;

#if defined(HAVE_AUDIOUNIT_BASIC_SUPPORT)
   bool CreatePlain(wxWindow *parent);
#endif

private:
   //! Will never be called
   virtual std::unique_ptr<EffectEditor> MakeEditor(
      ShuttleGui & S, EffectInstance &instance, EffectSettingsAccess &access,
      const EffectOutputs *pOutputs) const final;

   const PluginPath mPath;
   const wxString mName;
   const wxString mVendor;

   bool mInteractive{ false };
};

#endif

#endif
