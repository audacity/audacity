/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioUnitEffectBase.h

  Dominic Mazzoni
  Leland Lucius

  Paul Licameli split from AudioUnitEffect.h

**********************************************************************/
#ifndef AUDACITY_AUDIOUNIT_EFFECT_BASE_H
#define AUDACITY_AUDIOUNIT_EFFECT_BASE_H

#if USE_AUDIO_UNITS

#include "AudioUnitWrapper.h"
#include "GlobalVariable.h"
#include "PerTrackEffect.h"

constexpr auto OptionsKey = L"Options";
constexpr auto UseLatencyKey = L"UseLatency";

#define AUDIOUNITEFFECTS_VERSION wxT("1.0.0.0")
/* i18n-hint: the name of an Apple audio software protocol */
#define AUDIOUNITEFFECTS_FAMILY EffectFamilySymbol{ wxT("AudioUnit"), XO("Audio Unit") }

class AudioUnitEffectBase
   : public PerTrackEffect
   , public AudioUnitWrapper
{
public:
   using FactoryType =
      std::unique_ptr<AudioUnitEffectBase>(const PluginPath & path,
         const wxString & name, AudioComponent component);

   static std::unique_ptr<AudioUnitEffectBase>
      DefaultEffectFactory(const PluginPath & path,
         const wxString & name, AudioComponent component);

   //! Global hook making AudioIOStartStreamOptions for a project, which
   //! has a non-trivial default implementation
   struct Factory : GlobalFactoryHook< Factory,
      FactoryType,
      DefaultEffectFactory // default installed implementation
   >{};

   using Parameters = PackedArray::Ptr<const AudioUnitParameterID>;

   AudioUnitEffectBase(const PluginPath & path,
      const wxString & name, AudioComponent component,
      Parameters *pParameters = nullptr,
      AudioUnitEffectBase *master = nullptr);
   ~AudioUnitEffectBase() override;

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

   bool InitializePlugin();

   std::shared_ptr<EffectInstance> MakeInstance() const override;

   bool CanExportPresets() const override;

   bool HasOptions() const override;

   // AudioUnitEffect implementation

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

protected:
   const PluginPath mPath;
   const wxString mName;
   const wxString mVendor;

   bool mInteractive{ false };
};

#endif

#endif
