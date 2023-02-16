/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioUnitEffectBase.cpp

  Dominic Mazzoni
  Leland Lucius

  Paul Licameli split from AudioUnitEffect

*******************************************************************//**

\class AudioUnitEffectBase
\brief An Effect class that handles a wide range of effects.  ??Mac only??

*//*******************************************************************/

#if USE_AUDIO_UNITS
#include "AudioUnitEffectBase.h"
#include "AudioUnitInstance.h"
#include "ConfigInterface.h"

#include <wx/ffile.h>
#include <wx/osx/core/private.h>

//
// When a plug-in's state is saved to the settings file (as a preset),
// it is in binary and gets base64 encoded before storing.
//
// When exporting, save as XML without base64 encoding.
//
// The advantages of XML format is less chance of failures occurring
// when exporting.  But, it can take a bit more space per preset int
// the Audacity settings file.
//
// Using binary for now.  Use kCFPropertyListXMLFormat_v1_0 if XML
// format is desired.
//
#define PRESET_FORMAT kCFPropertyListBinaryFormat_v1_0

// Name of the settings key to use for the above value
#define PRESET_KEY wxT("Data")

// Where the presets are located
#define PRESET_LOCAL_PATH wxT("/Library/Audio/Presets")
#define PRESET_USER_PATH wxT("~/Library/Audio/Presets")

TranslatableString AudioUnitEffectBase::SaveBlobToConfig(
   const RegistryPath &group, const wxString &path,
   const void *blob, size_t len, bool allowEmpty) const
{
   // Base64 encode the returned binary property list
   auto parms = wxBase64Encode(blob, len);
   if (!allowEmpty && parms.IsEmpty())
      return XO("Failed to encode preset from \"%s\"").Format(path);

   // And write it to the config
   if (!SetConfig(*this, PluginSettings::Private, group, PRESET_KEY, parms))
      return XO("Unable to store preset in config file");
   return {};
}

///////////////////////////////////////////////////////////////////////////////
//
// AudioUnitEffect
//
///////////////////////////////////////////////////////////////////////////////

std::unique_ptr<AudioUnitEffectBase>
   AudioUnitEffectBase::DefaultEffectFactory(const PluginPath & path,
      const wxString & name, AudioComponent component)
{
   return std::make_unique<AudioUnitEffectBase>(path, name, component);
}

AudioUnitEffectBase::AudioUnitEffectBase(const PluginPath & path,
   const wxString & name, AudioComponent component,
   Parameters *pParameters, AudioUnitEffectBase *master
)  : AudioUnitWrapper{ component, pParameters }
   , mPath{ path }
   , mName{ name.AfterFirst(wxT(':')).Trim(true).Trim(false) }
   , mVendor{ name.BeforeFirst(wxT(':')).Trim(true).Trim(false) }
{
}

AudioUnitEffectBase::~AudioUnitEffectBase() = default;

// ============================================================================
// ComponentInterface implementation
// ============================================================================

PluginPath AudioUnitEffectBase::GetPath() const
{
   return mPath;
}

ComponentInterfaceSymbol AudioUnitEffectBase::GetSymbol() const
{
   return mName;
}

VendorSymbol AudioUnitEffectBase::GetVendor() const
{
   return { mVendor };
}

wxString AudioUnitEffectBase::GetVersion() const
{
   UInt32 version;

   OSStatus result = AudioComponentGetVersion(mComponent, &version);

   return wxString::Format(wxT("%d.%d.%d"),
                           (version >> 16) & 0xffff,
                           (version >> 8) & 0xff,
                           version & 0xff);
}

TranslatableString AudioUnitEffectBase::GetDescription() const
{
   /* i18n-hint: Can mean "not available," "not applicable," "no answer" */
   return XO("n/a");
}

// ============================================================================
// EffectDefinitionInterface implementation
// ============================================================================

EffectType AudioUnitEffectBase::GetType() const
{
   if (mAudioIns == 0 && mAudioOuts == 0)
   {
      return EffectTypeNone;
   }

   if (mAudioIns == 0)
   {
      return EffectTypeGenerate;
   }

   if (mAudioOuts == 0)
   {
      return EffectTypeAnalyze;
   }

   return EffectTypeProcess;
}

EffectFamilySymbol AudioUnitEffectBase::GetFamily() const
{
   return AUDIOUNITEFFECTS_FAMILY;
}

bool AudioUnitEffectBase::IsInteractive() const
{
   return mInteractive;
}

bool AudioUnitEffectBase::IsDefault() const
{
   return false;
}

auto AudioUnitEffectBase::RealtimeSupport() const -> RealtimeSince
{
   return GetType() == EffectTypeProcess
      ? RealtimeSince::After_3_1
      : RealtimeSince::Never;
}

bool AudioUnitEffectBase::SupportsAutomation() const
{
   bool supports = false;
   ForEachParameter(
   [&supports](const ParameterInfo &pi, AudioUnitParameterID) {
      if (pi.mInfo.flags & kAudioUnitParameterFlag_IsWritable)
         supports = true;
      // Search only until we find one, that's all we need to know
      return !supports;
   });
   return supports;
}

std::shared_ptr<EffectInstance> AudioUnitEffectBase::MakeInstance() const
{
   bool useLatency;
   GetConfig(*this, PluginSettings::Shared, OptionsKey, UseLatencyKey,
      useLatency, true);

   return std::make_shared<AudioUnitInstance>(*this, mComponent, mParameters,
      GetSymbol().Internal(), mAudioIns, mAudioOuts, useLatency);
}

bool AudioUnitEffectBase::InitializePlugin()
{
   // To implement the services of EffectPlugin -- such as, a query of the
   // set of effect parameters, so that we can implement MakeSettings -- we
   // also need what is called an AudioComponentInstance, also called an
   // AudioUnit.
   // It's not just for implementing EffectInstance.  AudioUnits is unlike other
   // third party effect families that distinguish the notions of plug-in and
   // instance.

   GetChannelCounts();

   if (!CreateAudioUnit())
      return false;

   // Use an arbitrary rate while completing the discovery of channel support
   if (!SetRateAndChannels(44100.0, GetSymbol().Internal()))
      return false;

   // Determine interactivity
   mInteractive = (Count(mParameters) > 0);
   if (!mInteractive) {
      // Check for a Cocoa UI
      // This could retrieve a variable-size property, but we only look at
      // the first element.
      AudioUnitCocoaViewInfo cocoaViewInfo;
      mInteractive =
         !GetFixedSizeProperty(kAudioUnitProperty_CocoaUI, cocoaViewInfo);
      if (!mInteractive) {
         // Check for a Carbon UI
         // This could retrieve a variable sized array but we only need the
         // first
         AudioComponentDescription compDesc;
         mInteractive = !GetFixedSizeProperty(
            kAudioUnitProperty_GetUIComponentList, compDesc);
      }
   }

   return true;
}

#if 0
size_t AudioUnitInstance::GetTailSize() const
{
   // Retrieve the tail time
   Float64 tailTime = 0.0;
   if (!GetFixedSizeProperty(kAudioUnitProperty_TailTime, tailTime))
      return tailTime * mSampleRate;
   return 0;
}
#endif

// Don't use the template-generated MakeSettings(), which default-constructs
// the structure.  Instead allocate a number of values chosen by the plug-in
EffectSettings AudioUnitEffectBase::MakeSettings() const
{
   AudioUnitEffectSettings settings;
   FetchSettings(settings, true);
   return EffectSettings::Make<AudioUnitEffectSettings>(std::move(settings));
}

bool AudioUnitEffectBase::CopySettingsContents(
   const EffectSettings &, EffectSettings &) const
{
   // Not needed -- rely on EffectInstance::Message instead
   return true;
}

constexpr auto PresetStr = "_PRESET";

RegistryPath AudioUnitEffectBase::ChoosePresetKey(
   const EffectSettings &settings)
{
   // Find a key to use for the preset that does not collide with any
   // parameter name
   wxString result = PresetStr;

   // That string probably works but be sure
   const auto &map = GetSettings(settings).values;
   using Pair = decltype(*map.begin());
   while (std::any_of(map.begin(), map.end(), [&](Pair &pair){
      return pair.second && pair.second->first == result;
   }))
      result += "_";

   return result;
}

RegistryPath AudioUnitEffectBase::FindPresetKey(const CommandParameters & parms)
{
   RegistryPath result;
   auto len = strlen(PresetStr);
   if (auto [index, key] = std::tuple(0L, wxString{})
       ; parms.GetFirstEntry(key, index)
   ) do {
      if (key.StartsWith(PresetStr)
          && key.Mid(len).find_first_not_of("_") == wxString::npos
          && key.length() > result.length())
         result = key;
   } while(parms.GetNextEntry(key, index));
   return result;
}

bool AudioUnitEffectBase::SaveSettings(
   const EffectSettings &settings, CommandParameters & parms) const
{
   const auto &mySettings = GetSettings(settings);
   if (mySettings.mPresetNumber) {
      const auto key = ChoosePresetKey(settings);
      parms.Write(key, *mySettings.mPresetNumber);
   }

   // Save settings into CommandParameters
   // Iterate the map only, not using any AudioUnit handles
   for (auto &[ID, pPair] : mySettings.values)
      if (pPair)
         // Write names, not numbers, as keys in the config file
         parms.Write(pPair->first, pPair->second);
   return true;
}

bool AudioUnitEffectBase::LoadSettings(
   const CommandParameters & parms, EffectSettings &settings) const
{
   // First clean all settings, in case any are not defined in parms
   auto &mySettings = GetSettings(settings);
   mySettings.ResetValues();
   auto &map = mySettings.values;

   // Reload preset first
   if (auto presetKey = FindPresetKey(parms); !presetKey.empty()) {
      SInt32 value = 0;
      if (parms.Read(presetKey, &value))
         AudioUnitWrapper::LoadFactoryPreset(*this, value, &settings);
   }

   // Load settings from CommandParameters
   // Iterate the config only, not using any AudioUnit handles
   if (auto [index, key, value] = std::tuple(
         0L, wxString{}, AudioUnitParameterValue{})
       ; parms.GetFirstEntry(key, index)
   ) do {
      if (auto pKey = ParameterInfo::ParseKey(key)
         ; pKey && parms.Read(key, &value)
      )
         map[*pKey].emplace(mySettings.Intern(key), value);
   } while(parms.GetNextEntry(key, index));
   return true;
}

OptionalMessage AudioUnitEffectBase::LoadUserPreset(
   const RegistryPath & name, EffectSettings &settings) const
{
   // To do: externalize state so const_cast isn't needed
   return const_cast<AudioUnitEffectBase*>(this)->LoadPreset(name, settings);
}

bool AudioUnitEffectBase::SaveUserPreset(
   const RegistryPath & name, const EffectSettings &settings) const
{
   return SavePreset(name, GetSettings(settings));
}

OptionalMessage
AudioUnitEffectBase::LoadFactoryPreset(int id, EffectSettings &settings) const
{
   if (AudioUnitWrapper::LoadFactoryPreset(*this, id, &settings))
      return { nullptr };
   return {};
}

RegistryPaths AudioUnitEffectBase::GetFactoryPresets() const
{
   RegistryPaths presets;

   // Retrieve the list of factory presets
   CF_ptr<CFArrayRef> array;
   if (!GetFixedSizeProperty(kAudioUnitProperty_FactoryPresets, array))
      for (CFIndex i = 0, cnt = CFArrayGetCount(array.get()); i < cnt; ++i)
         presets.push_back(wxCFStringRef::AsString(
            static_cast<const AUPreset*>(CFArrayGetValueAtIndex(array.get(), i))
               ->presetName));
   return presets;
}

bool AudioUnitEffectBase::CanExportPresets() const
{
   return true;
}

bool AudioUnitEffectBase::HasOptions() const
{
   return true;
}

// ============================================================================
// AudioUnitEffect Implementation
// ============================================================================

bool AudioUnitEffectBase::MigrateOldConfigFile(
   const RegistryPath & group, EffectSettings &settings) const
{
   // Migration of very old format configuration file, should not normally
   // happen and perhaps this code can be abandoned
   // Attempt to load old preset parameters and resave using new method
   constexpr auto oldKey = L"Parameters";
   wxString parms;
   if (GetConfig(*this, PluginSettings::Private,
      group, oldKey, parms, wxEmptyString)) {
      CommandParameters eap;
      if (eap.SetParameters(parms))
         if (LoadSettings(eap, settings))
            if (SavePreset(group, GetSettings(settings)))
               RemoveConfig(*this, PluginSettings::Private, group, oldKey);
      return true;
   }
   return false;
}

OptionalMessage AudioUnitEffectBase::LoadPreset(
   const RegistryPath & group, EffectSettings &settings) const
{
   if (MigrateOldConfigFile(group, settings))
      return { nullptr };

   if (AudioUnitWrapper::LoadPreset(*this, group, settings))
      return { nullptr };
   return {};
}

bool AudioUnitEffectBase::SavePreset(
   const RegistryPath & group, const AudioUnitEffectSettings &settings) const
{
   wxCFStringRef cfname(wxFileNameFromPath(group));
   const auto &[data, _] = MakeBlob(*this, settings, cfname, true);
   if (!data)
      return false;

   // Nothing to do if we don't have any data
   if (const auto length = CFDataGetLength(data.get())) {
      auto error =
         SaveBlobToConfig(group, {}, CFDataGetBytePtr(data.get()), length);
      if (!error.empty())
         return false;
   }
   return true;
}

TranslatableString AudioUnitEffectBase::Export(
   const AudioUnitEffectSettings &settings, const wxString & path) const
{
   // Create the file
   wxFFile f(path, wxT("wb"));
   if (!f.IsOpened())
      return XO("Couldn't open \"%s\"").Format(path);

   // First set the name of the preset
   wxCFStringRef cfname(wxFileName(path).GetName());

   const auto &[data, message] = MakeBlob(*this, settings, cfname, false);
   if (!data || !message.empty())
      return message;

   // Write XML data
   auto length = CFDataGetLength(data.get());
   if (f.Write(CFDataGetBytePtr(data.get()), length) != length || f.Error())
      return XO("Failed to write XML preset to \"%s\"").Format(path);

   f.Close();
   return {};
}

TranslatableString AudioUnitEffectBase::Import(
   AudioUnitEffectSettings &settings, const wxString & path) const
{
   // Open the preset
   wxFFile f(path, wxT("r"));
   if (!f.IsOpened())
      return XO("Couldn't open \"%s\"").Format(path);

   // Load it into the buffer
   size_t len = f.Length();
   wxMemoryBuffer buf(len);
   if (f.Read(buf.GetData(), len) != len || f.Error())
      return XO("Unable to read the preset from \"%s\"").Format(path);
   buf.SetDataLen(len);

   const auto error = InterpretBlob(settings, path, buf);
   if (!error.empty())
      return error;

   return {};
}

void AudioUnitEffectBase::GetChannelCounts()
{
   // Does AU have channel info
   PackedArray::Ptr<AUChannelInfo> info;
   if (GetVariableSizeProperty(kAudioUnitProperty_SupportedNumChannels, info)) {
      // None supplied.  Apparently all FX type units can do any number of INs
      // and OUTs as long as they are the same number.  In this case, we'll
      // just say stereo.
      //
      // We should probably check to make sure we're dealing with an FX type.
      mAudioIns = 2;
      mAudioOuts = 2;
      return;
   }

   // This is where it gets weird...not sure what is the best
   // way to do this really.  If we knew how many ins/outs we
   // really needed, we could make a better choice.

   bool haven2m = false;   // nothing -> mono
   bool haven2s = false;   // nothing -> stereo
   bool havem2n = false;   // mono -> nothing
   bool haves2n = false;   // stereo -> nothing
   bool havem2m = false;   // mono -> mono
   bool haves2s = false;   // stereo -> stereo
   bool havem2s = false;   // mono -> stereo
   bool haves2m = false;   // stereo -> mono

   mAudioIns = 2;
   mAudioOuts = 2;

   // Look only for exact channel constraints
   for (auto &ci : info) {
      int ic = ci.inChannels;
      int oc = ci.outChannels;

      if (ic < 0 && oc >= 0)
         ic = 2;
      else if (ic >= 0 && oc < 0)
         oc = 2;
      else if (ic < 0 && oc < 0) {
         ic = 2;
         oc = 2;
      }

      if (ic == 2 && oc == 2)
         haves2s = true;
      else if (ic == 1 && oc == 1)
         havem2m = true;
      else if (ic == 1 && oc == 2)
         havem2s = true;
      else if (ic == 2 && oc == 1)
         haves2m = true;
      else if (ic == 0 && oc == 2)
         haven2s = true;
      else if (ic == 0 && oc == 1)
         haven2m = true;
      else if (ic == 1 && oc == 0)
         havem2n = true;
      else if (ic == 2 && oc == 0)
         haves2n = true;
   }

   if (haves2s) {
      mAudioIns = 2;
      mAudioOuts = 2;
   }
   else if (havem2m) {
      mAudioIns = 1;
      mAudioOuts = 1;
   }
   else if (havem2s) {
      mAudioIns = 1;
      mAudioOuts = 2;
   }
   else if (haves2m) {
      mAudioIns = 2;
      mAudioOuts = 1;
   }
   else if (haven2m) {
      mAudioIns = 0;
      mAudioOuts = 1;
   }
   else if (haven2s) {
      mAudioIns = 0;
      mAudioOuts = 2;
   }
   else if (haves2n) {
      mAudioIns = 2;
      mAudioOuts = 0;
   }
   else if (havem2n) {
      mAudioIns = 1;
      mAudioOuts = 0;
   }

   return;
}
#endif
