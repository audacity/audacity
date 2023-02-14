/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioUnitEffect.cpp

  Dominic Mazzoni
  Leland Lucius

*******************************************************************//**

\class AudioUnitEffect
\brief An Effect class that handles a wide range of effects.  ??Mac only??

*//*******************************************************************/

#if USE_AUDIO_UNITS
#include "AudioUnitEffect.h"
#include "AudioUnitEffectOptionsDialog.h"
#include "AudioUnitInstance.h"
#include "AudioUnitValidator.h"
#include "SampleCount.h"
#include "ConfigInterface.h"

#include <optional>
#include <wx/defs.h>
#include <wx/base64.h>
#include <wx/control.h>
#include <wx/crt.h>
#include <wx/dir.h>
#include <wx/ffile.h>

#ifdef __WXMAC__
#include <wx/evtloop.h>
#endif

#include <wx/filename.h>
#include <wx/frame.h>
#include <wx/listctrl.h>
#include <wx/log.h>
#include <wx/settings.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/osx/core/private.h>

#include "../../SelectFile.h"
#include "ShuttleGui.h"
#include "../../widgets/AudacityMessageBox.h"
#include "valnum.h"

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

TranslatableString AudioUnitEffect::SaveBlobToConfig(
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

AudioUnitEffect::AudioUnitEffect(const PluginPath & path,
   const wxString & name, AudioComponent component,
   Parameters *pParameters, AudioUnitEffect *master
)  : AudioUnitWrapper{ component, pParameters }
   , mPath{ path }
   , mName{ name.AfterFirst(wxT(':')).Trim(true).Trim(false) }
   , mVendor{ name.BeforeFirst(wxT(':')).Trim(true).Trim(false) }
{
}

AudioUnitEffect::~AudioUnitEffect()
{
}

// ============================================================================
// ComponentInterface implementation
// ============================================================================

PluginPath AudioUnitEffect::GetPath() const
{
   return mPath;
}

ComponentInterfaceSymbol AudioUnitEffect::GetSymbol() const
{
   return mName;
}

VendorSymbol AudioUnitEffect::GetVendor() const
{
   return { mVendor };
}

wxString AudioUnitEffect::GetVersion() const
{
   UInt32 version;

   OSStatus result = AudioComponentGetVersion(mComponent, &version);

   return wxString::Format(wxT("%d.%d.%d"),
                           (version >> 16) & 0xffff,
                           (version >> 8) & 0xff,
                           version & 0xff);
}

TranslatableString AudioUnitEffect::GetDescription() const
{
   /* i18n-hint: Can mean "not available," "not applicable," "no answer" */
   return XO("n/a");
}

// ============================================================================
// EffectDefinitionInterface implementation
// ============================================================================

EffectType AudioUnitEffect::GetType() const
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

EffectFamilySymbol AudioUnitEffect::GetFamily() const
{
   return AUDIOUNITEFFECTS_FAMILY;
}

bool AudioUnitEffect::IsInteractive() const
{
   return mInteractive;
}

bool AudioUnitEffect::IsDefault() const
{
   return false;
}

auto AudioUnitEffect::RealtimeSupport() const -> RealtimeSince
{
   return GetType() == EffectTypeProcess
      ? RealtimeSince::After_3_1
      : RealtimeSince::Never;
}

bool AudioUnitEffect::SupportsAutomation() const
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

std::shared_ptr<EffectInstance> AudioUnitEffect::MakeInstance() const
{
   return std::make_shared<AudioUnitInstance>(*this, mComponent, mParameters,
      GetSymbol().Internal(), mAudioIns, mAudioOuts, mUseLatency);
}

constexpr auto OptionsKey = L"Options";
constexpr auto UseLatencyKey = L"UseLatency";
constexpr auto UITypeKey = L"UIType";

bool AudioUnitEffect::InitializePlugin()
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

bool AudioUnitEffect::FullyInitializePlugin()
{
   if (!InitializePlugin())
      return false;

   // Reading these values from the config file can't be done in the PluginHost
   // process but isn't needed only for plugin discovery.

   // Consult preferences
   // Decide mUseLatency, which affects GetLatency(), which is actually used
   // so far only in destructive effect processing
   GetConfig(*this, PluginSettings::Shared, OptionsKey, UseLatencyKey,
      mUseLatency, true);
   // Decide whether to build plain or fancy user interfaces
   GetConfig(*this, PluginSettings::Shared, OptionsKey, UITypeKey,
      mUIType, FullValue.MSGID().GET() /* Config stores un-localized string */);

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

int AudioUnitEffect::ShowClientInterface(wxWindow &parent, wxDialog &dialog,
   EffectUIValidator *, bool forceModal)
{
   if ((SupportsRealtime() || GetType() == EffectTypeAnalyze) && !forceModal) {
      dialog.Show();
      return 0;
   }
   return dialog.ShowModal();
}

// Don't use the template-generated MakeSettings(), which default-constructs
// the structure.  Instead allocate a number of values chosen by the plug-in
EffectSettings AudioUnitEffect::MakeSettings() const
{
   AudioUnitEffectSettings settings;
   FetchSettings(settings, true);
   return EffectSettings::Make<AudioUnitEffectSettings>(std::move(settings));
}

bool AudioUnitEffect::CopySettingsContents(
   const EffectSettings &, EffectSettings &) const
{
   // Not needed -- rely on EffectInstance::Message instead
   return true;
}

constexpr auto PresetStr = "_PRESET";

RegistryPath AudioUnitEffect::ChoosePresetKey(
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

RegistryPath AudioUnitEffect::FindPresetKey(const CommandParameters & parms)
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

bool AudioUnitEffect::SaveSettings(
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

bool AudioUnitEffect::LoadSettings(
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

OptionalMessage AudioUnitEffect::LoadUserPreset(
   const RegistryPath & name, EffectSettings &settings) const
{
   // To do: externalize state so const_cast isn't needed
   return const_cast<AudioUnitEffect*>(this)->LoadPreset(name, settings);
}

bool AudioUnitEffect::SaveUserPreset(
   const RegistryPath & name, const EffectSettings &settings) const
{
   return SavePreset(name, GetSettings(settings));
}

OptionalMessage
AudioUnitEffect::LoadFactoryPreset(int id, EffectSettings &settings) const
{
   if (AudioUnitWrapper::LoadFactoryPreset(*this, id, &settings))
      return { nullptr };
   return {};
}

RegistryPaths AudioUnitEffect::GetFactoryPresets() const
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

// ============================================================================
// EffectUIClientInterface Implementation
// ============================================================================

std::unique_ptr<EffectUIValidator> AudioUnitEffect::PopulateUI(ShuttleGui &S,
   EffectInstance &instance, EffectSettingsAccess &access,
   const EffectOutputs *)
{
   mParent = S.GetParent();
   return AudioUnitValidator::Create(*this, S, mUIType, instance, access);
}

#if defined(HAVE_AUDIOUNIT_BASIC_SUPPORT)
bool AudioUnitEffect::CreatePlain(wxWindow *parent)
{
   // TODO???  Never implemented...
   return false;
}
#endif

bool AudioUnitEffect::CloseUI()
{
#ifdef __WXMAC__
#ifdef __WX_EVTLOOP_BUSY_WAITING__
   wxEventLoop::SetBusyWaiting(false);
#endif
#endif
   mParent = nullptr;
   return true;
}

bool AudioUnitEffect::CanExportPresets()
{
   return true;
}

void AudioUnitEffect::ExportPresets(const EffectSettings &settings) const
{
   // Generate the user domain path
   wxFileName fn;
   fn.SetPath(PRESET_USER_PATH);
   fn.AppendDir(mVendor);
   fn.AppendDir(mName);
   fn.Normalize();
   FilePath path = fn.GetFullPath();

   if (!fn.Mkdir(fn.GetFullPath(), 0755, wxPATH_MKDIR_FULL)) {
      wxLogError(wxT("Couldn't create the \"%s\" directory"), fn.GetPath());
      return;
   }

   // Ask the user for the name to use
   //
   // Passing a valid parent will cause some effects dialogs to malfunction
   // upon returning from the SelectFile().
   path = SelectFile(FileNames::Operation::_None,
      XO("Export Audio Unit Preset As %s:").Format(fn.GetFullPath()),
      fn.GetFullPath(),
      wxEmptyString,
      wxT("aupreset"),
      {
        { XO("Standard Audio Unit preset file"), { wxT("aupreset") }, true },
      },
      wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER,
      NULL);

   // User canceled...
   if (path.empty())
      return;

   auto msg = Export(GetSettings(settings), path);
   if (!msg.empty())
      AudacityMessageBox(
         XO("Could not export \"%s\" preset\n\n%s").Format(path, msg),
         XO("Export Audio Unit Presets"),
         wxOK | wxCENTRE,
         mParent);
}

OptionalMessage AudioUnitEffect::ImportPresets(EffectSettings &settings)
{
   // Generate the user domain path
   wxFileName fn;
   fn.SetPath(PRESET_USER_PATH);
   fn.AppendDir(mVendor);
   fn.AppendDir(mName);
   fn.Normalize();
   FilePath path = fn.GetFullPath();

   // Ask the user for the name to use
   //
   // Passing a valid parent will cause some effects dialogs to malfunction
   // upon returning from the SelectFile().
   path = SelectFile(FileNames::Operation::_None,
      XO("Import Audio Unit Preset As %s:").Format(fn.GetFullPath()),
      fn.GetFullPath(), wxEmptyString, wxT("aupreset"),
      {
        { XO("Standard Audio Unit preset file"), { wxT("aupreset") }, true },
      },
      wxFD_OPEN | wxRESIZE_BORDER,
      nullptr);

   // User canceled...
   if (path.empty())
      return {};

   auto msg = Import(GetSettings(settings), path);
   if (!msg.empty()) {
      AudacityMessageBox(
         XO("Could not import \"%s\" preset\n\n%s").Format(path, msg),
         XO("Import Audio Unit Presets"),
         wxOK | wxCENTRE,
         mParent);
      return {};
   }

   return { nullptr };
}

bool AudioUnitEffect::HasOptions()
{
   return true;
}

void AudioUnitEffect::ShowOptions()
{
   AudioUnitEffectOptionsDialog dlg(mParent, mUseLatency, mUIType);
   if (dlg.ShowModal()) {
      // Save changed values to the config file
      SetConfig(*this, PluginSettings::Shared, OptionsKey, UseLatencyKey,
         mUseLatency);
      SetConfig(*this, PluginSettings::Shared, OptionsKey, UITypeKey, mUIType);
   }
}

// ============================================================================
// AudioUnitEffect Implementation
// ============================================================================

bool AudioUnitEffect::MigrateOldConfigFile(
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

OptionalMessage AudioUnitEffect::LoadPreset(
   const RegistryPath & group, EffectSettings &settings) const
{
   if (MigrateOldConfigFile(group, settings))
      return { nullptr };

   if (AudioUnitWrapper::LoadPreset(*this, group, settings))
      return { nullptr };
   return {};
}

bool AudioUnitEffect::SavePreset(
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

TranslatableString AudioUnitEffect::Export(
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

TranslatableString AudioUnitEffect::Import(
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

void AudioUnitEffect::GetChannelCounts()
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
