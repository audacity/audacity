/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioUnitEffectsModule.cpp

  Dominic Mazzoni
  Leland Lucius

  Paul Licameli split from AudioUnitEffect.

**********************************************************************/

#if USE_AUDIO_UNITS
#include "AudioUnitEffectsModule.h"

#include "AudioUnitEffect.h"
#include "ModuleManager.h"
#include <wx/log.h>
#include <wx/tokenzr.h>

#include "wxArrayStringEx.h"
#include <wx/osx/core/private.h>

static const struct
{
   OSType componentManufacturer;
   OSType componentType;
   OSType componentSubType;
}
BlackList[] =
{
   { 'appl', 'augn', 'afpl' },   // Apple: AUAudioFilePlayer
   { 'appl', 'augn', 'sspl' },   // Apple: AUScheduledSoundPlayer
   { 'appl', 'augn', 'ttsp' },   // Apple: AUSpeechSynthesis
   { 'appl', 'augn', 'nrcv' },   // Apple: AUNetReceive
   { 'appl', 'aumx', '3dmx' },   // Apple: AUMixer3D
   { 'appl', 'aumx', 'mspl' },   // Apple: AUMultiSplitter
   { 'appl', 'aumx', 'mcmx' },   // Apple: AUMultiChannelMixer
   { 'appl', 'aumx', 'mxmx' },   // Apple: AUMatrixMixer
   { 'appl', 'aumx', 'smxr' },   // Apple: AUMixer
   { 'Ignt', 'aufx', 'PTQX' },   // Ignite Amps PTEq-X
};

// ============================================================================
// Module registration entry point
//
// This is the symbol that Audacity looks for when the module is built as a
// dynamic library.
//
// When the module is builtin to Audacity, we use the same function, but it is
// declared static so as not to clash with other builtin modules.
// ============================================================================
DECLARE_PROVIDER_ENTRY(AudacityModule)
{
   // Create and register the importer
   // Trust the module manager not to leak this
   return std::make_unique<AudioUnitEffectsModule>();
}

// ============================================================================
// Register this as a builtin module
// ============================================================================
DECLARE_BUILTIN_PROVIDER(AudioUnitEffectsBuiltin);

///////////////////////////////////////////////////////////////////////////////
//
// AudioUnitEffectsModule
//
///////////////////////////////////////////////////////////////////////////////


namespace
{

wxString FromOSType(OSType type)
{
   OSType rev = (type & 0xff000000) >> 24 |
                (type & 0x00ff0000) >> 8  |
                (type & 0x0000ff00) << 8  |
                (type & 0x000000ff) << 24;
   
   return wxString::FromUTF8(reinterpret_cast<char *>(&rev), 4);
}

OSType ToOSType(const wxString & type)
{
   wxCharBuffer buf = type.ToUTF8();

   OSType rev = ((unsigned char)buf.data()[0]) << 24 |
                ((unsigned char)buf.data()[1]) << 16 |
                ((unsigned char)buf.data()[2]) << 8 |
                ((unsigned char)buf.data()[3]);

   return rev;
}

AudioComponent FindAudioUnit(const PluginPath & path,
                                                     wxString & name)
{
   wxStringTokenizer tokens(path, wxT("/"));

   AudioComponentDescription desc;

   desc.componentManufacturer = ToOSType(tokens.GetNextToken());
   desc.componentType = ToOSType(tokens.GetNextToken());
   desc.componentSubType = ToOSType(tokens.GetNextToken());
   desc.componentFlags = 0;
   desc.componentFlagsMask = 0;

   name = tokens.GetNextToken();
   return AudioComponentFindNext(NULL, &desc);
}

}

AudioUnitEffectsModule::AudioUnitEffectsModule()
{
}

AudioUnitEffectsModule::~AudioUnitEffectsModule()
{
}

// ============================================================================
// ComponentInterface implementation
// ============================================================================

PluginPath AudioUnitEffectsModule::GetPath() const
{
   return {};
}

ComponentInterfaceSymbol AudioUnitEffectsModule::GetSymbol() const
{
   /* i18n-hint: Audio Unit is the name of an Apple audio software protocol */
   return XO("Audio Unit Effects");
}

VendorSymbol AudioUnitEffectsModule::GetVendor() const
{
   return XO("The Audacity Team");
}

wxString AudioUnitEffectsModule::GetVersion() const
{
   // This "may" be different if this were to be maintained as a separate DLL
   return AUDIOUNITEFFECTS_VERSION;
}

TranslatableString AudioUnitEffectsModule::GetDescription() const
{
   return XO("Provides Audio Unit Effects support to Audacity");
}

// ============================================================================
// PluginProvider implementation
// ============================================================================

const FileExtensions &AudioUnitEffectsModule::GetFileExtensions()
{
   static FileExtensions result{{ _T("au") }};
   return result;
}

bool AudioUnitEffectsModule::Initialize()
{
   // Nothing to do here
   return true;
}

void AudioUnitEffectsModule::Terminate()
{
   // Nothing to do here
   return;
}

EffectFamilySymbol AudioUnitEffectsModule::GetOptionalFamilySymbol()
{
#if USE_AUDIO_UNITS
   return AUDIOUNITEFFECTS_FAMILY;
#else
   return {};
#endif
}

void AudioUnitEffectsModule::AutoRegisterPlugins(PluginManagerInterface &)
{
}

PluginPaths AudioUnitEffectsModule::FindModulePaths(PluginManagerInterface &)
{
   PluginPaths effects;

   LoadAudioUnitsOfType(kAudioUnitType_Effect, effects);
   LoadAudioUnitsOfType(kAudioUnitType_Generator, effects);
   LoadAudioUnitsOfType(kAudioUnitType_Mixer, effects);
   LoadAudioUnitsOfType(kAudioUnitType_MusicEffect, effects);
   LoadAudioUnitsOfType(kAudioUnitType_Panner, effects);

   return effects;
}

unsigned AudioUnitEffectsModule::DiscoverPluginsAtPath(
   const PluginPath & path, TranslatableString &errMsg,
   const RegistrationCallback &callback)
{
   errMsg = {};
   wxString name;
   AudioComponent component = FindAudioUnit(path, name);
   if (component == NULL)
   {
      errMsg = XO("Could not find component");
      return 0;
   }

   AudioUnitEffect effect(path, name, component);
   if (!effect.InitializePlugin())
   {
      // TODO:  Is it worth it to discriminate all the ways SetHost might
      // return false?
      errMsg = XO("Could not initialize component");
      return 0;
   }

   if (callback)
   {
      callback(this, &effect);
   }

   return 1;
}

std::unique_ptr<ComponentInterface>
AudioUnitEffectsModule::LoadPlugin(const PluginPath & path)
{
   // Acquires a resource for the application.
   if (wxString name; auto component = FindAudioUnit(path, name)) {
      auto result = std::make_unique<AudioUnitEffect>(path, name, component);
      result->InitializePlugin();
      return result;
   }
   return nullptr;
}

bool AudioUnitEffectsModule::CheckPluginExist(const PluginPath& path) const
{
   wxString unused;
   return FindAudioUnit(path, unused) != nullptr;
}

// ============================================================================
// AudioUnitEffectsModule implementation
// ============================================================================

void AudioUnitEffectsModule::LoadAudioUnitsOfType(OSType inAUType,
                                                  PluginPaths & effects)
{
   AudioComponentDescription desc;
   AudioComponent component;

   desc.componentType = inAUType;
   desc.componentSubType = 0;
   desc.componentManufacturer = 0;
   desc.componentFlags = 0;
   desc.componentFlagsMask = 0;

   component = AudioComponentFindNext(NULL, &desc);
   while (component != NULL)
   {
      OSStatus result;
      AudioComponentDescription found;

      result = AudioComponentGetDescription(component, &found);
      if (result == noErr)
      {
         CFStringRef cfName{};
         result = AudioComponentCopyName(component, &cfName);
         CF_ptr<CFStringRef> uName{ cfName };
         if (result == noErr) {
            wxString path;

            path.Printf(wxT("%-4.4s/%-4.4s/%-4.4s/%s"),
                        FromOSType(found.componentManufacturer),
                        FromOSType(found.componentType),
                        FromOSType(found.componentSubType),
                        wxCFStringRef::AsString(cfName));

            for (int i = 0; i < WXSIZEOF(BlackList); ++i) {
               if (BlackList[i].componentType == found.componentType &&
                   BlackList[i].componentSubType == found.componentSubType &&
                   BlackList[i].componentManufacturer ==
                      found.componentManufacturer) {
                  wxLogDebug(wxT("Blacklisted AU skipped: %s"), path);
                  result = !noErr;
                  break;
               }
            }
            if (result == noErr)
               effects.push_back(path);
         }
      }
      component = AudioComponentFindNext(component, &desc);
   }
}

#endif
