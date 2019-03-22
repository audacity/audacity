/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadEffects.cpp

  Dominic Mazzoni

**************************************************************************//**
\class BuiltinEffectsModule
\brief Internal module to auto register all built in effects.  
*****************************************************************************/

#include "../Audacity.h" // for USE_* macros
#include "LoadEffects.h"

#include "../Experimental.h"

#include "../Prefs.h"

#include "../MemoryX.h"

#include "EffectManager.h"

#include "Amplify.h"
#include "AutoDuck.h"
#include "BassTreble.h"
#include "ChangeSpeed.h"
#include "ClickRemoval.h"
#include "Compressor.h"
#include "Distortion.h"
#include "DtmfGen.h"
#include "Echo.h"
#include "Paulstretch.h"
#include "Equalization.h"
#include "Fade.h"
#include "Invert.h"
#include "Noise.h"
#ifdef EXPERIMENTAL_NOISE_REDUCTION
#include "NoiseReduction.h"
#endif
#include "NoiseRemoval.h"
#include "Normalize.h"
#include "Phaser.h"
#include "Repair.h"
#include "Repeat.h"
#include "Reverb.h"
#include "Reverse.h"
#include "Silence.h"
#include "ScienFilter.h"
#include "StereoToMono.h"
#ifdef USE_SBSMS
#include "TimeScale.h"
#endif
#include "ToneGen.h"
#include "TruncSilence.h"
#include "Wahwah.h"

#include "FindClipping.h"

#ifdef USE_SOUNDTOUCH
#include "ChangePitch.h"
#include "ChangeTempo.h"
#endif

//
// Include the SoundTouch effects, if requested
//
#if defined(USE_SOUNDTOUCH)
#define SOUNDTOUCH_EFFECTS \
   EFFECT( CHANGEPITCH, EffectChangePitch, () ) \
   EFFECT( CHANGETEMPO, EffectChangeTempo, () )
#else
#define SOUNDTOUCH_EFFECTS
#endif

//
// Select the desired Noise Reduction/Removal effect
//
#if defined(EXPERIMENTAL_NOISE_REDUCTION)
#define NOISEREDUCTION_EFFECT \
   EFFECT( NOISEREDUCTION, EffectNoiseReduction, () )
#else
#define NOISEREDUCTION_EFFECT \
   EFFECT( NOISEREMOVAL, EffectNoiseRemoval, () )
#endif

//
// Include the Classic Filters effect, if requested
//
#if defined(EXPERIMENTAL_SCIENCE_FILTERS)
#define CLASSICFILTER_EFFECT \
   EFFECT( CLASSICFILTERS, EffectScienFilter, () )
#else
#define CLASSICFILTER_EFFECT
#endif

//
// Include the SBSMS effect, if requested
//
#if defined(USE_SBSMS)
#define SBSMS_EFFECTS \
   EFFECT( TIMESCALE, EffectTimeScale, () )
#else
#define SBSMS_EFFECTS
#endif

//
// Define the list of effects that will be autoregistered and how to instantiate each
//
#define EFFECT_LIST \
   EFFECT( CHIRP,             EffectToneGen, (true) )                  \
   EFFECT( DTMFTONES,         EffectDtmf, () )                         \
   EFFECT( NOISE,             EffectNoise, () )                        \
   EFFECT( SILENCE,           EffectSilence, () )                      \
   EFFECT( TONE,              EffectToneGen, (false) )                 \
   EFFECT( AMPLIFY,           EffectAmplify, () )                      \
   EFFECT( BASSTREBLE,        EffectBassTreble, () )                   \
   EFFECT( CHANGESPEED,       EffectChangeSpeed, () )                  \
   EFFECT( CLICKREMOVAL,      EffectClickRemoval, () )                 \
   EFFECT( COMPRESSOR,        EffectCompressor, () )                   \
   EFFECT( DISTORTION,        EffectDistortion, () )                   \
   EFFECT( ECHO,              EffectEcho, () )                         \
   EFFECT( EQUALIZATION,      EffectEqualization, (kEqLegacy) )        \
   EFFECT( FADEIN,            EffectFade, (true) )                     \
   EFFECT( FADEOUT,           EffectFade, (false) )                    \
   EFFECT( FILTERCURVE,       EffectEqualization, (kEqOptionCurve) )   \
   EFFECT( GRAPHICEQ,         EffectEqualization, (kEqOptionGraphic) ) \
   EFFECT( INVERT,            EffectInvert, () )                       \
   EFFECT( NORMALIZE,         EffectNormalize, () )                    \
   EFFECT( PHASER,            EffectPhaser, () )                       \
   EFFECT( REPAIR,            EffectRepair, () )                       \
   EFFECT( REPEAT,            EffectRepeat, () )                       \
   EFFECT( REVERB,            EffectReverb, () )                       \
   EFFECT( REVERSE,           EffectReverse, () )                      \
   EFFECT( STEREOTOMONO,      EffectStereoToMono, () )                 \
   EFFECT( TRUNCATESILENCE,   EffectTruncSilence, () )                 \
   EFFECT( WAHWAH,            EffectWahwah, () )                       \
   EFFECT( FINDCLIPPING,      EffectFindClipping, () )                 \
   NOISEREDUCTION_EFFECT                                               \
   SOUNDTOUCH_EFFECTS                                                  \
   EFFECT( AUTODUCK,          EffectAutoDuck, () )                     \
   EFFECT( PAULSTRETCH,       EffectPaulstretch, () )                  \
   SBSMS_EFFECTS

//
// Define the list of effects that do not get autoregistered
//
#define EXCLUDE_LIST \
   CLASSICFILTER_EFFECT

//
// Define the EFFECT() macro to generate enum names
//
#define EFFECT(n, i, args) ENUM_ ## n,

//
// Create the enum for the list of effects (will be used in a switch statement)
//
enum
{
   EFFECT_LIST
   EXCLUDE_LIST
};

//
// Redefine EFFECT() to add the effect's name to an array
//
#undef EFFECT
#define EFFECT(n, i, args) results.push_back((n ## _PLUGIN_SYMBOL).Internal());

//
// Create the effect name array
//
static const std::vector<wxString> kEffectNames() {
   std::vector<wxString> results;
   EFFECT_LIST;
   return results;
}

//
// Create the effect name array of excluded effects
//
static const std::vector<wxString> kExcludedNames() {
   std::vector<wxString> results;
   EXCLUDE_LIST;
   return results;
}

//
// Redefine EFFECT() to generate a case statement for the lookup switch
//
#undef EFFECT
#define EFFECT(n, i, args) case ENUM_ ## n: return std::make_unique<i> args;

// ============================================================================
// Module registration entry point
//
// This is the symbol that Audacity looks for when the module is built as a
// dynamic library.
//
// When the module is builtin to Audacity, we use the same function, but it is
// declared static so as not to clash with other builtin modules.
// ============================================================================
DECLARE_MODULE_ENTRY(AudacityModule)
{
   // Create and register the importer
   // Trust the module manager not to leak this
   return safenew BuiltinEffectsModule(moduleManager, path);
}

// ============================================================================
// Register this as a builtin module
// ============================================================================
DECLARE_BUILTIN_MODULE(BuiltinsEffectBuiltin);

///////////////////////////////////////////////////////////////////////////////
//
// BuiltinEffectsModule
//
///////////////////////////////////////////////////////////////////////////////

BuiltinEffectsModule::BuiltinEffectsModule(ModuleManagerInterface *moduleManager,
                                           const wxString *path)
{
   mModMan = moduleManager;
   if (path)
   {
      mPath = *path;
   }
}

BuiltinEffectsModule::~BuiltinEffectsModule()
{
   mPath.clear();
}

// ============================================================================
// ComponentInterface implementation
// ============================================================================

PluginPath BuiltinEffectsModule::GetPath()
{
   return mPath;
}

ComponentInterfaceSymbol BuiltinEffectsModule::GetSymbol()
{
   return XO("Builtin Effects");
}

VendorSymbol BuiltinEffectsModule::GetVendor()
{
   return XO("The Audacity Team");
}

wxString BuiltinEffectsModule::GetVersion()
{
   // This "may" be different if this were to be maintained as a separate DLL
   return AUDACITY_VERSION_STRING;
}

wxString BuiltinEffectsModule::GetDescription()
{
   return _("Provides builtin effects to Audacity");
}

// ============================================================================
// ModuleInterface implementation
// ============================================================================

bool BuiltinEffectsModule::Initialize()
{
   const auto &names = kEffectNames();
   for (const auto &name : names)
   {
      mNames.push_back(wxString(BUILTIN_EFFECT_PREFIX) + name);
   }

   const auto &excluded = kExcludedNames();
   for (const auto &name : excluded)
   {
      mNames.push_back(wxString(BUILTIN_EFFECT_PREFIX) + name);
   }

   return true;
}

void BuiltinEffectsModule::Terminate()
{
   // Nothing to do here
   return;
}

bool BuiltinEffectsModule::AutoRegisterPlugins(PluginManagerInterface & pm)
{
   wxString ignoredErrMsg;
   const auto &names = kEffectNames();
   for (const auto &name : names)
   {
      PluginPath path(wxString(BUILTIN_EFFECT_PREFIX) + name);

      if (!pm.IsPluginRegistered(path))
      {
         // No checking of error ?
         DiscoverPluginsAtPath(path, ignoredErrMsg,
            PluginManagerInterface::DefaultRegistrationCallback);
      }
   }

   // We still want to be called during the normal registration process
   return false;
}

PluginPaths BuiltinEffectsModule::FindPluginPaths(PluginManagerInterface & WXUNUSED(pm))
{
   return mNames;
}

unsigned BuiltinEffectsModule::DiscoverPluginsAtPath(
   const PluginPath & path, wxString &errMsg,
   const RegistrationCallback &callback)
{
   errMsg.clear();
   auto effect = Instantiate(path);
   if (effect)
   {
      if (callback)
         callback(this, effect.get());
      return 1;
   }

   errMsg = _("Unknown built-in effect name");
   return 0;
}

bool BuiltinEffectsModule::IsPluginValid(const PluginPath & path, bool bFast)
{
   // bFast is unused as checking in the list is fast.
   static_cast<void>(bFast);
   return make_iterator_range( mNames ).contains( path );
}

ComponentInterface *BuiltinEffectsModule::CreateInstance(const PluginPath & path)
{
   // Acquires a resource for the application.
   // Safety of this depends on complementary calls to DeleteInstance on the module manager side.
   return Instantiate(path).release();
}

void BuiltinEffectsModule::DeleteInstance(ComponentInterface *instance)
{
   // Releases the resource.
   std::unique_ptr < Effect > {
      dynamic_cast<Effect *>(instance)
   };
}

// ============================================================================
// BuiltinEffectsModule implementation
// ============================================================================

std::unique_ptr<Effect> BuiltinEffectsModule::Instantiate(const PluginPath & path)
{
   wxASSERT(path.StartsWith(BUILTIN_EFFECT_PREFIX));
   auto index = make_iterator_range( mNames ).index( path );
   wxASSERT( index != wxNOT_FOUND );

   switch ( index )
   {
      EFFECT_LIST;
      EXCLUDE_LIST;
   }

   return nullptr;
}
