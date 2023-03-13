/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTEffect.cpp

  Dominic Mazzoni

  Paul Licameli split from VSTEffect.cpp

  This class implements a VST Plug-in effect.  The plug-in must be
  loaded in a platform-specific way and passed into the constructor,
  but from here this class handles the interfacing.

********************************************************************//**

\class AEffect
\brief VST Effects class, conforming to VST layout.

*//********************************************************************/

#include "VSTEffectBase.h"
#include "VSTInstance.h"

#if USE_VST

#include <wx/time.h>

#include "Base64.h"
#include "ConfigInterface.h"

VSTEffectBase::VSTEffectBase(const PluginPath & path)
:  VSTWrapper(path)
{
   memset(&mTimeInfo, 0, sizeof(mTimeInfo));
   mTimeInfo.samplePos = 0.0;
   mTimeInfo.sampleRate = 44100.0;  // this is a bogus value, but it's only for the display
   mTimeInfo.nanoSeconds = wxGetUTCTimeMillis().ToDouble();
   mTimeInfo.tempo = 120.0;
   mTimeInfo.timeSigNumerator = 4;
   mTimeInfo.timeSigDenominator = 4;
   mTimeInfo.flags = kVstTempoValid | kVstNanosValid;
}

VSTEffectBase::~VSTEffectBase() = default;

PluginPath VSTEffectBase::GetPath() const
{
   return mPath;
}

ComponentInterfaceSymbol VSTEffectBase::GetSymbol() const
{
   return VSTWrapper::GetSymbol();
}

VendorSymbol VSTEffectBase::GetVendor() const
{
   return { mVendor };
}

wxString VSTEffectBase::GetVersion() const
{
   wxString version;

   bool skipping = true;
   for (int i = 0, s = 0; i < 4; i++, s += 8)
   {
      int dig = (mVersion >> s) & 0xff;
      if (dig != 0 || !skipping)
      {
         version += !skipping ? wxT(".") : wxT("");
         version += wxString::Format(wxT("%d"), dig);
         skipping = false;
      }
   }

   return version;
}

TranslatableString VSTEffectBase::GetDescription() const
{
   // VST does have a product string opcode and some effects return a short
   // description, but most do not or they just return the name again.  So,
   // try to provide some sort of useful information.
   return XO("Audio In: %d, Audio Out: %d").Format( mAudioIns, mAudioOuts );
}

// ============================================================================
// EffectDefinitionInterface Implementation
// ============================================================================

EffectType VSTEffectBase::GetType() const
{
   if (mAudioIns == 0 && mAudioOuts == 0)
   {
      return EffectTypeTool;
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


EffectFamilySymbol VSTEffectBase::GetFamily() const
{
   return VSTPLUGINTYPE;
}

bool VSTEffectBase::IsInteractive() const
{
   return mInteractive;
}

bool VSTEffectBase::IsDefault() const
{
   return false;
}

auto VSTEffectBase::RealtimeSupport() const -> RealtimeSince
{
   return RealtimeSince::Always;

   /* return GetType() == EffectTypeProcess
      ? RealtimeSince::Always
      : RealtimeSince::Never; */
}

bool VSTEffectBase::SupportsAutomation() const
{
   return mAutomatable;
}

bool VSTEffectBase::InitializePlugin()
{
   if (!mAEffect)
   {
      Load();
   }

   if (!mAEffect)
   {
      return false;
   }

   return true;
}

std::shared_ptr<EffectInstance> VSTEffectBase::MakeInstance() const
{
   int userBlockSize;
   GetConfig(*this, PluginSettings::Shared, wxT("Options"),
      wxT("BufferSize"), userBlockSize, 8192);
   size_t userBlockSizeC = std::max( 1, userBlockSize );
   bool useLatency;
   GetConfig(*this, PluginSettings::Shared, wxT("Options"),
      wxT("UseLatency"), useLatency, true);
  
 
   return std::make_shared<VSTInstance>(
      *this, mPath, userBlockSizeC, userBlockSizeC, useLatency);
}

bool VSTEffectBase::SaveSettings(const EffectSettings& settings, CommandParameters& parms) const
{
   const VSTSettings& vstSettings = GetSettings(settings);

   for (const auto& item : vstSettings.mParamsMap)
   {
      if (item.second)
      {
         const auto& name  =   item.first;
         const auto& value = *(item.second);

         if (!parms.Write(name, value))
         {
            return false;
         }
      }
   }

   return true;
}

bool VSTEffectBase::LoadSettings(const CommandParameters& parms, EffectSettings& settings) const
{
   VSTSettings& vstSettings = GetSettings(settings);

   long index{};
   wxString key;
   double value = 0.0;
   if (parms.GetFirstEntry(key, index))
   {
      do
      {
         if (parms.Read(key, &value)) {
            auto &map = vstSettings.mParamsMap;
            auto iter = map.find(key);
            if (iter != map.end()) {
               if (iter->second)
                  // Should be guaranteed by MakeSettings
                  iter->second = value;
               else {
                  assert(false);
               }
            }
            else
               // Unknown parameter name in the file
               return false;
         }
      } while (parms.GetNextEntry(key, index));
   }

   // Loads key-value pairs only from a config file -- no chunk
   vstSettings.mChunk.resize(0);
   vstSettings.mVersion   = VSTWrapper::mVersion;
   vstSettings.mUniqueID  = VSTWrapper::mAEffect->uniqueID;
   vstSettings.mNumParams = VSTWrapper::mAEffect->numParams;

   return true;
}

RegistryPaths VSTEffectBase::GetFactoryPresets() const
{
   RegistryPaths progs;

   // Some plugins, like Guitar Rig 5, only report 128 programs while they have hundreds.  While
   // I was able to come up with a hack in the Guitar Rig case to gather all of the program names
   // it would not let me set a program outside of the first 128.
   if (mVstVersion >= 2)
   {
      for (int i = 0; i < mAEffect->numPrograms; i++)
      {
         progs.push_back(GetString(effGetProgramNameIndexed, i));
      }
   }

   return progs;
}

OptionalMessage
VSTEffectBase::LoadFactoryPreset(int id, EffectSettings& settings) const
{
   // To do: externalize state so const_cast isn't needed
   bool loadOK = const_cast<VSTEffectBase*>(this)->DoLoadFactoryPreset(id) &&
      FetchSettings(GetSettings(settings));
   if (!loadOK)
      return {};
   return MakeMessageFS(
      VSTInstance::GetSettings(settings));
}

bool VSTEffectBase::DoLoadFactoryPreset(int id)
{
   callSetProgram(id);

   return true;
}

bool VSTEffectBase::CanExportPresets() const
{
   return true;
}

bool VSTEffectBase::HasOptions() const
{
   return true;
}

std::vector<int> VSTEffectBase::GetEffectIDs()
{
   std::vector<int> effectIDs;

   // Are we a shell?
   if (mVstVersion >= 2 && (VstPlugCategory) callDispatcher(effGetPlugCategory, 0, 0, NULL, 0) == kPlugCategShell)
   {
      char name[64];
      int effectID;

      effectID = (int) callDispatcher(effShellGetNextPlugin, 0, 0, &name, 0);
      while (effectID)
      {
         effectIDs.push_back(effectID);
         effectID = (int) callDispatcher(effShellGetNextPlugin, 0, 0, &name, 0);
      }
   }

   return effectIDs;
}

OptionalMessage VSTEffectBase::LoadUserPreset(
   const RegistryPath & group, EffectSettings &settings) const
{
   wxString value;

   auto info = GetChunkInfo();

   GetConfig(*this, PluginSettings::Private, group, wxT("UniqueID"),
      info.pluginUniqueID, info.pluginUniqueID);
   GetConfig(*this, PluginSettings::Private, group, wxT("Version"),
      info.pluginVersion, info.pluginVersion);
   GetConfig(*this, PluginSettings::Private, group, wxT("Elements"),
      info.numElements, info.numElements);

   if ( ! IsCompatible(info) )
   {
      return {};
   }

   if (GetConfig(*this,
      PluginSettings::Private, group, wxT("Chunk"), value, wxEmptyString))
   {
      ArrayOf<char> buf{ value.length() / 4 * 3 };

      int len = Base64::Decode(value, buf.get());
      if (len)
      {
         callSetChunk(true, len, buf.get(), &info);
         if (!FetchSettings(GetSettings(settings)))
            return {};
      }

      return MakeMessageFS(
         VSTInstance::GetSettings(settings));
   }

   wxString parms;
   if (!GetConfig(*this,
      PluginSettings::Private, group, wxT("Parameters"), parms, wxEmptyString))
   {
      return {};
   }

   CommandParameters eap;
   if (!eap.SetParameters(parms))
   {
      return {};
   }

   const bool loadOK = LoadSettings(eap, settings) &&
      FetchSettings(GetSettings(settings));
   if (!loadOK)
      return {};

   return MakeMessageFS(
      VSTInstance::GetSettings(settings));
}

bool VSTEffectBase::SaveUserPreset(
   const RegistryPath & group, const EffectSettings &settings) const
{
   const auto& vstSettings = GetSettings(settings);

   if ( ! StoreSettings(vstSettings) )
      return false;

   SetConfig(*this, PluginSettings::Private, group, wxT("UniqueID"), vstSettings.mUniqueID );
   SetConfig(*this, PluginSettings::Private, group, wxT("Version"),  vstSettings.mVersion  );
   SetConfig(*this, PluginSettings::Private, group, wxT("Elements"), vstSettings.mNumParams);

   if (mAEffect->flags & effFlagsProgramChunks)
   {
      void *chunk = NULL;
      int clen = (int) constCallDispatcher(effGetChunk, 1, 0, &chunk, 0.0);
      if (clen <= 0)
      {
         return false;
      }

      SetConfig(*this, PluginSettings::Private, group, wxT("Chunk"),
         Base64::Encode(chunk, clen));
      return true;
   }

   CommandParameters eap;
   if (!SaveSettings(settings, eap))
   {
      return false;
   }

   wxString parms;
   if (!eap.GetParameters(parms))
   {
      return false;
   }

   return SetConfig(*this, PluginSettings::Private,
      group, wxT("Parameters"), parms);
}

EffectSettings VSTEffectBase::MakeSettings() const
{
   VSTSettings settings;
   FetchSettings(settings);
   return EffectSettings::Make<VSTSettings>(std::move(settings));
}

#endif // USE_VST
