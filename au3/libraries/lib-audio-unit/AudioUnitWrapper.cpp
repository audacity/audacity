/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioUnitWrapper.cpp

  Dominic Mazzoni
  Leland Lucius

  Paul Licameli split from AudioUnitEffect.cpp

**********************************************************************/


#if USE_AUDIO_UNITS
#include "AudioUnitWrapper.h"
#include "ConfigInterface.h"
#include "EffectInterface.h"
#include "Internat.h"
#include "ModuleManager.h"
#include "PluginProvider.h"

#include <wx/osx/core/private.h>
#include <wx/log.h>

AudioUnitEffectSettings &
AudioUnitWrapper::GetSettings(EffectSettings &settings) {
   auto pSettings = settings.cast<AudioUnitEffectSettings>();
   // Assume the settings object ultimately came from AudioUnitEffect's
   // MakeSettings or copying of that
   assert(pSettings);
   return *pSettings;
}

const AudioUnitEffectSettings &
AudioUnitWrapper::GetSettings(const EffectSettings &settings) {
   return GetSettings(const_cast<EffectSettings &>(settings));
}

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

// Uncomment to include parameter IDs in the final name.  Only needed if it's
// discovered that many effects have duplicate names.  It could even be done
// at runtime by scanning an effects parameters to determine if dups are present
// and, if so, enable the clump and parameter IDs.
#define USE_EXTENDED_NAMES

AudioUnitWrapper::ParameterInfo::ParameterInfo(
   AudioUnit mUnit, AudioUnitParameterID parmID)
{
   UInt32 dataSize;

   mInfo = {};
   // Note non-default element parameter, parmID
   if (AudioUnitUtils::GetFixedSizeProperty(mUnit,
      kAudioUnitProperty_ParameterInfo, mInfo,
      kAudioUnitScope_Global, parmID))
      return;

   wxString &name = mName.emplace();
   if (mInfo.flags & kAudioUnitParameterFlag_HasCFNameString)
      name = wxCFStringRef::AsString(mInfo.cfNameString);
   else
      name = wxString(mInfo.name);

#if defined(USE_EXTENDED_NAMES)
   // Parameter name may or may not be present.  The modified name will be:
   //
   //    <[ParameterName,]parmID>
   //
   // (where the [ ] meta-characters denote optionality)
   // (And any of the characters < , > in ParameterName are replaced with _)
   if (!name.empty()) {
      name.Replace(idBeg, wxT('_'));
      name.Replace(idSep, wxT('_'));
      name.Replace(idEnd, wxT('_'));
      name.Append(idSep);
   }
   name = wxString::Format(wxT("%c%s%x%c"), idBeg, name, parmID, idEnd);

   // If the parameter has a clumpID, then the final modified name will be:
   //
   //    <[clumpName,]clumpId><[ParameterName,]parmID>
   //
   // (And any of the characters < , > in clumpName are replaced with _)
   if (mInfo.flags & kAudioUnitParameterFlag_HasClump) {
      wxString clumpName;
      AudioUnitUtils::ParameterNameInfo clumpInfo{
         mInfo.clumpID, kAudioUnitParameterName_Full
      };

      if (!AudioUnitUtils::GetFixedSizeProperty(mUnit,
         kAudioUnitProperty_ParameterClumpName, clumpInfo)) {
         clumpName =  wxCFStringRef::AsString(clumpInfo.outName);
         clumpName.Replace(idBeg, wxT('_'));
         clumpName.Replace(idSep, wxT('_'));
         clumpName.Replace(idEnd, wxT('_'));
         clumpName.Append(idSep);
      }
      name = wxString::Format(wxT("%c%s%x%c%s"),
         idBeg, clumpName, mInfo.clumpID, idEnd, name);
   }
#endif
}

std::optional<AudioUnitParameterID>
AudioUnitWrapper::ParameterInfo::ParseKey(const wxString &key)
{
   // This is not a complete validation of the format of the key
   // Scan left for , or <
   if (const auto rend = key.rend(), riter = std::find_if(key.rbegin(), rend,
      [](wxChar c) { return c == idBeg || c == idSep; })
      ; riter != rend
   ) {
      // Scan right for >
      if (const auto end = key.end(), left = riter.base(), // one right of riter
         right = std::find(left, end, idEnd)
         ; left != right && right != end
      ){
         // Interpret character range as hex
         if (long value{}
             ; wxString{left, right}.ToLong(&value, 16))
            return value;
      }
   }
   return {};
}

bool AudioUnitWrapper::FetchSettings(
   AudioUnitEffectSettings &settings, bool fetchValues, bool fetchPreset) const
{
   settings.mPresetNumber = {};
   if (fetchPreset) {
      AUPreset preset{};
      if (!GetFixedSizeProperty(kAudioUnitProperty_PresentPreset, preset))
         // Only want factory preset number, not a user preset (<0)
         if (preset.presetNumber >= 0)
            settings.mPresetNumber = { preset.presetNumber };
   }

   // Fetch values from the AudioUnit into AudioUnitEffectSettings,
   // keeping the cache up-to-date after state changes in the AudioUnit
   ForEachParameter(
   [this, &settings, fetchValues]
   (const ParameterInfo &pi, AudioUnitParameterID ID) {
      // Always make a slot, even for parameter IDs that are known but
      // not gettable from the instance now.  Example:  AUGraphicEQ when
      // you choose 10 bands; the values for bands 10 ... 30 are undefined
      // but the parameter IDs are known
      auto &slot = settings.values[ID];
      slot.reset();
      if (fetchValues) {
         AudioUnitParameterValue value;
         if (!pi.mName ||
            AudioUnitGetParameter(
               mUnit.get(), ID, kAudioUnitScope_Global, 0, &value)) {
               // Probably failed because of invalid parameter which can happen
               // if a plug-in is in a certain mode that doesn't contain the
               // parameter.  In any case, just ignore it.
            }
         else
            slot.emplace(settings.Intern(*pi.mName), value);
      }
      return true;
   });
   return true;
}

bool AudioUnitWrapper::StoreSettings(const EffectDefinitionInterface &effect,
   const AudioUnitEffectSettings &settings) const
{
   // This is a const member function inherited by AudioUnitEffect, though it
   // mutates the AudioUnit object (mUnit.get()).  This is necessary for the
   // AudioUnitEffect (an EffectPlugin) to compute the "blob" of settings state
   // for export or to save settings in the config file, which the SDK later
   // reinterprets.
   // So consider mUnit a mutable scratch pad object.  This doesn't really make
   // the AudioUnitEffect stateful.

   // First restore factory preset if it applies
   if (settings.mPresetNumber) {
      // Mutate the scratch AudioUnit, don't pass settings
      LoadFactoryPreset(effect, *settings.mPresetNumber, nullptr);
      // Then go on to reapply some slider changes that might have been done
      // after a change of preset
   }

   // Update parameter values in the AudioUnit from const
   // AudioUnitEffectSettings
   // Allow two passes; because sometimes the re-assignability of one parameter
   // depends on first doing it for another parameter, but the other is later
   // in the iteration.  For instance, for AUGraphicEQ, parameter ID 10000
   // (decimal) determines 10 or 31 bands, but while the effect is still set
   // for 10, then assignment to the last 21 sliders fails.
   for (auto pass : {0, 1}) {
      ForEachParameter([ this, &settings
      ] (const ParameterInfo &pi, AudioUnitParameterID ID) {
         if (pi.mName) {
            if (auto iter = settings.values.find(ID);
               iter != settings.values.end() && iter->second.has_value()
            ){
               if (AudioUnitSetParameter(mUnit.get(), ID,
                  kAudioUnitScope_Global, 0, iter->second->second, 0)) {
                  // Probably failed because of an invalid parameter when
                  // a plug-in is in a certain mode that doesn't contain
                  // the parameter.
               }
            }
            else {
               // Leave parameters that are in the AudioUnit, but not known in
               // settings, unchanged
            }
         }
         return true;
      });
   }
   return true;
}

bool AudioUnitWrapper::MoveSettingsContents(
   AudioUnitEffectSettings &&src, AudioUnitEffectSettings &dst, bool merge)
{
   // Do an in-place rewrite of dst, avoiding allocations
   auto &dstMap = dst.values;
   auto dstIter = dstMap.begin(), dstEnd = dstMap.end();
   auto &srcMap = src.values;
   for (auto &[key, oValue] : srcMap) {
      while (dstIter != dstEnd && dstIter->first != key)
         ++dstIter;
      if (dstIter == dstEnd)
         break;
      auto &[dstKey, dstOValue] = *dstIter;
      assert(dstKey == key);
      if (oValue) {
         dstOValue.emplace(*oValue);
         oValue.reset();
      }
      else if (!merge)
         // Don't accumulate non-nulls only, but copy the nulls
         dstOValue.reset();
   }
   return true;
}

bool AudioUnitWrapper::CreateAudioUnit()
{
   AudioUnit unit{};
   auto result = AudioComponentInstanceNew(mComponent, &unit);
   if (!result) {
      mUnit.reset(unit);
      if (&mParameters == &mOwnParameters && !mOwnParameters)
         result = GetVariableSizeProperty(
            kAudioUnitProperty_ParameterList, mOwnParameters);
   }

   return (!result && unit != nullptr);
}

TranslatableString AudioUnitWrapper::InterpretBlob(
   AudioUnitEffectSettings &settings,
   const RegistryPath &group, const wxMemoryBuffer &buf) const
{
   size_t bufLen = buf.GetDataLen();
   if (!bufLen)
      return XO("Failed to decode \"%s\" preset").Format(group);

   // Create a CFData object that references the decoded preset
   const auto bufPtr = static_cast<const uint8_t *>(buf.GetData());
   CF_ptr<CFDataRef> data{ CFDataCreateWithBytesNoCopy(kCFAllocatorDefault,
      bufPtr, bufLen, kCFAllocatorNull)
   };
   if (!data)
      return XO("Failed to convert \"%s\" preset to internal format")
         .Format(group);

   // Convert it back to a property list
   CF_ptr<CFPropertyListRef> content{
      CFPropertyListCreateWithData(kCFAllocatorDefault,
      data.get(), kCFPropertyListImmutable, nullptr,
      // TODO might retrieve more error information
      nullptr)
   };
   if (!content)
      return XO("Failed to create property list for \"%s\" preset")
         .Format(group);

   // Finally, update the properties and parameters
   if (SetProperty(kAudioUnitProperty_ClassInfo, content.get()))
      return XO("Failed to set class info for \"%s\" preset").Format(group);

   // Repopulate the AudioUnitEffectSettings from the change of state in
   // the AudioUnit, and include the preset
   FetchSettings(settings, true, true);
   return {};
}

void AudioUnitWrapper::ForEachParameter(ParameterVisitor visitor) const
{
   if (!mParameters)
      return;
   for (const auto &ID : mParameters)
      if (ParameterInfo pi{ mUnit.get(), ID };
         !visitor(pi, ID))
         break;
}

bool AudioUnitWrapper::LoadPreset(const EffectDefinitionInterface &effect,
   const RegistryPath & group, EffectSettings &settings) const
{
   // Retrieve the preset
   wxString parms;
   if (!GetConfig(effect, PluginSettings::Private, group, PRESET_KEY, parms,
      wxEmptyString)) {
      // Commented "CurrentSettings" gets tried a lot and useless messages appear
      // in the log
      //wxLogError(wxT("Preset key \"%s\" not found in group \"%s\""), PRESET_KEY, group);
      return false;
   }

   // Decode it, complementary to what SaveBlobToConfig did
   auto error =
      InterpretBlob(GetSettings(settings), group, wxBase64Decode(parms));
   if (!error.empty()) {
      wxLogError(error.Debug());
      return false;
   }

   return true;
}

bool AudioUnitWrapper::LoadFactoryPreset(
   const EffectDefinitionInterface &effect, int id, EffectSettings *pSettings)
const
{
   if (pSettings) {
      // Issue 3441: Some factory presets of some effects do not reassign all
      // controls.  So first put controls into a default state, not contaminated
      // by previous importing or other loading of settings into this wrapper.
      if (!LoadPreset(effect, FactoryDefaultsGroup(), *pSettings))
         return false;
   }

   // Retrieve the list of factory presets
   CF_ptr<CFArrayRef> array;
   if (GetFixedSizeProperty(kAudioUnitProperty_FactoryPresets, array) ||
       id < 0 || id >= CFArrayGetCount(array.get()))
      return false;

   // Mutate the scratch pad AudioUnit in this wrapper
   if (SetProperty(kAudioUnitProperty_PresentPreset,
      *static_cast<const AUPreset*>(CFArrayGetValueAtIndex(array.get(), id))))
      return false;

   if (pSettings) {
      // Repopulate the AudioUnitEffectSettings from the change of state in
      // the AudioUnit
      if (!FetchSettings(GetSettings(*pSettings), true, true))
         return false;
   }

   return true;
}

std::pair<CF_ptr<CFDataRef>, TranslatableString>
AudioUnitWrapper::MakeBlob(const EffectDefinitionInterface &effect,
   const AudioUnitEffectSettings &settings,
   const wxCFStringRef &cfname, bool binary) const
{
   // This is a const function of AudioUnitEffect, but it mutates
   // an AudioUnit object (mUnit.get()) to accomplish its work.  But that
   // should be the "scratchpad" unit only, not real instance state.

   // Update state of the unit from settings
   StoreSettings(effect, settings);

   CF_ptr<CFDataRef> data;
   TranslatableString message;

   // Define the preset property and set it in the audio unit
   if (SetProperty(
      kAudioUnitProperty_PresentPreset, AudioUnitUtils::UserPreset{ cfname }))
      message = XO("Failed to set preset name");

   // Now retrieve the preset content
   else if (CF_ptr<CFPropertyListRef> content;
      GetFixedSizeProperty(kAudioUnitProperty_ClassInfo, content))
      message = XO("Failed to retrieve preset content");

   // And convert it to serialized XML data
   else if (data.reset(CFPropertyListCreateData(kCFAllocatorDefault,
         content.get(),
            (binary ? PRESET_FORMAT : kCFPropertyListXMLFormat_v1_0), 0,
         // TODO might retrieve more error information
         nullptr));
      !data)
      message = XO("Failed to convert property list to XML data");

   // Nothing to do if we don't have any data
   else if (auto length = CFDataGetLength(data.get()); length == 0)
      // Caller might not treat this as error, becauase data is non-null
      message = XO("XML data is empty after conversion");

   return { move(data), message };
}

bool AudioUnitWrapper::SetRateAndChannels(
   double sampleRate, const wxString &identifier)
{
   AudioUnitUtils::StreamBasicDescription streamFormat{
      // Float64 mSampleRate;
      sampleRate,

      // UInt32  mFormatID;
      kAudioFormatLinearPCM,

      // UInt32  mFormatFlags;
      (kAudioFormatFlagsNativeFloatPacked |
          kAudioFormatFlagIsNonInterleaved),

      // UInt32  mBytesPerPacket;
      sizeof(float),

      // UInt32  mFramesPerPacket;
      1,

      // UInt32  mBytesPerFrame;
      sizeof(float),

      // UInt32  mChannelsPerFrame;
      0,

      // UInt32  mBitsPerChannel;
      sizeof(float) * 8,
   };

   unsigned one = 1u;
   const struct Info{
      unsigned &nChannels;
      AudioUnitScope scope;
      const char *const msg; // used only in log messages
   } infos[]{
      { one, kAudioUnitScope_Global, "global" },
      { mAudioIns, kAudioUnitScope_Input, "input" },
      { mAudioOuts, kAudioUnitScope_Output, "output" },
   };
   for (const auto &[nChannels, scope, msg] : infos) {
      if (nChannels) {
         if (SetProperty(kAudioUnitProperty_SampleRate, sampleRate, scope)) {
            wxLogError("%ls Didn't accept sample rate on %s\n",
               // Exposing internal name only in logging
               identifier.wx_str(), msg);
            return false;
         }
         if (scope != kAudioUnitScope_Global) {
            bool failed = true;
            ++nChannels;
            do {
               --nChannels;
               streamFormat.mChannelsPerFrame = nChannels;
               failed = SetProperty(kAudioUnitProperty_StreamFormat,
                  streamFormat, scope);
            } while(failed && nChannels > 0);
            if (failed) {
               wxLogError("%ls didn't accept stream format on %s\n",
                  // Exposing internal name only in logging
                  identifier.wx_str(), msg);
               return false;
            }
         }
      }
   }
   return true;
}
#endif
