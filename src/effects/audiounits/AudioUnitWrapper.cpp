/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioUnitWrapper.cpp

  Dominic Mazzoni
  Leland Lucius

  Paul Licameli split from AudioUnitEffect.cpp

**********************************************************************/


#if USE_AUDIO_UNITS
#include "AudioUnitWrapper.h"
#include "Internat.h"
#include "ModuleManager.h"

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

// Uncomment to include parameter IDs in the final name.  Only needed if it's
// discovered that many effects have duplicate names.  It could even be done
// at runtime by scanning an effects parameters to determine if dups are present
// and, if so, enable the clump and parameter IDs.
#define USE_EXTENDED_NAMES

AudioUnitWrapper::ParameterInfo::ParameterInfo(
   AudioUnit mUnit, AudioUnitParameterID parmID)
{
   static constexpr char idBeg = wxT('<');
   static constexpr char idSep = wxT(',');
   static constexpr char idEnd = wxT('>');

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

bool AudioUnitWrapper::CreateAudioUnit()
{
   AudioUnit unit{};
   auto result = AudioComponentInstanceNew(mComponent, &unit);
   if (!result)
      mUnit.reset(unit);
   return (!result && unit != nullptr);
}

TranslatableString AudioUnitWrapper::InterpretBlob(
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
   return {};
}

bool AudioUnitWrapper::ForEachParameter(ParameterVisitor visitor) const
{
   PackedArray::Ptr<AudioUnitParameterID> array;
   if (GetVariableSizeProperty(kAudioUnitProperty_ParameterList, array))
      return false;
   for (const auto &ID : array)
      if (ParameterInfo pi{ mUnit.get(), ID };
         !visitor(pi, ID))
         break;
   return true;
}

std::pair<CF_ptr<CFDataRef>, TranslatableString>
AudioUnitWrapper::MakeBlob(const wxCFStringRef &cfname, bool binary) const
{
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
#endif
