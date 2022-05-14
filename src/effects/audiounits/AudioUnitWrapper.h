/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioUnitWrapper.h

  Dominic Mazzoni
  Leland Lucius

  Paul Licameli split from AudioUnitEffect.h

**********************************************************************/
#ifndef AUDACITY_AUDIOUNIT_WRAPPER_H
#define AUDACITY_AUDIOUNIT_WRAPPER_H

#if USE_AUDIO_UNITS

#include <optional>
#include <wx/string.h>

#include "AudioUnitUtils.h"

class wxCFStringRef;
class wxMemoryBuffer;
class TranslatableString;

//! Common base class for AudioUnitEffect and its Instance
/*!
 Maintains a smart handle to an AudioUnit (also called AudioComponentInstance)
 in the SDK and defines some utility functions
 */
struct AudioUnitWrapper
{
   explicit AudioUnitWrapper(AudioComponent component)
      : mComponent{ component }
   {}

   // Supply most often used values as defaults for scope and element
   template<typename T>
   OSStatus GetFixedSizeProperty(AudioUnitPropertyID inID, T &property,
      AudioUnitScope inScope = kAudioUnitScope_Global,
      AudioUnitElement inElement = 0) const
   {
      // Supply mUnit.get() to the non-member function
      return AudioUnitUtils::GetFixedSizeProperty(mUnit.get(),
         inID, property, inScope, inElement);
   }

   // Supply most often used values as defaults for scope and element
   template<typename T>
   OSStatus GetVariableSizeProperty(AudioUnitPropertyID inID,
      PackedArray::Ptr<T> &pObject,
      AudioUnitScope inScope = kAudioUnitScope_Global,
      AudioUnitElement inElement = 0) const
   {
      return AudioUnitUtils::GetVariableSizeProperty(mUnit.get(),
         inID, pObject, inScope, inElement);
   }

   // Supply most often used values as defaults for scope and element
   template<typename T>
   OSStatus SetProperty(AudioUnitPropertyID inID, const T &property,
      AudioUnitScope inScope = kAudioUnitScope_Global,
      AudioUnitElement inElement = 0) const
   {
      // Supply mUnit.get() to the non-member function
      return AudioUnitUtils::SetProperty(mUnit.get(),
         inID, property, inScope, inElement);
   }

   class ParameterInfo;
   //! Return value: if true, continue visiting
   using ParameterVisitor =
      std::function< bool(const ParameterInfo &pi, AudioUnitParameterID ID) >;
   //! @return false if parameters could not be retrieved at all, else true
   bool ForEachParameter(ParameterVisitor visitor) const;

   //! Obtain dump of the setting state of an AudioUnit instance
   /*!
    @param binary if false, then produce XML serialization instead; but
    AudioUnits does not need to be told the format again to reinterpret the blob
    @return smart pointer to data, and an error message
    */
   std::pair<CF_ptr<CFDataRef>, TranslatableString>
   MakeBlob(const wxCFStringRef &cfname, bool binary) const;

   //! Interpret the dump made before by MakeBlob
   /*!
    @param group only for formatting error messages
    @return an error message
    */
   TranslatableString InterpretBlob(
      const wxString &group, const wxMemoryBuffer &buf) const;

   bool CreateAudioUnit();

   const AudioComponent mComponent;
   AudioUnitCleanup<AudioUnit, AudioComponentInstanceDispose> mUnit;
};

class AudioUnitWrapper::ParameterInfo final
{
public:
   ParameterInfo(AudioUnit mUnit, AudioUnitParameterID parmID);

   std::optional<wxString> mName;
   AudioUnitParameterInfo mInfo{};
};

#endif

#endif
