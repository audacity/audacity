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
#include <unordered_map>
#include <wx/string.h>

#include "AudioUnitUtils.h"

class wxCFStringRef;
class wxMemoryBuffer;
class TranslatableString;

//! This works as a cached copy of state stored in an AudioUnit, but can also
//! outlive it
struct AudioUnitEffectSettings {
   // Hash from numerical perameter IDs (should we assume they are small
   // integers?) to floating point values
   using Map =
      std::unordered_map<AudioUnitParameterID, AudioUnitParameterValue>;
   Map values;

   AudioUnitEffectSettings() = default;
   AudioUnitEffectSettings(Map map) : values{ move(map) } {}
   
   //! Associate 0 with all keys already present in the map
   void ResetValues()
   {
      for (auto &[_, value] : values)
         value = {};
   }
};

//! Common base class for AudioUnitEffect and its Instance
/*!
 Maintains a smart handle to an AudioUnit (also called AudioComponentInstance)
 in the SDK and defines some utility functions
 */
struct AudioUnitWrapper
{
   using Parameters = PackedArray::Ptr<const AudioUnitParameterID>;

   /*!
    @param pParameters if non-null, use those; else, fetch from the AudioUnit
    */
   AudioUnitWrapper(AudioComponent component, Parameters *pParameters)
      : mComponent{ component }
      , mParameters{ pParameters ? *pParameters : mOwnParameters }
   {
   }

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
   void ForEachParameter(ParameterVisitor visitor) const;

   //! Obtain dump of the setting state of an AudioUnit instance
   /*!
    @param binary if false, then produce XML serialization instead; but
    AudioUnits does not need to be told the format again to reinterpret the blob
    @return smart pointer to data, and an error message
    */
   std::pair<CF_ptr<CFDataRef>, TranslatableString>
   MakeBlob(const AudioUnitEffectSettings &settings,
      const wxCFStringRef &cfname, bool binary) const;

   //! Interpret the dump made before by MakeBlob
   /*!
    @param group only for formatting error messages
    @return an error message
    */
   TranslatableString InterpretBlob(AudioUnitEffectSettings &settings,
      const wxString &group, const wxMemoryBuffer &buf) const;

   bool FetchSettings(AudioUnitEffectSettings &settings) const;
   bool StoreSettings(const AudioUnitEffectSettings &settings) const;

   bool CreateAudioUnit();

   AudioUnit GetAudioUnit() const { return mUnit.get(); }
   const Parameters &GetParameters() const
   { return mParameters; }

protected:
   const AudioComponent mComponent;
   AudioUnitCleanup<AudioUnit, AudioComponentInstanceDispose> mUnit;

   Parameters mOwnParameters;
   Parameters &mParameters;
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
