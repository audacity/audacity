/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioUnitUtils.h

  Paul Licameli

***********************************************************************/

#ifndef __AUDACITY_AUDIO_UNIT_UTILS__
#define __AUDACITY_AUDIO_UNIT_UTILS__

#include <algorithm>
#include <AudioUnit/AudioUnit.h>
#include "PackedArray.h"

namespace AudioUnitUtils {
   //! Type-erased function to get an AudioUnit property of fixed size
   OSStatus GetFixedSizePropertyPtr(AudioUnit unit, AudioUnitPropertyID inID,
      void *pProperty, UInt32 size, AudioUnitScope inScope,
      AudioUnitElement inElement);

   //! Get an AudioUnit property of deduced type and fixed size,
   //! supplying most often used values as defaults for scope and element
   template<typename T>
   OSStatus GetFixedSizeProperty(AudioUnit unit, AudioUnitPropertyID inID,
      T &property,
      AudioUnitScope inScope = kAudioUnitScope_Global,
      AudioUnitElement inElement = 0)
   {
      return GetFixedSizePropertyPtr(unit, inID,
         &property, sizeof(property), inScope, inElement);
   }

   //! Type-erased function to get an AudioUnit property of variable size
   OSStatus GetVariableSizePropertyPtr(AudioUnit unit, AudioUnitPropertyID inID,
      size_t minSize, void *&pObject, size_t &size,
      AudioUnitScope inScope, AudioUnitElement inElement);

   //! Get an AudioUnit property of deduced type and variable size,
   //! supplying most often used values as defaults for scope and element
   /*!
    Warning: on success, performs a "naked" allocation in pObject!
    Else, nulls it.
    */
   template<typename T>
   OSStatus GetVariableSizeProperty(AudioUnit unit, AudioUnitPropertyID inID,
      PackedArrayPtr<T> &pObject,
      AudioUnitScope inScope = kAudioUnitScope_Global,
      AudioUnitElement inElement = 0)
   {
      void *p{};
      size_t size{};
      auto result = GetVariableSizePropertyPtr(unit, inID,
         sizeof(typename PackedArrayTraits<T>::header_type), p, size,
         inScope, inElement);
      if (!result)
         pObject = { static_cast<T*>(p), size };
      return result;
   }

   //! Type-erased function to set an AudioUnit property
   OSStatus SetPropertyPtr(AudioUnit unit, AudioUnitPropertyID inID,
      const void *pProperty, UInt32 size, AudioUnitScope inScope,
      AudioUnitElement inElement);

   //! Set an AudioUnit property of deduced type,
   //! supplying most often used values as defaults for scope and element
   template<typename T>
   OSStatus SetProperty(AudioUnit unit, AudioUnitPropertyID inID,
      const T &property,
      AudioUnitScope inScope = kAudioUnitScope_Global,
      AudioUnitElement inElement = 0)
   {
      return SetPropertyPtr(unit, inID,
         &property, sizeof(property), inScope, inElement);
   }
}

#endif
