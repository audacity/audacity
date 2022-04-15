/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioUnitUtils.cpp

  Paul Licameli

***********************************************************************/

#include "AudioUnitUtils.h"

OSStatus AudioUnitUtils::GetFixedSizePropertyPtr(AudioUnit unit,
   AudioUnitPropertyID inID, void *pProperty, UInt32 size,
   AudioUnitScope inScope, AudioUnitElement inElement)
{
   auto newSize = size;
   auto result = AudioUnitGetProperty(unit, inID, inScope, inElement,
      pProperty, &newSize);
   assert(newSize <= size);
   return result;
}

OSStatus AudioUnitUtils::SetPropertyPtr(AudioUnit unit,
   AudioUnitPropertyID inID, const void *pProperty, UInt32 size,
   AudioUnitScope inScope, AudioUnitElement inElement)
{
   return AudioUnitSetProperty(unit, inID, inScope, inElement,
      pProperty, size);
}
