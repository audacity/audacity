/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioUnitUtils.cpp

  Paul Licameli

***********************************************************************/

#include "AudioUnitUtils.h"
#include "MemoryX.h"

OSStatus AudioUnitUtils::GetFixedSizePropertyPtr(AudioUnit unit,
                                                 AudioUnitPropertyID inID, void* pProperty, const UInt32 size,
                                                 AudioUnitScope inScope, AudioUnitElement inElement)
{
    auto newSize = size;
    auto result = AudioUnitGetProperty(unit, inID, inScope, inElement,
                                       pProperty, &newSize);
    assert(newSize <= size);
    return result;
}

OSStatus AudioUnitUtils::GetVariableSizePropertyPtr(AudioUnit unit,
                                                    AudioUnitPropertyID inID, const size_t minSize,
                                                    void*& pObject, size_t& size,
                                                    AudioUnitScope inScope, AudioUnitElement inElement)
{
    size = 0;

    // Query for statically unknown size of the property first
    UInt32 dataSize;
    auto result = AudioUnitGetPropertyInfo(unit, inID, inScope, inElement,
                                           &dataSize, nullptr);
    if (result) {
        return result;
    }

    // Allocate
    auto newSize = std::max<size_t>(minSize, dataSize);
    auto pObj = ::operator new(newSize);
    auto cleanup = finally([&]{
        // In case AudioUnitGetProperty fails
        if (!pObject) {
            ::operator delete(pObj);
        }
    });

    // Try to get the property
    dataSize = newSize;
    result
        =AudioUnitGetProperty(unit, inID, inScope, inElement, pObj, &dataSize);
    if (result) {
        pObject = nullptr;
        return result;
    }

    // Success
    pObject = pObj;
    size = newSize;
    return result;
}

OSStatus AudioUnitUtils::SetPropertyPtr(AudioUnit unit,
                                        AudioUnitPropertyID inID, const void* pProperty, UInt32 size,
                                        AudioUnitScope inScope, AudioUnitElement inElement)
{
    return AudioUnitSetProperty(unit, inID, inScope, inElement,
                                pProperty, size);
}
