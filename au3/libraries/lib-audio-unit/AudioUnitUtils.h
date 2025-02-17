/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioUnitUtils.h

  Paul Licameli

***********************************************************************/

#ifndef __AUDACITY_AUDIO_UNIT_UTILS__
#define __AUDACITY_AUDIO_UNIT_UTILS__

#include <algorithm>
#include <AudioUnit/AudioUnit.h>
#include "CFResources.h"
#include "PackedArray.h"

//! Generates deleters for std::unique_ptr that clean up AU plugin state
template<typename T, OSStatus(*fn)(T*)> struct AudioUnitCleaner {
    // Let this have non-void return type, though ~unique_ptr() will ignore it
    auto operator ()(T* p) noexcept { return fn(p); }
};
//! RAII for cleaning up AU plugin state
template<typename Ptr, OSStatus(*fn)(Ptr),
         typename T = std::remove_pointer_t<Ptr> /* deduced */ >
using AudioUnitCleanup = std::unique_ptr<T, AudioUnitCleaner<T, fn> >;

/*! @namespace AudioUnitUtils
 */
namespace AudioUnitUtils {
//! Type-erased function to get an AudioUnit property of fixed size
OSStatus GetFixedSizePropertyPtr(AudioUnit unit, AudioUnitPropertyID inID, void* pProperty, UInt32 size, AudioUnitScope inScope,
                                 AudioUnitElement inElement);

//! Get an AudioUnit property of deduced type and fixed size,
//! supplying most often used values as defaults for scope and element
template<typename T>
OSStatus GetFixedSizeProperty(AudioUnit unit, AudioUnitPropertyID inID,
                              T& property,
                              AudioUnitScope inScope = kAudioUnitScope_Global,
                              AudioUnitElement inElement = 0)
{
    return GetFixedSizePropertyPtr(unit, inID,
                                   &property, sizeof(property), inScope, inElement);
}

//! Type-erased function to get an AudioUnit property of variable size
/*!
    Warning: on success, performs a "naked" allocation in pObject!
    Else, nulls it.
    */
OSStatus GetVariableSizePropertyPtr(AudioUnit unit, AudioUnitPropertyID inID, size_t minSize, void*& pObject, size_t& size,
                                    AudioUnitScope inScope, AudioUnitElement inElement);

//! Get an AudioUnit property of deduced type and variable size,
//! supplying most often used values as defaults for scope and element,
//! and seating the raw pointer result into a smart pointer
template<typename T>
OSStatus GetVariableSizeProperty(AudioUnit unit, AudioUnitPropertyID inID,
                                 PackedArray::Ptr<T>& pObject,
                                 AudioUnitScope inScope = kAudioUnitScope_Global,
                                 AudioUnitElement inElement = 0)
{
    void* p{};
    size_t size{};
    auto result = GetVariableSizePropertyPtr(unit, inID,
                                             sizeof(typename PackedArray::Traits<T>::header_type), p, size,
                                             inScope, inElement);
    if (!result) {
        // Construct PackedArray::Ptr
        pObject = {
            static_cast<T*>(p), // the pointer
            size // the size that the deleter must remember
        }
    }
    return result;
}

//! Type-erased function to set an AudioUnit property
OSStatus SetPropertyPtr(AudioUnit unit, AudioUnitPropertyID inID, const void* pProperty, UInt32 size, AudioUnitScope inScope,
                        AudioUnitElement inElement);

//! Set an AudioUnit property of deduced type,
//! supplying most often used values as defaults for scope and element
template<typename T>
OSStatus SetProperty(AudioUnit unit, AudioUnitPropertyID inID,
                     const T& property,
                     AudioUnitScope inScope = kAudioUnitScope_Global,
                     AudioUnitElement inElement = 0)
{
    return SetPropertyPtr(unit, inID,
                          &property, sizeof(property), inScope, inElement);
}

/*! @name Wrappers for SDK structures related to AudioUnits

    Allow the use of convenient, brief aggregate initialization syntax but also
    future-proofing in the (unlikely) event that fields of the structures are
    rearranged in the future.  (But if that really happens, then more work
    would be needed to keep Audacity binarily compatible with different runtime
    system versions.)

    Each constructor first zero-initializes all fields of the base structure.

    These wrappers can also define convenient destructors as needed to free
    associated resources, or can abbreviate some constructions by defaulting
    some fields (such as "out" fields).
    */
//! @{

struct Buffer : AudioBuffer {
    Buffer(UInt32 numberChannels, UInt32 dataByteSize, void* data)
        : AudioBuffer{}
    {
        mNumberChannels = numberChannels;
        mDataByteSize = dataByteSize;
        mData = data;
    }
};

struct RenderCallback : AURenderCallbackStruct {
    RenderCallback(AURenderCallback inProc, void* inProcRefCon)
        : AURenderCallbackStruct{}
    {
        inputProc = inProc;
        inputProcRefCon = inProcRefCon;
    }
};

struct Parameter : AudioUnitParameter {
    //! This constructor leaves the parameter ID and element fields as 0
    Parameter(AudioUnit audioUnit, AudioUnitScope scope)
        : AudioUnitParameter{}
    {
        mAudioUnit = audioUnit;
        mScope = scope;
    }
};

struct ParameterInfo : AudioUnitParameterInfo {
    ParameterInfo()
        : AudioUnitParameterInfo{} {}
    ~ParameterInfo()
    {
        if (flags & kAudioUnitParameterFlag_CFNameRelease) {
            if (flags & kAudioUnitParameterFlag_HasCFNameString) {
                if (cfNameString) {
                    CFRelease(cfNameString);
                }
            }
            if (flags & kAudioUnitParameterUnit_CustomUnit) {
                if (unitName) {
                    CFRelease(unitName);
                }
            }
        }
    }
};

struct ParameterNameInfo : AudioUnitParameterNameInfo {
    ParameterNameInfo(AudioUnitParameterID id, SInt32 desiredLength)
        : AudioUnitParameterNameInfo{}
    {
        inID = id;
        inDesiredLength = desiredLength;
    }

    ~ParameterNameInfo()
    {
        if (outName) {
            CFRelease(outName);
        }
    }
};

struct Property : AudioUnitProperty {
    Property(AudioUnit audioUnit, AudioUnitPropertyID propertyID,
             AudioUnitScope scope)
        : AudioUnitProperty{}
    {
        mAudioUnit = audioUnit;
        mPropertyID = propertyID;
        mScope = scope;
        // Default element to 0
    }
};

struct UserPreset : AUPreset {
    UserPreset(CFStringRef name)
        : AUPreset{}
    {
        presetNumber = -1;
        presetName = name;
    }
};

struct StreamBasicDescription : AudioStreamBasicDescription {
    StreamBasicDescription(Float64 sampleRate,
                           AudioFormatID formatID, AudioFormatFlags formatFlags,
                           UInt32 bytesPerPacket, UInt32 framesPerPacket,
                           UInt32 bytesPerFrame, UInt32 channelsPerFrame,
                           UInt32 bitsPerChannel)
        : AudioStreamBasicDescription{}
    {
        mSampleRate = sampleRate;
        mFormatID = formatID;
        mFormatFlags = formatFlags;
        mBytesPerPacket = bytesPerPacket;
        mFramesPerPacket = framesPerPacket;
        mBytesPerFrame = bytesPerFrame;
        mChannelsPerFrame = channelsPerFrame;
        mBitsPerChannel = bitsPerChannel;
        // Just padding:
        // mReserved = 0; // But AudioStreamBasicDescription{} did that
    }
};

//! @}
}

/*!
 @name Traits attached to SDK structures, in the global namespace
 */
//! @{

template<> struct PackedArray::Traits<AudioBufferList> {
    struct header_type {
        UInt32 mNumberBuffers;
    };
    // Overlay the element type with the wrapper type
    using element_type = AudioUnitUtils::Buffer;
    static constexpr auto array_member = &AudioBufferList::mBuffers;
};

template<> struct PackedArray::Traits<AudioUnitCocoaViewInfo> {
    struct header_type {
        CF_ptr<CFURLRef> p1;

        header_type()
        {
            // Sanity checks against toolkit version change
            static_assert(offsetof(header_type, p1)
                          == offsetof(AudioUnitCocoaViewInfo, mCocoaAUViewBundleLocation));
        }
    };
    using element_type = CF_ptr<CFStringRef>;
    static constexpr auto array_member
        =&AudioUnitCocoaViewInfo::mCocoaAUViewClass;
};

//! @}

#endif
