/**********************************************************************

Audacity: A Digital Audio Editor

SampleTrack.cpp

Paul Licameli split from WaveTrack.cpp

**********************************************************************/

#include "SampleTrack.h"

#include <cmath>

SampleTrack::SampleTrack() = default;

SampleTrack::SampleTrack(const SampleTrack& other, ProtectedCreationArg&& a)
    : PlayableTrack(other, std::move(a))
{
}

SampleTrack::~SampleTrack() = default;

static const Track::TypeInfo& typeInfo()
{
    static const Track::TypeInfo info{
        { "sample", "sample", XO("Sample Track") },
        true, &PlayableTrack::ClassTypeInfo() };
    return info;
}

auto SampleTrack::ClassTypeInfo() -> const TypeInfo&
{
    return typeInfo();
}

auto SampleTrack::GetTypeInfo() const -> const TypeInfo&
{
    return typeInfo();
}

WritableSampleTrack::WritableSampleTrack() = default;

WritableSampleTrack::WritableSampleTrack(
    const WritableSampleTrack& other, ProtectedCreationArg&& a)
    : SampleTrack(other, std::move(a))
{
}

WritableSampleTrack::~WritableSampleTrack() = default;

static const Track::TypeInfo& typeInfo2()
{
    static const Track::TypeInfo info{
        { "writable-sample", "writable-sample", XO("Writable Sample Track") },
        true, &SampleTrack::ClassTypeInfo() };
    return info;
}

auto WritableSampleTrack::ClassTypeInfo() -> const TypeInfo&
{
    return typeInfo2();
}

auto WritableSampleTrack::GetTypeInfo() const -> const TypeInfo&
{
    return typeInfo2();
}
