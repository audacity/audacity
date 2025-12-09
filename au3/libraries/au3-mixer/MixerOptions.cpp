/**********************************************************************

  Audacity: A Digital Audio Editor

  @file MixerOptions.cpp

  Dominic Mazzoni
  Markus Meyer
  Vaughan Johnson

  Paul Licameli split from Mix.cpp

**********************************************************************/
#include "MixerOptions.h"

#include "Envelope.h"

MixerOptions::Warp::Warp(const AudacityProject* pProject)
    : envelope(DefaultWarp::Call(pProject)), minSpeed(0.0), maxSpeed(0.0)
{
}

MixerOptions::Warp::Warp(const BoundedEnvelope* e)
    : envelope(e), minSpeed(0.0), maxSpeed(0.0)
{}

MixerOptions::Warp::Warp(double min, double max, double initial)
    : minSpeed{std::max(0.0, std::min(min, max))}
    , maxSpeed{std::max(0.0, std::max(min, max))}
    , initialSpeed{initial}
{
    assert(min >= 0);
    assert(max >= 0);
    assert(min <= max);
}

MixerOptions::ResampleParameters::ResampleParameters(bool highQuality,
                                                     double inRate, double outRate, const Warp& options)
    : mHighQuality{highQuality}
{
    double factor = (outRate / inRate);
    if (const auto envelope = options.envelope) {
        // variable rate resampling
        mVariableRates = true;
        mMinFactor = factor / envelope->GetRangeUpper();
        mMaxFactor = factor / envelope->GetRangeLower();
    } else if (options.minSpeed > 0.0 && options.maxSpeed > 0.0) {
        // variable rate resampling
        mVariableRates = true;
        mMinFactor = factor / options.maxSpeed;
        mMaxFactor = factor / options.minSpeed;
    } else {
        // constant rate resampling
        mVariableRates = false;
        mMinFactor = factor;
        mMaxFactor = factor;
    }
}

MixerOptions::Downmix::Downmix(unsigned numTracks, unsigned maxNumChannels)
{
    mNumTracks = mNumChannels = numTracks;
    mMaxNumChannels = maxNumChannels;

    if (mNumChannels > mMaxNumChannels) {
        mNumChannels = mMaxNumChannels;
    }

    Alloc();

    for ( unsigned int i = 0; i < mNumTracks; i++ ) {
        for ( unsigned int j = 0; j < mNumChannels; j++ ) {
            mMap[ i ][ j ] = (i == j);
        }
    }
}

MixerOptions::Downmix::Downmix(const Downmix& mixerSpec)
{
    mNumTracks = mixerSpec.mNumTracks;
    mMaxNumChannels = mixerSpec.mMaxNumChannels;
    mNumChannels = mixerSpec.mNumChannels;

    Alloc();

    for ( unsigned int i = 0; i < mNumTracks; i++ ) {
        for ( unsigned int j = 0; j < mNumChannels; j++ ) {
            mMap[ i ][ j ] = mixerSpec.mMap[ i ][ j ];
        }
    }
}

MixerOptions::Downmix::Downmix(const Downmix& mixerSpec, const std::vector<bool>& tracksMask)
    : mMaxNumChannels(mixerSpec.mMaxNumChannels)
    , mNumChannels(mixerSpec.mNumChannels)
{
    mNumTracks = static_cast<unsigned>(std::count(tracksMask.begin(), tracksMask.end(), true));
    Alloc();
    unsigned int dstTrackIndex = 0;
    for ( unsigned int srcTrackIndex = 0; srcTrackIndex < tracksMask.size(); srcTrackIndex++ ) {
        if (!tracksMask[srcTrackIndex]) {
            continue;
        }

        for ( unsigned int j = 0; j < mNumChannels; j++ ) {
            mMap[ dstTrackIndex ][ j ] = mixerSpec.mMap[ srcTrackIndex ][ j ];
        }

        ++dstTrackIndex;
    }
}

void MixerOptions::Downmix::Alloc()
{
    mMap.reinit(mNumTracks, mMaxNumChannels);
}

MixerOptions::Downmix::~Downmix()
{
}

bool MixerOptions::Downmix::SetNumChannels(unsigned newNumChannels)
{
    if (mNumChannels == newNumChannels) {
        return true;
    }

    if (newNumChannels > mMaxNumChannels) {
        return false;
    }

    for ( unsigned int i = 0; i < mNumTracks; i++ ) {
        for ( unsigned int j = newNumChannels; j < mNumChannels; j++ ) {
            mMap[ i ][ j ] = false;
        }

        for ( unsigned int j = mNumChannels; j < newNumChannels; j++ ) {
            mMap[ i ][ j ] = false;
        }
    }

    mNumChannels = newNumChannels;
    return true;
}

auto MixerOptions::Downmix::operator=(const Downmix& mixerSpec) -> Downmix
&
{
    mNumTracks = mixerSpec.mNumTracks;
    mNumChannels = mixerSpec.mNumChannels;
    mMaxNumChannels = mixerSpec.mMaxNumChannels;

    Alloc();

    for ( unsigned int i = 0; i < mNumTracks; i++ ) {
        for ( unsigned int j = 0; j < mNumChannels; j++ ) {
            mMap[ i ][ j ] = mixerSpec.mMap[ i ][ j ];
        }
    }

    return *this;
}
