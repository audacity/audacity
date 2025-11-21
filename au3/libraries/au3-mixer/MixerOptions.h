/**********************************************************************

  Audacity: A Digital Audio Editor

  @file MixerOptions.h
  @brief supporting classes for Mixer

  Dominic Mazzoni
  Markus Meyer

  Paul Licameli split from Mix.h

***********************************************************************/

#ifndef __AUDACITY_MIXER_OPTIONS__
#define __AUDACITY_MIXER_OPTIONS__

#include "EffectInterface.h" // for EffectSettings
#include "GlobalVariable.h"
#include "MemoryX.h"
#include <functional>
#include <vector>

class AudacityProject;
class BoundedEnvelope;
class EffectInstanceEx;
class WideSampleSequence;

namespace MixerOptions {
//! A matrix of booleans, one row per input channel, column per output
class MIXER_API Downmix final
{
    unsigned mNumTracks, mNumChannels, mMaxNumChannels;

    void Alloc();

public:
    ArraysOf<bool> mMap;

    Downmix(unsigned numTracks, unsigned maxNumChannels);
    Downmix(const Downmix& mixerSpec);
    //!brief Copies channel map for mixerSpec for tracks specified in tracksMask
    Downmix(const Downmix& mixerSpec, const std::vector<bool>& tracksMask);
    ~Downmix();

    bool SetNumChannels(unsigned numChannels);
    unsigned GetNumChannels() const { return mNumChannels; }

    unsigned GetMaxNumChannels() const { return mMaxNumChannels; }
    unsigned GetNumTracks() const { return mNumTracks; }

    Downmix& operator=(const Downmix& mixerSpec);
};

//! Immutable structure is an argument to Mixer's constructor
struct MIXER_API Warp final {
    //! Hook function for default time warp
    struct MIXER_API DefaultWarp : GlobalHook<DefaultWarp,
                                              const BoundedEnvelope* (const AudacityProject*)
                                              > {};

    //! Construct using the default warp function
    explicit Warp(const AudacityProject* pProject);

    //! Construct with an explicit warp
    explicit Warp(const BoundedEnvelope* e);

    //! Construct with no time warp
    /*!
    @pre `min >= 0`
    @pre `max >= 0`
    @pre `min <= max`
    */
    Warp(double min, double max, double initial = 1.0);

    const BoundedEnvelope* const envelope = nullptr;
    const double minSpeed, maxSpeed;
    const double initialSpeed{ 1.0 };
};

// Information derived from Warp and other data
struct ResampleParameters final {
    ResampleParameters(bool highQuality, double inRate, double outRate, const Warp& options);
    bool mHighQuality{};
    bool mVariableRates{ false };
    double mMinFactor, mMaxFactor;
};

//! Reassignable bounds and speed for a Mixer's fetch from tracks, and a
//! readout of last fetched time
struct TimesAndSpeed final {
    // Bounds for fetch position in the sample source
    double mT0;          // Start time
    double mT1;          // Stop time (none if mT0==mT1)
    // Varying scrub speed is one cause for resampling
    double mSpeed;

    // For output purposes only (like progress indicator update)
    double mTime;           // Current time (renamed from mT to mTime for
    // consistency with AudioIO - mT represented warped time there)
};

struct StageSpecification final {
    using Factory = std::function<std::shared_ptr<EffectInstance>()>;

    const Factory factory;
    EffectSettings settings;

    mutable std::shared_ptr<EffectInstance> mpFirstInstance;
};
}
#endif
