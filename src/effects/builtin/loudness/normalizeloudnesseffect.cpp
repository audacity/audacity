/*
 * Audacity: A Digital Audio Editor
 */
/**********************************************************************

  Audacity: A Digital Audio Editor

  LoudnessBase.cpp

  Max Maisel

*******************************************************************//**

\class LoudnessBase
\brief An Effect to bring the loudness level up to a chosen level.

*//*******************************************************************/
#include "normalizeloudnesseffect.h"

#include "libraries/lib-effects/EffectOutputTracks.h"
#include "libraries/lib-math/EBUR128.h"
#include "libraries/lib-wave-track/WaveChannelUtilities.h"
#include "libraries/lib-wave-track/WaveTrack.h"
#include "libraries/lib-command-parameters/ShuttleAutomation.h"

#include <cmath>

namespace au::effects {
const ComponentInterfaceSymbol NormalizeLoudnessEffect::Symbol { XO(
                                                                     "Loudness Normalization") };

const EffectParameterMethods& NormalizeLoudnessEffect::Parameters() const
{
    static CapturedParameters<
        NormalizeLoudnessEffect, StereoInd, LUFSLevel, RMSLevel, DualMono, NormalizeTo>
    parameters;
    return parameters;
}

NormalizeLoudnessEffect::NormalizeLoudnessEffect()
{
    Parameters().Reset(*this);
    SetLinearEffectFlag(false);
}

NormalizeLoudnessEffect::~NormalizeLoudnessEffect()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol NormalizeLoudnessEffect::GetSymbol() const
{
    return Symbol;
}

TranslatableString NormalizeLoudnessEffect::GetDescription() const
{
    return XO("Sets the loudness of one or more tracks");
}

ManualPageID NormalizeLoudnessEffect::ManualPage() const
{
    return L"Loudness_Normalization";
}

// EffectDefinitionInterface implementation

::EffectType NormalizeLoudnessEffect::GetType() const
{
    return EffectTypeProcess;
}

// Effect implementation

bool NormalizeLoudnessEffect::Process(::EffectInstance&, EffectSettings&)
{
    const float ratio = DB_TO_LINEAR(
        (mNormalizeTo == kLoudness) // LU use 10*log10(...) instead of
        ?                           // 20*log10(...) so multiply level by 2
        std::clamp<double>(
            mLUFSLevel * 2, LUFSLevel.min, LUFSLevel.max)   // RMS
        : std::clamp<double>(mRMSLevel, RMSLevel.min, RMSLevel.max));

    // Iterate over each track
    EffectOutputTracks outputs { *mTracks, GetType(), { { mT0, mT1 } } };
    bool bGoodResult = true;
    auto topMsg = XO("Normalizing Loudness...\n");

    AllocBuffers(outputs.Get());
    mProgressVal = 0;

    for (auto pTrack : outputs.Get().Selected<WaveTrack>()) {
        // Get start and end times from track
        double trackStart = pTrack->GetStartTime();
        double trackEnd = pTrack->GetEndTime();

        // Set the current bounds to whichever left marker is
        // greater and whichever right marker is less:
        const double curT0 = std::max(trackStart, mT0);
        const double curT1 = std::min(trackEnd, mT1);

        // Get the track rate
        mCurRate = pTrack->GetRate();

        wxString msg;
        auto trackName = pTrack->GetName();
        // This affects only the progress indicator update during ProcessOne
        mSteps = (mNormalizeTo == kLoudness) ? 2 : 1;

        mProgressMsg = topMsg + XO("Analyzing: %s").Format(trackName);

        const auto channels = pTrack->Channels();
        auto nChannels = mStereoInd ? 1 : channels.size();
        mProcStereo = nChannels > 1;

        const auto processOne = [&](WaveChannel& track) {
            std::optional<EBUR128> loudnessProcessor;
            float RMS[2];

            if (mNormalizeTo == kLoudness) {
                loudnessProcessor.emplace(mCurRate, nChannels);
                if (!ProcessOne(
                        track, nChannels, curT0, curT1, 0, &*loudnessProcessor)) {
                    // Processing failed -> abort
                    return false;
                }
            } else {
                // RMS
                if (mProcStereo) {
                    size_t idx = 0;
                    for (const auto pChannel : channels) {
                        if (!GetTrackRMS(*pChannel, curT0, curT1, RMS[idx])) {
                            return false;
                        }
                        ++idx;
                    }
                } else {
                    if (!GetTrackRMS(track, curT0, curT1, RMS[0])) {
                        return false;
                    }
                }
            }

            // Calculate normalization values the analysis results
            float extent;
            if (mNormalizeTo == kLoudness) {
                extent = loudnessProcessor->IntegrativeLoudness();
            } else {
                // RMS
                extent = RMS[0];
                if (mProcStereo) {
                    // RMS: use average RMS, average must be calculated in quadratic
                    // domain.
                    extent = sqrt((RMS[0] * RMS[0] + RMS[1] * RMS[1]) / 2.0);
                }
            }

            if (extent == 0.0) {
                FreeBuffers();
                return false;
            }
            float mult = ratio / extent;

            if (mNormalizeTo == kLoudness) {
                // Target half the LUFS value if mono (or independent processed
                // stereo) shall be treated as dual mono.
                if (nChannels == 1 && (mDualMono || !IsMono(track))) {
                    mult /= 2.0;
                }

                // LUFS are related to square values so the multiplier must be the
                // xroot.
                mult = sqrt(mult);
            }

            mProgressMsg = topMsg + XO("Processing: %s").Format(trackName);
            if (!ProcessOne(track, nChannels, curT0, curT1, mult, nullptr)) {
                // Processing failed -> abort
                return false;
            }
            return true;
        };

        if (mStereoInd) {
            for (const auto pChannel : channels) {
                if (!(bGoodResult = processOne(*pChannel))) {
                    goto done;
                }
            }
        } else {
            // processOne captured nChannels which is 2 and is passed to
            // LoadBufferBlock, StoreBufferBlock which find the track from the
            // channel and iterate channels
            if (!(bGoodResult = processOne(**pTrack->Channels().begin()))) {
                break;
            }
        }
    }
done:

    if (bGoodResult) {
        outputs.Commit();
    }

    FreeBuffers();
    return bGoodResult;
}

// NormalizeLoudnessEffect implementation

/// Get required buffer size for the largest whole track and allocate buffers.
/// This reduces the amount of allocations required.
void NormalizeLoudnessEffect::AllocBuffers(TrackList& outputs)
{
    mTrackBufferCapacity = 0;
    bool stereoTrackFound = false;
    double maxSampleRate = 0;
    mProcStereo = false;

    for (auto track : outputs.Selected<WaveTrack>() + &Track::Any) {
        mTrackBufferCapacity
            =std::max(mTrackBufferCapacity, track->GetMaxBlockSize());
        maxSampleRate = std::max(maxSampleRate, track->GetRate());

        // There is a stereo track
        if (track->NChannels() == 2) {
            stereoTrackFound = true;
        }
    }

    // Initiate a processing buffer. This buffer will (most likely)
    // be shorter than the length of the track being processed.
    mTrackBuffer[0].reinit(mTrackBufferCapacity);

    if (!mStereoInd && stereoTrackFound) {
        mTrackBuffer[1].reinit(mTrackBufferCapacity);
    }
}

void NormalizeLoudnessEffect::FreeBuffers()
{
    mTrackBuffer[0].reset();
    mTrackBuffer[1].reset();
}

bool NormalizeLoudnessEffect::GetTrackRMS(
    WaveChannel& track, const double curT0, const double curT1, float& rms)
{
    // set mRMS.  No progress bar here as it's fast.
    float _rms = WaveChannelUtilities::GetRMS(track, curT0, curT1); // may throw
    rms = _rms;
    return true;
}

/// ProcessOne() takes a track, transforms it to bunch of buffer-blocks,
/// and executes ProcessData, on it...
///  uses mMult to normalize a track.
///  mMult must be set before this is called
/// In analyse mode, it executes the selected analyse operation on it...
///  mMult does not have to be set before this is called
bool NormalizeLoudnessEffect::ProcessOne(
    WaveChannel& track, size_t nChannels, const double curT0, const double curT1,
    const float mult, EBUR128* pLoudnessProcessor)
{
    // Transform the marker timepoints to samples
    auto start = track.TimeToLongSamples(curT0);
    auto end = track.TimeToLongSamples(curT1);

    // Get the length of the buffer (as double). len is
    // used simply to calculate a progress meter, so it is easier
    // to make it a double now than it is to do it later
    mTrackLen = (end - start).as_double();

    // Abort if the right marker is not to the right of the left marker
    if (curT1 <= curT0) {
        return false;
    }

    // Go through the track one buffer at a time. s counts which
    // sample the current buffer starts at.
    auto s = start;
    while (s < end)
    {
        // Get a block of samples (smaller than the size of the buffer)
        // Adjust the block size if it is the final block in the track
        auto blockLen
            =limitSampleBufferSize(track.GetBestBlockSize(s), mTrackBufferCapacity);

        const size_t remainingLen = (end - s).as_size_t();
        blockLen = blockLen > remainingLen ? remainingLen : blockLen;
        LoadBufferBlock(track, nChannels, s, blockLen);

        // Process the buffer.
        if (pLoudnessProcessor) {
            if (!AnalyseBufferBlock(*pLoudnessProcessor)) {
                return false;
            }
        } else {
            if (!ProcessBufferBlock(mult)) {
                return false;
            }
            if (!StoreBufferBlock(track, nChannels, s, blockLen)) {
                return false;
            }
        }

        // Increment s one blockfull of samples
        s += blockLen;
    }

    // Return true because the effect processing succeeded ... unless cancelled
    return true;
}

void NormalizeLoudnessEffect::LoadBufferBlock(
    WaveChannel& track, size_t nChannels, sampleCount pos, size_t len)
{
    size_t idx = 0;
    const auto getOne = [&](WaveChannel& channel) {
        // Get the samples from the track and put them in the buffer
        channel.GetFloats(mTrackBuffer[idx].get(), pos, len);
    };

    if (nChannels == 1) {
        getOne(track);
    } else {
        for (const auto channel : track.GetTrack().Channels()) {
            getOne(*channel);
            ++idx;
        }
    }
    mTrackBufferLen = len;
}

/// Calculates sample sum (for DC) and EBU R128 weighted square sum
/// (for loudness).
bool NormalizeLoudnessEffect::AnalyseBufferBlock(EBUR128& loudnessProcessor)
{
    for (size_t i = 0; i < mTrackBufferLen; i++) {
        loudnessProcessor.ProcessSampleFromChannel(mTrackBuffer[0][i], 0);
        if (mProcStereo) {
            loudnessProcessor.ProcessSampleFromChannel(mTrackBuffer[1][i], 1);
        }
        loudnessProcessor.NextSample();
    }

    if (!UpdateProgress()) {
        return false;
    }
    return true;
}

bool NormalizeLoudnessEffect::ProcessBufferBlock(const float mult)
{
    for (size_t i = 0; i < mTrackBufferLen; i++) {
        mTrackBuffer[0][i] = mTrackBuffer[0][i] * mult;
        if (mProcStereo) {
            mTrackBuffer[1][i] = mTrackBuffer[1][i] * mult;
        }
    }

    if (!UpdateProgress()) {
        return false;
    }
    return true;
}

bool NormalizeLoudnessEffect::StoreBufferBlock(
    WaveChannel& track, size_t nChannels, sampleCount pos, size_t len)
{
    size_t idx = 0;
    const auto setOne = [&](WaveChannel& channel) {
        // Copy the newly-changed samples back onto the track.
        return channel.SetFloats(mTrackBuffer[idx].get(), pos, len);
    };

    if (nChannels == 1) {
        return setOne(track);
    } else {
        for (auto channel : track.GetTrack().Channels()) {
            if (!setOne(*channel)) {
                return false;
            }
            ++idx;
        }
        return true;
    }
}

bool NormalizeLoudnessEffect::UpdateProgress()
{
    mProgressVal
        +=(double(1 + mProcStereo) * double(mTrackBufferLen)
           / (double(GetNumWaveTracks()) * double(mSteps) * mTrackLen));
    return !TotalProgress(mProgressVal, mProgressMsg);
}
}
