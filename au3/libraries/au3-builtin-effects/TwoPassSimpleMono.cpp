/**********************************************************************

  Audacity: A Digital Audio Editor

  TwoPassSimpleMono.cpp

  Dominic Mazzoni

*******************************************************************//**
\class EffectTwoPassSimpleMono
\brief An Effect base class that implements a two pass process.

Inherit from it if your effect needs to pass twice over the data.
It does the first pass on all selected tracks before going back and
doing the second pass over all selected tracks.

*//*******************************************************************/
#include "TwoPassSimpleMono.h"
#include "EffectOutputTracks.h"
#include "WaveTrack.h"

EffectTwoPassSimpleMono::~EffectTwoPassSimpleMono() = default;

bool EffectTwoPassSimpleMono::Process(
    EffectInstance&, EffectSettings& settings)
{
    mPass = 0;
    mSecondPassDisabled = false;

    InitPass1();
    EffectOutputTracks outputs { *mTracks, GetType(), { { mT0, mT1 } } };

    mWorkTracks = TrackList::Create(const_cast<AudacityProject*>(FindProject()));
    for (auto track : outputs.Get().Selected<WaveTrack>()) {
        auto pNewTrack = track->EmptyCopy();
        mWorkTracks->Add(pNewTrack);
    }
    for (const auto pNewTrack : mWorkTracks->Any<WaveTrack>()) {
        pNewTrack->ConvertToSampleFormat(floatSample);
    }

    mTrackLists[0] = &outputs.Get();
    mTrackLists[1] = mSecondPassDisabled ? mTrackLists[0] : &*mWorkTracks;

    bool bGoodResult = ProcessPass(settings);
    if (bGoodResult && !mSecondPassDisabled) {
        mPass = 1;
        if (InitPass2()) {
            bGoodResult = ProcessPass(settings);
        }
    }

    mWorkTracks->Clear();
    mWorkTracks.reset();

    if (bGoodResult) {
        outputs.Commit();
    }

    return bGoodResult;
}

bool EffectTwoPassSimpleMono::ProcessPass(EffectSettings& settings)
{
    // Iterate over each track
    mCurTrackNum = 0;

    auto outTracks
        =(*mTrackLists[1 - mPass]).Selected<WaveTrack>().begin();
    for (auto track : (*mTrackLists[mPass]).Selected<WaveTrack>()) {
        auto outTrack = *outTracks;

        // Get start and end times from the orignal track
        double trackStart = mPass == 0 ? track->GetStartTime() : (*outTracks)->GetStartTime();
        double trackEnd = mPass == 0 ? track->GetEndTime() : (*outTracks)->GetEndTime();

        // Set the current bounds to whichever left marker is
        // greater and whichever right marker is less:
        mCurT0 = std::max(trackStart, mT0);
        mCurT1 = std::min(trackEnd, mT1);

        // So that in ProcessOne(), when mPass == 0, samples are
        // appended starting at mCurT0
        if (!mSecondPassDisabled && mPass == 0 && mCurT0 > 0) {
            (*outTracks)->InsertSilence(0, mCurT0);
        }

        // Process only if the right marker is to the right of the left marker
        if (mCurT1 > mCurT0) {
            // Transform the marker timepoints to samples
            auto start = track->TimeToLongSamples(mCurT0);
            auto end = track->TimeToLongSamples(mCurT1);

            // Get the track rate and samples
            mCurRate = track->GetRate();

            // NewTrackPass1/2() returns true by default
            bool ret;
            if (mPass == 0) {
                ret = NewTrackPass1();
            } else {
                ret = NewTrackPass2();
            }
            if (!ret) {
                return false;
            }

            // ProcessOne() (implemented below) processes a single track
            auto outIter = outTrack->Channels().begin();
            for (const auto pChannel : track->Channels()) {
                if (!ProcessOne(*pChannel, **outIter++, start, end)) {
                    return false;
                }
            }
            if (!mSecondPassDisabled && mPass == 0) {
                outTrack->Flush();
            }
        }

        ++mCurTrackNum;
        ++outTracks;
    }

    return true;
}

// ProcessOne() takes a track, transforms it to bunch of buffer-blocks,
// and executes TwoBufferProcessPass1 or TwoBufferProcessPass2 on these blocks
bool EffectTwoPassSimpleMono::ProcessOne(WaveChannel& track,
                                         WaveChannel& outTrack, sampleCount start, sampleCount end)
{
    bool ret;

    // Get the length of the buffer (as double). len is
    // used simple to calculate a progress meter, so it is easier
    // to make it a double now than it is to do it later
    auto len = (end - start).as_double();
    auto maxblock = track.GetMaxBlockSize();

    // Initiate a processing buffer.  This buffer will (most likely)
    // be shorter than the length of the track being processed.
    Floats buffer1{ maxblock };
    Floats buffer2{ maxblock };
    auto samples1 =  limitSampleBufferSize(
        std::min(maxblock, track.GetBestBlockSize(start)), end - start);

    // Get the samples from the track and put them in the buffer
    track.GetFloats(buffer1.get(), start, samples1);

    // Process the first buffer with a null previous buffer
    if (mPass == 0) {
        ret = TwoBufferProcessPass1(nullptr, 0, buffer1.get(), samples1);
    } else {
        ret = TwoBufferProcessPass2(nullptr, 0, buffer1.get(), samples1);
    }
    if (!ret) {
        // Return false because the effect failed.
        return false;
    }

    // Go through the track one buffer at a time. s counts which
    // sample the current buffer starts at.
    auto s = start + samples1;
    while (s < end) {
        // Get a block of samples (smaller than the size of the buffer)
        // Adjust the block size if it is the final block in the track
        auto samples2 = limitSampleBufferSize(
            std::min(track.GetBestBlockSize(s), maxblock), end - s);

        // Get the samples from the track and put them in the buffer
        track.GetFloats(buffer2.get(), s, samples2);

        // Process the buffer.  If it fails, clean up and exit.
        if (mPass == 0) {
            ret = TwoBufferProcessPass1(buffer1.get(), samples1, buffer2.get(),
                                        samples2);
        } else {
            ret = TwoBufferProcessPass2(buffer1.get(), samples1, buffer2.get(),
                                        samples2);
        }
        if (!ret) {
            // Return false because the effect failed.
            return false;
        }

        // Processing succeeded. copy the newly-changed samples back
        // onto the track.
        if (mSecondPassDisabled || mPass != 0) {
            if (!outTrack.SetFloats(buffer1.get(), s - samples1,
                                    samples1)) {
                return false;
            }
        } else {
            outTrack.Append((samplePtr)buffer1.get(), floatSample, samples1);
        }

        // Increment s one blockfull of samples
        s += samples2;

        // Update the Progress meter
        if (mSecondPassDisabled) {
            ret = TotalProgress(
                (mCurTrackNum + (s - start).as_double() / len)
                / GetNumWaveTracks());
        } else {
            ret = TotalProgress(
                (mCurTrackNum + (s - start).as_double() / len
                 + GetNumWaveTracks() * mPass)
                / (GetNumWaveTracks() * 2));
        }
        if (ret) {
            // Return false because the effect failed.
            return false;
        }

        // Rotate the buffers
        buffer1.swap(buffer2);
        std::swap(samples1, samples2);
    }

    // Send the last buffer with a null pointer for the current buffer
    if (mPass == 0) {
        ret = TwoBufferProcessPass1(buffer1.get(), samples1, nullptr, 0);
    } else {
        ret = TwoBufferProcessPass2(buffer1.get(), samples1, nullptr, 0);
    }

    if (!ret) {
        //Return false because the effect failed.
        return false;
    }

    // Processing succeeded. copy the newly-changed samples back
    // onto the track.
    if (mSecondPassDisabled || mPass != 0) {
        if (!outTrack.SetFloats(buffer1.get(), s - samples1,
                                samples1)) {
            return false;
        }
    } else {
        outTrack.Append((samplePtr)buffer1.get(), floatSample, samples1);
    }

    // Return true because the effect processing succeeded.
    return true;
}

bool EffectTwoPassSimpleMono::NewTrackPass1()
{
    return true;
}

bool EffectTwoPassSimpleMono::NewTrackPass2()
{
    return true;
}

//Initialisations before the first pass
bool EffectTwoPassSimpleMono::InitPass1()
{
    return true;
}

//Initialisations before the second pass.
//Return true if you actually want the second pass to go ahead
bool EffectTwoPassSimpleMono::InitPass2()
{
    return true;
}
