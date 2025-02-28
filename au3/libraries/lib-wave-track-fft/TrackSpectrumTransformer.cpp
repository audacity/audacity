/*!********************************************************************

Audacity: A Digital Audio Editor

TrackSpectrumTransformer.cpp

Paul Licameli

**********************************************************************/
#include "TrackSpectrumTransformer.h"

#include "WaveTrack.h"

void TrackSpectrumTransformer::DoOutput(
    const float* outBuffer, size_t mStepSize)
{
    mOutputTrack->Append((constSamplePtr)outBuffer, floatSample, mStepSize);
}

bool TrackSpectrumTransformer::Process(
    const WindowProcessor& processor, const WaveChannel& channel,
    size_t queueLength, sampleCount start, sampleCount len)
{
    if (!Start(queueLength)) {
        return false;
    }

    auto bufferSize = channel.GetMaxBlockSize();
    FloatVector buffer(bufferSize);

    bool bLoopSuccess = true;
    auto samplePos = start;
    while (bLoopSuccess && samplePos < start + len)
    {
        // Get a blockSize of samples (smaller than the size of the buffer)
        const auto blockSize = limitSampleBufferSize(
            std::min(bufferSize, channel.GetBestBlockSize(samplePos)),
            start + len - samplePos);

        // Get the samples from the track and put them in the buffer
        channel.GetFloats(buffer.data(), samplePos, blockSize);
        samplePos += blockSize;
        bLoopSuccess = ProcessSamples(processor, buffer.data(), blockSize);
    }

    if (!Finish(processor)) {
        return false;
    }

    return bLoopSuccess;
}

bool TrackSpectrumTransformer::DoFinish()
{
    return SpectrumTransformer::DoFinish();
}

bool TrackSpectrumTransformer::PostProcess(
    WaveTrack& outputTrack, sampleCount len)
{
    outputTrack.Flush();
    auto tLen = outputTrack.LongSamplesToTime(len);
    // Filtering effects always end up with more data than they started with.
    // Delete this 'tail'.
    outputTrack.Clear(tLen, outputTrack.GetEndTime());
    return true;
}

TrackSpectrumTransformer::~TrackSpectrumTransformer() = default;

bool TrackSpectrumTransformer::DoStart()
{
    return SpectrumTransformer::DoStart();
}
