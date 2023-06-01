#include "WaveClipSegment.h"
#include "WaveClipProcessor.h"

WaveClipSegment::WaveClipSegment(WaveClipHolder waveClip)
    : mWaveClip(waveClip)
    , mWaveClipProcessor(*mWaveClip)
{
}

AudioSegmentProcessor& WaveClipSegment::GetProcessor() const
{
   return const_cast<WaveClipProcessor&>(mWaveClipProcessor);
}

sampleCount WaveClipSegment::GetNumSamples() const
{
   return mWaveClip->GetPlaySamplesCount();
}
