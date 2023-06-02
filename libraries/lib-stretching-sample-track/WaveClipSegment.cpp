#include "WaveClipSegment.h"
#include "WaveClipProcessor.h"

WaveClipSegment::WaveClipSegment(
   WaveClipHolder waveClip, double offsetFromPlayStartTime)
    : mWaveClip(waveClip)
    , mWaveClipProcessor(*mWaveClip, offsetFromPlayStartTime)
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
