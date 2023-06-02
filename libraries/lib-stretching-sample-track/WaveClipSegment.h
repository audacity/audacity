#include "AudioSegment.h"
#include "WaveClip.h"
#include "WaveClipProcessor.h"

class WaveClipSegment : public AudioSegment
{
public:
   WaveClipSegment(WaveClipHolder waveClip, double offsetFromPlayStartTime);

   AudioSegmentProcessor& GetProcessor() const override;
   sampleCount GetNumSamples() const override;

private:
   const WaveClipHolder mWaveClip;
   WaveClipProcessor mWaveClipProcessor;
};
