/*!********************************************************************

Audacity: A Digital Audio Editor

SpectrumTransformer.h
@brief Transformer of sample sequences by FFT, coefficient changes, inverse FFT, overlap-add

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_SPECTRUM_TRANSFORMER__
#define __AUDACITY_SPECTRUM_TRANSFORMER__

class SpectrumTransformer /* not final */
{
public:
   virtual ~SpectrumTransformer();
};

class WaveTrack;

//! Subclass of SpectrumTransformer that rewrites a track
class TrackSpectrumTransformer /* not final */ : public SpectrumTransformer {
public:
   using SpectrumTransformer::SpectrumTransformer;
   ~TrackSpectrumTransformer() override;
};

#endif
