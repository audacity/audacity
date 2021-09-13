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
   // Public interface
   using FloatVector = std::vector<float>;

   /*!
    @pre `windowSize % stepsPerWindow == 0`
    @pre `windowSize` is a power of 2
    */
   SpectrumTransformer(
      size_t windowSize,     //!< must be a power of 2
      unsigned stepsPerWindow //!< determines the overlap
   );

   virtual ~SpectrumTransformer();

   const size_t mWindowSize;
   const size_t mSpectrumSize;

   const unsigned mStepsPerWindow;
   const size_t mStepSize;

   HFFT     hFFT;
   sampleCount mInSampleCount = 0;
   sampleCount mOutStepCount = 0; //!< sometimes negative
   size_t mInWavePos = 0;

   //! These have size mWindowSize:
   FloatVector mFFTBuffer;
   FloatVector mInWaveBuffer;
   FloatVector mOutOverlapBuffer;
   //! These have size mWindowSize, or 0 for rectangular window:
   FloatVector mInWindow;
   FloatVector mOutWindow;
};

class WaveTrack;

//! Subclass of SpectrumTransformer that rewrites a track
class TrackSpectrumTransformer /* not final */ : public SpectrumTransformer {
public:
   using SpectrumTransformer::SpectrumTransformer;
   ~TrackSpectrumTransformer() override;
};

#endif
