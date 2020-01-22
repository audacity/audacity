/**********************************************************************

  Audacity: A Digital Audio Editor

  SpectrumAnalyst.h

  Dominic Mazzoni
  Paul Licameli split from FreqWindow.h

**********************************************************************/

#ifndef __AUDACITY_SPECTRUM_ANALYST__
#define __AUDACITY_SPECTRUM_ANALYST__

#include <vector>
#include <wx/statusbr.h>

class FreqGauge;

class AUDACITY_DLL_API SpectrumAnalyst
{
public:

   enum Algorithm {
      Spectrum,
      Autocorrelation,
      CubeRootAutocorrelation,
      EnhancedAutocorrelation,
      Cepstrum,

      NumAlgorithms
   };

   SpectrumAnalyst();
   ~SpectrumAnalyst();

   // Return true iff successful
   bool Calculate(Algorithm alg,
      int windowFunc, // see FFT.h for values
      size_t windowSize, double rate,
      const float *data, size_t dataLen,
      float *pYMin = NULL, float *pYMax = NULL, // outputs
      FreqGauge *progress = NULL);

   const float *GetProcessed() const;
   int GetProcessedSize() const;

   float GetProcessedValue(float freq0, float freq1) const;
   float FindPeak(float xPos, float *pY) const;

private:
   float CubicInterpolate(float y0, float y1, float y2, float y3, float x) const;
   float CubicMaximize(float y0, float y1, float y2, float y3, float * max) const;

private:
   Algorithm mAlg;
   double mRate;
   size_t mWindowSize;
   std::vector<float> mProcessed;
};

class AUDACITY_DLL_API FreqGauge final : public wxStatusBar
{
public:
   FreqGauge(wxWindow * parent, wxWindowID winid);

   void SetRange(int range, int bar = 12, int gap = 3);
   void SetValue(int value);
   void Reset();

private:
   wxRect mRect;
   int mRange;
   int mCur;
   int mLast;
   int mInterval;
   int mBar;
   int mGap;
   int mMargin;
};

#endif
