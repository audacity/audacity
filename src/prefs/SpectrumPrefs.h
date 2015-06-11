/**********************************************************************

  Audacity: A Digital Audio Editor

  SpectrumPrefs.h

  Dominic Mazzoni
  James Crook

**********************************************************************/
/*
  Salvo Ventura
  November 2006

  Added selection box for windowType

  All params are saved in config file.
*/


#ifndef __AUDACITY_SPECTRUM_PREFS__
#define __AUDACITY_SPECTRUM_PREFS__

#include <wx/defs.h>
#include <wx/string.h>
#include <wx/window.h>

#include "../Experimental.h"
#include "../ShuttleGui.h"

#include "PrefsPanel.h"

struct FFTParam;

class SpectrumPrefs:public PrefsPanel
{
 public:
   SpectrumPrefs(wxWindow * parent);
   virtual ~SpectrumPrefs();
   virtual bool Apply();
   virtual bool Validate();

 private:
   void Populate(int windowSize);
   void PopulatePaddingChoices(int windowSize);
   void PopulateOrExchange(ShuttleGui & S);

   void OnWindowSize(wxCommandEvent &event);
   DECLARE_EVENT_TABLE()

   wxTextCtrl *mMinFreq;
   wxTextCtrl *mMaxFreq;
   wxTextCtrl *mGain;
   wxTextCtrl *mRange;
   wxTextCtrl *mFrequencyGain;

   wxArrayString mSizeChoices;
   wxArrayInt mSizeCodes;

#ifdef EXPERIMENTAL_ZERO_PADDED_SPECTROGRAMS
   int mZeroPaddingChoice;
   wxArrayString mZeroPaddingChoices;
   wxArrayInt mZeroPaddingCodes;
#endif

   wxArrayString mTypeChoices;
   wxArrayInt mTypeCodes;


#ifdef EXPERIMENTAL_FIND_NOTES
   wxTextCtrl *mFindNotesMinA;
   wxTextCtrl *mFindNotesN;
#endif
};


class SpectrogramSettings
{
public:
   static SpectrogramSettings &defaults();
   SpectrogramSettings();
   ~SpectrogramSettings();

   void UpdatePrefs();
   void DestroyWindows();
   void CacheWindows() const;

   int minFreq;
   int maxFreq;

   int logMinFreq;
   int logMaxFreq;

   int range;
   int gain;
   int frequencyGain;

   int windowType;
   int windowSize;
#ifdef EXPERIMENTAL_ZERO_PADDED_SPECTROGRAMS
   int zeroPaddingFactor;
#endif

   bool isGrayscale;

#ifdef EXPERIMENTAL_FFT_SKIP_POINTS
   int fftSkipPoints;
#endif

#ifdef EXPERIMENTAL_FFT_Y_GRID
   bool fftYGrid;
#endif //EXPERIMENTAL_FFT_Y_GRID

#ifdef EXPERIMENTAL_FIND_NOTES
   bool fftFindNotes;
   bool findNotesMinA;
   bool numberOfMaxima;
   bool findNotesQuantize;
#endif //EXPERIMENTAL_FIND_NOTES

   // Following fields are derived from preferences.

#ifdef EXPERIMENTAL_USE_REALFFTF
   // Variables used for computing the spectrum
   mutable FFTParam      *hFFT;
   mutable float         *window;
#endif
};

#endif
