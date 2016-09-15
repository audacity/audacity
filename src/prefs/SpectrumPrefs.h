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

#include "PrefsPanel.h"
#include "SpectrogramSettings.h"

class wxChoice;
class wxCheckBox;
class wxTextCtrl;
struct FFTParam;
class ShuttleGui;
class SpectrogramSettings;
class WaveTrack;

class SpectrumPrefs final : public PrefsPanel
{
 public:
   SpectrumPrefs(wxWindow * parent, WaveTrack *wt);
   virtual ~SpectrumPrefs();
   bool Apply() override;
   bool ShowsApplyButton() override;
   bool Validate() override;

 private:
   void Populate(size_t windowSize);
   void PopulatePaddingChoices(size_t windowSize);
   void PopulateOrExchange(ShuttleGui & S);

   void OnControl(wxCommandEvent &event);
   void OnWindowSize(wxCommandEvent &event);
   void OnDefaults(wxCommandEvent&);
   void OnAlgorithm(wxCommandEvent &);
   DECLARE_EVENT_TABLE()

   void EnableDisableSTFTOnlyControls();

   WaveTrack *const mWt;
   bool mDefaulted;

   wxTextCtrl *mMinFreq;
   wxTextCtrl *mMaxFreq;
   wxTextCtrl *mGain;
   wxTextCtrl *mRange;
   wxTextCtrl *mFrequencyGain;

   wxArrayString mSizeChoices;

#ifdef EXPERIMENTAL_ZERO_PADDED_SPECTROGRAMS
   int mZeroPaddingChoice;
   wxChoice *mZeroPaddingChoiceCtrl;
   wxArrayString mZeroPaddingChoices;
#endif

   wxArrayString mTypeChoices;
   wxArrayString mScaleChoices;

   wxChoice *mAlgorithmChoice;
   wxArrayString mAlgorithmChoices;


#ifdef EXPERIMENTAL_FIND_NOTES
   wxTextCtrl *mFindNotesMinA;
   wxTextCtrl *mFindNotesN;
#endif

   wxCheckBox *mDefaultsCheckbox;

   SpectrogramSettings mTempSettings;

   bool mPopulating;
};

class SpectrumPrefsFactory final : public PrefsPanelFactory
{
public:
   explicit SpectrumPrefsFactory(WaveTrack *wt = 0);
   PrefsPanel *Create(wxWindow *parent) override;

private:
   WaveTrack *const mWt;
};
#endif
