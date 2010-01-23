/**********************************************************************

  Audacity: A Digital Audio Editor

  NoiseRemoval.h

  Dominic Mazzoni
  Vaughan Johnson (Preview)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NOISE_REMOVAL__
#define __AUDACITY_EFFECT_NOISE_REMOVAL__

#include "Effect.h"

#include <wx/dialog.h>
#include <wx/slider.h>

class wxButton;
class wxSizer;
class wxString;

class Envelope;
class WaveTrack;

#include "../RealFFTf.h"

class EffectNoiseRemoval: public Effect {
   
public:
   
   EffectNoiseRemoval();
   virtual ~EffectNoiseRemoval();

   virtual wxString GetEffectName() {
      return wxString(_("Noise Removal..."));
   }
   
   virtual std::set<wxString> GetEffectCategories() {
      std::set<wxString> result;
      result.insert(wxT("http://audacityteam.org/namespace#NoiseRemoval"));
      return result;
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("NoiseRemoval"));
   }

   virtual wxString GetEffectAction() {
      if (mDoProfile)
         return wxString(_("Creating Noise Profile"));
      else
         return wxString(_("Removing Noise"));
   }
   
   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );
   
   virtual bool Init();
   virtual bool CheckWhetherSkipEffect();
   virtual bool Process();
   
private:
   void CleanSpeechMayReadNoisegate();
   void CleanSpeechMayWriteNoiseGate();

   bool      mDoProfile;
   bool      mHasProfile;
   int       mLevel;

   // Parameters chosen before the first phase
   double    mSampleRate;
   int       mWindowSize;
   int       mSpectrumSize;
   float     mMinSignalTime;    // in secs

   // The frequency-indexed noise threshold derived during the first
   // phase of analysis
   float    *mNoiseThreshold;  // length is mSpectrumSize

   // Parameters that affect the noise removal, regardless of how the
   // noise profile was extracted
   double     mFreqSmoothingHz;
   double     mNoiseGain;              // in dB, should be negative
   double     mAttackDecayTime;        // in secs

   bool ProcessOne(int count, WaveTrack * track,
                   sampleCount start, sampleCount len);

   void Initialize();
   void StartNewTrack();
   void ProcessSamples(sampleCount len, float *buffer);
   void FillFirstHistoryWindow();
   void ApplyFreqSmoothing(float *spec);
   void GetProfile();
   void RemoveNoise();
   void RotateHistoryWindows();
   void FinishTrack();
   void Cleanup();

   // Variables that only exist during processing
   WaveTrack            *mOutputTrack;
   sampleCount       mInSampleCount;
   sampleCount       mOutSampleCount;
   int                   mInputPos;

   HFFT     hFFT;
   float    *mFFTBuffer;         // mWindowSize
   float    *mWindow;            // mWindowSize

   int       mFreqSmoothingBins;
   int       mAttackDecayBlocks;
   float     mOneBlockAttackDecay;
   float     mNoiseAttenFactor;
   int       mMinSignalBlocks;
   int       mHistoryLen;
   float    *mInWaveBuffer;     // mWindowSize
   float    *mOutImagBuffer;    // mWindowSize
   float    *mOutOverlapBuffer; // mWindowSize
   float   **mSpectrums;        // mHistoryLen x mSpectrumSize
   float   **mGains;            // mHistoryLen x mSpectrumSize
   float   **mRealFFTs;         // mHistoryLen x mWindowSize
   float   **mImagFFTs;         // mHistoryLen x mWindowSize

friend class NoiseRemovalDialog;
};

// WDR: class declarations

//----------------------------------------------------------------------------
// NoiseRemovalDialog
//----------------------------------------------------------------------------

// Declare window functions

class NoiseRemovalDialog: public EffectDialog
{
public:
   // constructors and destructors
   NoiseRemovalDialog(EffectNoiseRemoval * effect,
                      wxWindow *parent);

   wxSizer *MakeNoiseRemovalDialog(bool call_fit = true,
                                   bool set_sizer = true);

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();
   
private:
   // handlers
   void OnGetProfile( wxCommandEvent &event );
   void OnPreview(wxCommandEvent &event);
   void OnRemoveNoise( wxCommandEvent &event );
   void OnCancel( wxCommandEvent &event );
   
   void OnGainText(wxCommandEvent & event);
   void OnFreqText(wxCommandEvent & event);
   void OnTimeText(wxCommandEvent & event);
   void OnGainSlider(wxCommandEvent & event);
   void OnFreqSlider(wxCommandEvent & event);
   void OnTimeSlider(wxCommandEvent & event);

 public:

   EffectNoiseRemoval * m_pEffect;

   wxButton * m_pButton_GetProfile;
   wxButton * m_pButton_Preview;
   wxButton * m_pButton_RemoveNoise;

   wxSlider   *mGainS;
   wxSlider   *mFreqS;
   wxSlider   *mTimeS;

   wxTextCtrl *mGainT;
   wxTextCtrl *mFreqT;
   wxTextCtrl *mTimeT;

   double      mGain;
   double      mFreq;
   double      mTime;

private:
   DECLARE_EVENT_TABLE()

};

#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: c42ae8d9-7625-4bf9-a719-e5d082430ed5

