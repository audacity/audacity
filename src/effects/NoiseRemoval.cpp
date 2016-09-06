/**********************************************************************

  Audacity: A Digital Audio Editor

  NoiseRemoval.cpp

  Dominic Mazzoni

*******************************************************************//**

\class EffectNoiseRemoval
\brief A two-pass effect to remove background noise.

  The first pass is done over just noise.  For each windowed sample
  of the sound, we take a FFT and then statistics are tabulated for
  each frequency band - specifically the maximum level achieved by
  at least (n) sampling windows in a row, for various values of (n).

  During the noise removal phase, we start by setting a gain control
  for each frequency band such that if the sound has exceeded the
  previously-determined threshold, the gain is set to 0, otherwise
  the gain is set lower (e.g. -18 dB), to suppress the noise.
  Then frequency-smoothing is applied so that a single frequency is
  never suppressed or boosted in isolation, and then time-smoothing
  is applied so that the gain for each frequency band moves slowly.
  Lookahead is employed; this effect is not designed for real-time
  but if it were, there would be a significant delay.

  The gain controls are applied to the complex FFT of the signal,
  and then the inverse FFT is applied, followed by a Hanning window;
  the output signal is then pieced together using overlap/add of
  half the window size.

*//****************************************************************//**

\class NoiseRemovalDialog
\brief Dialog used with EffectNoiseRemoval

*//*******************************************************************/

#include "../Audacity.h"
#include "../Experimental.h"

#if !defined(EXPERIMENTAL_NOISE_REDUCTION)

#include "NoiseRemoval.h"

#include "../Envelope.h"
#include "../WaveTrack.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../FileNames.h"
#include "../ShuttleGui.h"

#include <math.h>

#if defined(__WXMSW__) && !defined(__CYGWIN__)
#include <float.h>
#define finite(x) _finite(x)
#endif

#include <wx/file.h>
#include <wx/ffile.h>
#include <wx/bitmap.h>
#include <wx/brush.h>
#include <wx/button.h>
#include <wx/choice.h>
#include <wx/radiobut.h>
#include <wx/dcmemory.h>
#include <wx/image.h>
#include <wx/intl.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/valtext.h>


#include "../AudacityApp.h"
#include "../PlatformCompatibility.h"

EffectNoiseRemoval::EffectNoiseRemoval()
{
   mWindowSize = 2048;
   mSpectrumSize = 1 + mWindowSize / 2;

   gPrefs->Read(wxT("/Effects/NoiseRemoval/NoiseSensitivity"),
                &mSensitivity, 0.0);
   gPrefs->Read(wxT("/Effects/NoiseRemoval/NoiseGain"),
                &mNoiseGain, -24.0);
   gPrefs->Read(wxT("/Effects/NoiseRemoval/NoiseFreqSmoothing"),
                &mFreqSmoothingHz, 150.0);
   gPrefs->Read(wxT("/Effects/NoiseRemoval/NoiseAttackDecayTime"),
                &mAttackDecayTime, 0.15);
   gPrefs->Read(wxT("/Effects/NoiseRemoval/NoiseLeaveNoise"),
                &mbLeaveNoise, false);
//   mbLeaveNoise = false;


   mMinSignalTime = 0.05f;
   mHasProfile = false;
   mDoProfile = true;

   mNoiseThreshold = new float[mSpectrumSize];

   Init();
}

EffectNoiseRemoval::~EffectNoiseRemoval()
{
   delete [] mNoiseThreshold;
}

// IdentInterface implementation

wxString EffectNoiseRemoval::GetSymbol()
{
   return XO("Noise Removal");
}

wxString EffectNoiseRemoval::GetDescription()
{
   return XO("Removes constant background noise such as fans, tape noise, or hums");
}

// EffectIdentInterface implementation

EffectType EffectNoiseRemoval::GetType()
{
   return EffectTypeProcess;
}

bool EffectNoiseRemoval::SupportsAutomation()
{
   return false;
}

// Effect implementation

#define MAX_NOISE_LEVEL  30
bool EffectNoiseRemoval::Init()
{
   mLevel = gPrefs->Read(wxT("/Effects/NoiseRemoval/Noise_Level"), 3L);
   if ((mLevel < 0) || (mLevel > MAX_NOISE_LEVEL)) {  // corrupted Prefs?
      mLevel = 0;  //Off-skip
      gPrefs->Write(wxT("/Effects/NoiseRemoval/Noise_Level"), mLevel);
   }
   return gPrefs->Flush();
}

bool EffectNoiseRemoval::CheckWhetherSkipEffect()
{
   return (mLevel == 0);
}

bool EffectNoiseRemoval::PromptUser(wxWindow *parent)
{
   NoiseRemovalDialog dlog(this, parent);
   dlog.mSensitivity = mSensitivity;
   dlog.mGain = -mNoiseGain;
   dlog.mFreq = mFreqSmoothingHz;
   dlog.mTime = mAttackDecayTime;
   dlog.mbLeaveNoise = mbLeaveNoise;
   dlog.mKeepSignal->SetValue(!mbLeaveNoise);
   dlog.mKeepNoise->SetValue(mbLeaveNoise);

   // We may want to twiddle the levels if we are setting
   // from an automation dialog, the only case in which we can
   // get here without any wavetracks.
   bool bAllowTwiddleSettings = (GetNumWaveTracks() == 0);

   if (mHasProfile || bAllowTwiddleSettings) {
      dlog.m_pButton_Preview->Enable(GetNumWaveTracks() != 0);
      dlog.m_pButton_RemoveNoise->SetDefault();
   } else {
      dlog.m_pButton_Preview->Enable(false);
      dlog.m_pButton_RemoveNoise->Enable(false);
   }

   dlog.TransferDataToWindow();
   dlog.mKeepNoise->SetValue(dlog.mbLeaveNoise);
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (dlog.GetReturnCode() == 0) {
      return false;
   }

   mSensitivity = dlog.mSensitivity;
   mNoiseGain = -dlog.mGain;
   mFreqSmoothingHz = dlog.mFreq;
   mAttackDecayTime = dlog.mTime;
   mbLeaveNoise = dlog.mbLeaveNoise;

   gPrefs->Write(wxT("/Effects/NoiseRemoval/NoiseSensitivity"), mSensitivity);
   gPrefs->Write(wxT("/Effects/NoiseRemoval/NoiseGain"), mNoiseGain);
   gPrefs->Write(wxT("/Effects/NoiseRemoval/NoiseFreqSmoothing"), mFreqSmoothingHz);
   gPrefs->Write(wxT("/Effects/NoiseRemoval/NoiseAttackDecayTime"), mAttackDecayTime);
   gPrefs->Write(wxT("/Effects/NoiseRemoval/NoiseLeaveNoise"), mbLeaveNoise);

   mDoProfile = (dlog.GetReturnCode() == 1);
   return gPrefs->Flush();
}

bool EffectNoiseRemoval::Process()
{
   Initialize();

   // This same code will both remove noise and profile it,
   // depending on 'mDoProfile'
   this->CopyInputTracks(); // Set up mOutputTracks.
   bool bGoodResult = true;

   SelectedTrackListOfKindIterator iter(Track::Wave, mOutputTracks.get());
   WaveTrack *track = (WaveTrack *) iter.First();
   int count = 0;
   while (track) {
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();
      double t0 = mT0 < trackStart? trackStart: mT0;
      double t1 = mT1 > trackEnd? trackEnd: mT1;

      if (t1 > t0) {
         auto start = track->TimeToLongSamples(t0);
         auto end = track->TimeToLongSamples(t1);
         auto len = end - start;

         if (!ProcessOne(count, track, start, len)) {
            Cleanup();
            bGoodResult = false;
            break;
         }
      }
      track = (WaveTrack *) iter.Next();
      count++;
   }

   if (bGoodResult && mDoProfile) {
      mHasProfile = true;
      mDoProfile = false;
   }

   if (bGoodResult)
      Cleanup();
   this->ReplaceProcessedTracks(bGoodResult);
   return bGoodResult;
}

void EffectNoiseRemoval::ApplyFreqSmoothing(float *spec)
{
   float *tmp = new float[mSpectrumSize];
   int i, j, j0, j1;

   for(i = 0; i < mSpectrumSize; i++) {
      j0 = wxMax(0, i - mFreqSmoothingBins);
      j1 = wxMin(mSpectrumSize-1, i + mFreqSmoothingBins);
      tmp[i] = 0.0;
      for(j = j0; j <= j1; j++) {
         tmp[i] += spec[j];
      }
      tmp[i] /= (j1 - j0 + 1);
   }

   for(i = 0; i < mSpectrumSize; i++)
      spec[i] = tmp[i];

   delete[] tmp;
}

void EffectNoiseRemoval::Initialize()
{
   int i;

   mSampleRate = mProjectRate;
   mFreqSmoothingBins = (int)(mFreqSmoothingHz * mWindowSize / mSampleRate);
   mAttackDecayBlocks = 1 +
      (int)(mAttackDecayTime * mSampleRate / (mWindowSize / 2));
   mNoiseAttenFactor = DB_TO_LINEAR(mNoiseGain);
   mOneBlockAttackDecay = DB_TO_LINEAR(mNoiseGain / mAttackDecayBlocks);
   // Applies to power, divide by 10:
   mSensitivityFactor = pow(10.0, mSensitivity/10.0);
   mMinSignalBlocks =
      (int)(mMinSignalTime * mSampleRate / (mWindowSize / 2));
   if( mMinSignalBlocks < 1 )
      mMinSignalBlocks = 1;
   mHistoryLen = (2 * mAttackDecayBlocks) - 1;

   if (mHistoryLen < mMinSignalBlocks)
      mHistoryLen = mMinSignalBlocks;

   mSpectrums = new float*[mHistoryLen];
   mGains = new float*[mHistoryLen];
   mRealFFTs = new float*[mHistoryLen];
   mImagFFTs = new float*[mHistoryLen];
   for(i = 0; i < mHistoryLen; i++) {
      mSpectrums[i] = new float[mSpectrumSize];
      mGains[i] = new float[mSpectrumSize];
      mRealFFTs[i] = new float[mSpectrumSize];
      mImagFFTs[i] = new float[mSpectrumSize];
   }

   // Initialize the FFT
   hFFT = InitializeFFT(mWindowSize);

   mFFTBuffer = new float[mWindowSize];
   mInWaveBuffer = new float[mWindowSize];
   mWindow = new float[mWindowSize];
   mOutOverlapBuffer = new float[mWindowSize];

   // Create a Hanning window function
   for(i=0; i<mWindowSize; i++)
      mWindow[i] = 0.5 - 0.5 * cos((2.0*M_PI*i) / mWindowSize);

   if (mDoProfile) {
      for (i = 0; i < mSpectrumSize; i++)
         mNoiseThreshold[i] = float(0);
   }
}

void EffectNoiseRemoval::Cleanup()
{
   int i;

   EndFFT(hFFT);

   if (mDoProfile) {
      ApplyFreqSmoothing(mNoiseThreshold);
   }

   for(i = 0; i < mHistoryLen; i++) {
      delete[] mSpectrums[i];
      delete[] mGains[i];
      delete[] mRealFFTs[i];
      delete[] mImagFFTs[i];
   }
   delete[] mSpectrums;
   delete[] mGains;
   delete[] mRealFFTs;
   delete[] mImagFFTs;

   delete[] mFFTBuffer;
   delete[] mInWaveBuffer;
   delete[] mWindow;
   delete[] mOutOverlapBuffer;
}

void EffectNoiseRemoval::StartNewTrack()
{
   int i, j;

   for(i = 0; i < mHistoryLen; i++) {
      for(j = 0; j < mSpectrumSize; j++) {
         mSpectrums[i][j] = 0;
         mGains[i][j] = mNoiseAttenFactor;
         mRealFFTs[i][j] = 0.0;
         mImagFFTs[i][j] = 0.0;
      }
   }

   for(j = 0; j < mWindowSize; j++)
      mOutOverlapBuffer[j] = 0.0;

   mInputPos = 0;
   mInSampleCount = 0;
   mOutSampleCount = -(int)((mWindowSize / 2) * (mHistoryLen - 1));
}

void EffectNoiseRemoval::ProcessSamples(size_t len, float *buffer)
{
   int i;

   while(len && mOutSampleCount < mInSampleCount) {
      int avail = wxMin(len, mWindowSize - mInputPos);
      for(i = 0; i < avail; i++)
         mInWaveBuffer[mInputPos + i] = buffer[i];
      buffer += avail;
      len -= avail;
      mInputPos += avail;

      if (mInputPos == mWindowSize) {
         FillFirstHistoryWindow();
         if (mDoProfile)
            GetProfile();
         else
            RemoveNoise();
         RotateHistoryWindows();

         // Rotate halfway for overlap-add
         for(i = 0; i < mWindowSize / 2; i++) {
            mInWaveBuffer[i] = mInWaveBuffer[i + mWindowSize / 2];
         }
         mInputPos = mWindowSize / 2;
      }
   }
}

void EffectNoiseRemoval::FillFirstHistoryWindow()
{
   int i;

   for(i=0; i < mWindowSize; i++)
      mFFTBuffer[i] = mInWaveBuffer[i];
   RealFFTf(mFFTBuffer, hFFT);
   for(i = 1; i < (mSpectrumSize-1); i++) {
      mRealFFTs[0][i] = mFFTBuffer[hFFT->BitReversed[i]  ];
      mImagFFTs[0][i] = mFFTBuffer[hFFT->BitReversed[i]+1];
      mSpectrums[0][i] = mRealFFTs[0][i]*mRealFFTs[0][i] + mImagFFTs[0][i]*mImagFFTs[0][i];
      mGains[0][i] = mNoiseAttenFactor;
   }
   // DC and Fs/2 bins need to be handled specially
   mSpectrums[0][0] = mFFTBuffer[0]*mFFTBuffer[0];
   mSpectrums[0][mSpectrumSize-1] = mFFTBuffer[1]*mFFTBuffer[1];
   mGains[0][0] = mNoiseAttenFactor;
   mGains[0][mSpectrumSize-1] = mNoiseAttenFactor;
}

void EffectNoiseRemoval::RotateHistoryWindows()
{
   int last = mHistoryLen - 1;
   int i;

   // Remember the last window so we can reuse it
   float *lastSpectrum = mSpectrums[last];
   float *lastGain = mGains[last];
   float *lastRealFFT = mRealFFTs[last];
   float *lastImagFFT = mImagFFTs[last];

   // Rotate each window forward
   for(i = last; i >= 1; i--) {
      mSpectrums[i] = mSpectrums[i-1];
      mGains[i] = mGains[i-1];
      mRealFFTs[i] = mRealFFTs[i-1];
      mImagFFTs[i] = mImagFFTs[i-1];
   }

   // Reuse the last buffers as the NEW first window
   mSpectrums[0] = lastSpectrum;
   mGains[0] = lastGain;
   mRealFFTs[0] = lastRealFFT;
   mImagFFTs[0] = lastImagFFT;
}

void EffectNoiseRemoval::FinishTrack()
{
   // Keep flushing empty input buffers through the history
   // windows until we've output exactly as many samples as
   // were input.
   // Well, not exactly, but not more than mWindowSize/2 extra samples at the end.
   // We'll DELETE them later in ProcessOne.

   float *empty = new float[mWindowSize / 2];
   int i;
   for(i = 0; i < mWindowSize / 2; i++)
      empty[i] = 0.0;

   while (mOutSampleCount < mInSampleCount) {
      ProcessSamples(mWindowSize / 2, empty);
   }

   delete [] empty;
}

void EffectNoiseRemoval::GetProfile()
{
   // The noise threshold for each frequency is the maximum
   // level achieved at that frequency for a minimum of
   // mMinSignalBlocks blocks in a row - the max of a min.

   int start = mHistoryLen - mMinSignalBlocks;
   int finish = mHistoryLen;
   int i, j;

   for (j = 0; j < mSpectrumSize; j++) {
      float min = mSpectrums[start][j];
      for (i = start+1; i < finish; i++) {
         if (mSpectrums[i][j] < min)
            min = mSpectrums[i][j];
      }
      if (min > mNoiseThreshold[j])
         mNoiseThreshold[j] = min;
   }

   mOutSampleCount += mWindowSize / 2; // what is this for?  Not used when we are getting the profile?
}

void EffectNoiseRemoval::RemoveNoise()
{
   int center = mHistoryLen / 2;
   int start = center - mMinSignalBlocks/2;
   int finish = start + mMinSignalBlocks;
   int i, j;

   // Raise the gain for elements in the center of the sliding history
   for (j = 0; j < mSpectrumSize; j++) {
      float min = mSpectrums[start][j];
      for (i = start+1; i < finish; i++) {
         if (mSpectrums[i][j] < min)
            min = mSpectrums[i][j];
      }
      if (min > mSensitivityFactor * mNoiseThreshold[j] && mGains[center][j] < 1.0) {
         if (mbLeaveNoise) mGains[center][j] = 0.0;
         else mGains[center][j] = 1.0;
      } else {
         if (mbLeaveNoise) mGains[center][j] = 1.0;
      }
   }

   // Decay the gain in both directions;
   // note that mOneBlockAttackDecay is less than 1.0
   // of linear attenuation per block
   for (j = 0; j < mSpectrumSize; j++) {
      for (i = center + 1; i < mHistoryLen; i++) {
         if (mGains[i][j] < mGains[i - 1][j] * mOneBlockAttackDecay)
            mGains[i][j] = mGains[i - 1][j] * mOneBlockAttackDecay;
         if (mGains[i][j] < mNoiseAttenFactor)
            mGains[i][j] = mNoiseAttenFactor;
      }
      for (i = center - 1; i >= 0; i--) {
         if (mGains[i][j] < mGains[i + 1][j] * mOneBlockAttackDecay)
            mGains[i][j] = mGains[i + 1][j] * mOneBlockAttackDecay;
         if (mGains[i][j] < mNoiseAttenFactor)
            mGains[i][j] = mNoiseAttenFactor;
      }
   }


   // Apply frequency smoothing to output gain
   int out = mHistoryLen - 1;  // end of the queue

   ApplyFreqSmoothing(mGains[out]);

   // Apply gain to FFT
   for (j = 0; j < (mSpectrumSize-1); j++) {
      mFFTBuffer[j*2  ] = mRealFFTs[out][j] * mGains[out][j];
      mFFTBuffer[j*2+1] = mImagFFTs[out][j] * mGains[out][j];
   }
   // The Fs/2 component is stored as the imaginary part of the DC component
   mFFTBuffer[1] = mRealFFTs[out][mSpectrumSize-1] * mGains[out][mSpectrumSize-1];

   // Invert the FFT into the output buffer
   InverseRealFFTf(mFFTBuffer, hFFT);

   // Overlap-add
   for(j = 0; j < (mSpectrumSize-1); j++) {
      mOutOverlapBuffer[j*2  ] += mFFTBuffer[hFFT->BitReversed[j]  ] * mWindow[j*2  ];
      mOutOverlapBuffer[j*2+1] += mFFTBuffer[hFFT->BitReversed[j]+1] * mWindow[j*2+1];
   }

   // Output the first half of the overlap buffer, they're done -
   // and then shift the next half over.
   if (mOutSampleCount >= 0) {   // ...but not if it's the first half-window
      mOutputTrack->Append((samplePtr)mOutOverlapBuffer, floatSample,
                           mWindowSize / 2);
   }
   mOutSampleCount += mWindowSize / 2;
   for(j = 0; j < mWindowSize / 2; j++) {
      mOutOverlapBuffer[j] = mOutOverlapBuffer[j + (mWindowSize / 2)];
      mOutOverlapBuffer[j + (mWindowSize / 2)] = 0.0;
   }
}

bool EffectNoiseRemoval::ProcessOne(int count, WaveTrack * track,
                                    sampleCount start, sampleCount len)
{
   if (track == NULL)
      return false;

   StartNewTrack();

   if (!mDoProfile)
      mOutputTrack = mFactory->NewWaveTrack(track->GetSampleFormat(),
                                            track->GetRate());

   auto bufferSize = track->GetMaxBlockSize();
   float *buffer = new float[bufferSize];

   bool bLoopSuccess = true;
   auto samplePos = start;
   while (samplePos < start + len) {
      //Get a blockSize of samples (smaller than the size of the buffer)
      //Adjust the block size if it is the final block in the track
      const auto blockSize = limitSampleBufferSize(
         track->GetBestBlockSize(samplePos),
         start + len - samplePos
      );

      //Get the samples from the track and put them in the buffer
      track->Get((samplePtr)buffer, floatSample, samplePos, blockSize);

      mInSampleCount += blockSize;
      ProcessSamples(blockSize, buffer);

      samplePos += blockSize;

      // Update the Progress meter
      if (TrackProgress(count, (samplePos - start) / (double)len)) {
         bLoopSuccess = false;
         break;
      }
   }

   FinishTrack();
   delete [] buffer;

   if (!mDoProfile) {
      // Flush the output WaveTrack (since it's buffered)
      mOutputTrack->Flush();

      // Take the output track and insert it in place of the original
      // sample data (as operated on -- this may not match mT0/mT1)
      if (bLoopSuccess) {
         double t0 = mOutputTrack->LongSamplesToTime(start);
         double tLen = mOutputTrack->LongSamplesToTime(len);
         // Filtering effects always end up with more data than they started with.  Delete this 'tail'.
         mOutputTrack->HandleClear(tLen, mOutputTrack->GetEndTime(), false, false);
         bool bResult = track->ClearAndPaste(t0, t0 + tLen, mOutputTrack.get(), true, false);
         wxASSERT(bResult); // TO DO: Actually handle this.
      }

      // Delete the outputTrack now that its data is inserted in place
      mOutputTrack.reset();
   }

   return bLoopSuccess;
}

// WDR: class implementations

//----------------------------------------------------------------------------
// NoiseRemovalDialog
//----------------------------------------------------------------------------

// WDR: event table for NoiseRemovalDialog

enum {
   ID_BUTTON_GETPROFILE = 10001,
   ID_BUTTON_LEAVENOISE,
   ID_RADIOBUTTON_KEEPSIGNAL,
   ID_RADIOBUTTON_KEEPNOISE,
   ID_SENSITIVITY_SLIDER,
   ID_GAIN_SLIDER,
   ID_FREQ_SLIDER,
   ID_TIME_SLIDER,
   ID_SENSITIVITY_TEXT,
   ID_GAIN_TEXT,
   ID_FREQ_TEXT,
   ID_TIME_TEXT,
};

#define SENSITIVITY_MIN 0      // Corresponds to -20 dB
#define SENSITIVITY_MAX 4000    // Corresponds to 20 dB

#define GAIN_MIN 0
#define GAIN_MAX 48     // Corresponds to -48 dB

#define FREQ_MIN 0
#define FREQ_MAX 100    // Corresponds to 1000 Hz

#define TIME_MIN 0
#define TIME_MAX 100    // Corresponds to 1.000 seconds


BEGIN_EVENT_TABLE(NoiseRemovalDialog,wxDialogWrapper)
   EVT_BUTTON(wxID_OK, NoiseRemovalDialog::OnRemoveNoise)
   EVT_BUTTON(wxID_CANCEL, NoiseRemovalDialog::OnCancel)
   EVT_BUTTON(ID_EFFECT_PREVIEW, NoiseRemovalDialog::OnPreview)
   EVT_BUTTON(ID_BUTTON_GETPROFILE, NoiseRemovalDialog::OnGetProfile)
   EVT_RADIOBUTTON(ID_RADIOBUTTON_KEEPNOISE, NoiseRemovalDialog::OnKeepNoise)
   EVT_RADIOBUTTON(ID_RADIOBUTTON_KEEPSIGNAL, NoiseRemovalDialog::OnKeepNoise)
   EVT_SLIDER(ID_SENSITIVITY_SLIDER, NoiseRemovalDialog::OnSensitivitySlider)
   EVT_SLIDER(ID_GAIN_SLIDER, NoiseRemovalDialog::OnGainSlider)
   EVT_SLIDER(ID_FREQ_SLIDER, NoiseRemovalDialog::OnFreqSlider)
   EVT_SLIDER(ID_TIME_SLIDER, NoiseRemovalDialog::OnTimeSlider)
   EVT_TEXT(ID_SENSITIVITY_TEXT, NoiseRemovalDialog::OnSensitivityText)
   EVT_TEXT(ID_GAIN_TEXT, NoiseRemovalDialog::OnGainText)
   EVT_TEXT(ID_FREQ_TEXT, NoiseRemovalDialog::OnFreqText)
   EVT_TEXT(ID_TIME_TEXT, NoiseRemovalDialog::OnTimeText)
END_EVENT_TABLE()

NoiseRemovalDialog::NoiseRemovalDialog(EffectNoiseRemoval * effect,
                                       wxWindow *parent)
   : EffectDialog( parent, _("Noise Removal"), EffectTypeProcess)
{
   m_pEffect = effect;

   // NULL out the control members until the controls are created.
   m_pButton_GetProfile = NULL;
   m_pButton_Preview = NULL;
   m_pButton_RemoveNoise = NULL;

   Init();

   m_pButton_Preview =
      (wxButton *)wxWindow::FindWindowById(ID_EFFECT_PREVIEW, this);
   m_pButton_RemoveNoise =
      (wxButton *)wxWindow::FindWindowById(wxID_OK, this);
}

void NoiseRemovalDialog::OnGetProfile( wxCommandEvent & WXUNUSED(event))
{
   EndModal(1);
}

void NoiseRemovalDialog::OnKeepNoise( wxCommandEvent & WXUNUSED(event))
{
   mbLeaveNoise = mKeepNoise->GetValue();
}

void NoiseRemovalDialog::OnPreview(wxCommandEvent & WXUNUSED(event))
{
   // Save & restore parameters around Preview, because we didn't do OK.
   bool oldDoProfile = m_pEffect->mDoProfile;
   bool oldLeaveNoise = m_pEffect->mbLeaveNoise;
   double oldSensitivity = m_pEffect->mSensitivity;
   double oldGain = m_pEffect->mNoiseGain;
   double oldFreq = m_pEffect->mFreqSmoothingHz;
   double oldTime = m_pEffect->mAttackDecayTime;

   TransferDataFromWindow();

   m_pEffect->mDoProfile = false;
   m_pEffect->mbLeaveNoise = mbLeaveNoise;
   m_pEffect->mSensitivity = mSensitivity;
   m_pEffect->mNoiseGain = -mGain;
   m_pEffect->mFreqSmoothingHz =  mFreq;
   m_pEffect->mAttackDecayTime =  mTime;

   m_pEffect->Preview();

   m_pEffect->mSensitivity = oldSensitivity;
   m_pEffect->mNoiseGain = oldGain;
   m_pEffect->mFreqSmoothingHz =  oldFreq;
   m_pEffect->mAttackDecayTime =  oldTime;
   m_pEffect->mbLeaveNoise = oldLeaveNoise;
   m_pEffect->mDoProfile = oldDoProfile;
}

void NoiseRemovalDialog::OnRemoveNoise( wxCommandEvent & WXUNUSED(event))
{
   mbLeaveNoise = mKeepNoise->GetValue();
   EndModal(2);
}

void NoiseRemovalDialog::OnCancel(wxCommandEvent & WXUNUSED(event))
{
   EndModal(0);
}

void NoiseRemovalDialog::PopulateOrExchange(ShuttleGui & S)
{
   S.StartStatic(_("Step 1"));
   {
      S.AddVariableText(_("Select a few seconds of just noise so Audacity knows what to filter out,\nthen click Get Noise Profile:"));
      m_pButton_GetProfile = S.Id(ID_BUTTON_GETPROFILE).AddButton(_("&Get Noise Profile"));
   }
   S.EndStatic();

   S.StartStatic(_("Step 2"));
   {
      S.AddVariableText(_("Select all of the audio you want filtered, choose how much noise you want\nfiltered out, and then click 'OK' to remove noise.\n"));

      S.StartMultiColumn(3, wxEXPAND);
      S.SetStretchyCol(2);
      {
         wxTextValidator vld(wxFILTER_NUMERIC);
         mGainT = S.Id(ID_GAIN_TEXT).AddTextBox(_("Noise re&duction (dB):"), wxT(""), 0);
         S.SetStyle(wxSL_HORIZONTAL);
         mGainT->SetValidator(vld);
         mGainS = S.Id(ID_GAIN_SLIDER).AddSlider(wxT(""), 0, GAIN_MAX);
         mGainS->SetName(_("Noise reduction"));
         mGainS->SetRange(GAIN_MIN, GAIN_MAX);
         mGainS->SetSizeHints(150, -1);

         mSensitivityT = S.Id(ID_SENSITIVITY_TEXT).AddTextBox(_("&Sensitivity (dB):"),
                                                wxT(""),
                                                0);
         S.SetStyle(wxSL_HORIZONTAL);
         mSensitivityT->SetValidator(vld);
         mSensitivityS = S.Id(ID_SENSITIVITY_SLIDER).AddSlider(wxT(""), 0, SENSITIVITY_MAX);
         mSensitivityS->SetName(_("Sensitivity"));
         mSensitivityS->SetRange(SENSITIVITY_MIN, SENSITIVITY_MAX);
         mSensitivityS->SetSizeHints(150, -1);

         mFreqT = S.Id(ID_FREQ_TEXT).AddTextBox(_("Fr&equency smoothing (Hz):"),
                                                wxT(""),
                                                0);
         S.SetStyle(wxSL_HORIZONTAL);
         mFreqT->SetValidator(vld);
         mFreqS = S.Id(ID_FREQ_SLIDER).AddSlider(wxT(""), 0, FREQ_MAX);
         mFreqS->SetName(_("Frequency smoothing"));
         mFreqS->SetRange(FREQ_MIN, FREQ_MAX);
         mFreqS->SetSizeHints(150, -1);

         mTimeT = S.Id(ID_TIME_TEXT).AddTextBox(_("Attac&k/decay time (secs):"),
                                                wxT(""),
                                                0);
         S.SetStyle(wxSL_HORIZONTAL);
         mTimeT->SetValidator(vld);
         mTimeS = S.Id(ID_TIME_SLIDER).AddSlider(wxT(""), 0, TIME_MAX);
         mTimeS->SetName(_("Attack/decay time"));
         mTimeS->SetRange(TIME_MIN, TIME_MAX);
         mTimeS->SetSizeHints(150, -1);

         S.AddPrompt(_("Noise:"));
         mKeepSignal = S.Id(ID_RADIOBUTTON_KEEPSIGNAL)
               .AddRadioButton(_("Re&move"));
         mKeepNoise = S.Id(ID_RADIOBUTTON_KEEPNOISE)
               .AddRadioButtonToGroup(_("&Isolate"));
      }
      S.EndMultiColumn();
   }
   S.EndStatic();
}

bool NoiseRemovalDialog::TransferDataToWindow()
{
   mSensitivityT->SetValue(wxString::Format(wxT("%.2f"), mSensitivity));
   mGainT->SetValue(wxString::Format(wxT("%d"), (int)mGain));
   mFreqT->SetValue(wxString::Format(wxT("%d"), (int)mFreq));
   mTimeT->SetValue(wxString::Format(wxT("%.2f"), mTime));
   mKeepNoise->SetValue(mbLeaveNoise);
   mKeepSignal->SetValue(!mbLeaveNoise);

   mSensitivityS->SetValue(TrapLong(mSensitivity*100.0 + (SENSITIVITY_MAX-SENSITIVITY_MIN+1)/2.0, SENSITIVITY_MIN, SENSITIVITY_MAX));
   mGainS->SetValue(TrapLong(mGain, GAIN_MIN, GAIN_MAX));
   mFreqS->SetValue(TrapLong(mFreq / 10, FREQ_MIN, FREQ_MAX));
   mTimeS->SetValue(TrapLong(mTime * TIME_MAX + 0.5, TIME_MIN, TIME_MAX));

   return true;
}

bool NoiseRemovalDialog::TransferDataFromWindow()
{
   // Nothing to do here
   return true;
}

void NoiseRemovalDialog::OnSensitivityText(wxCommandEvent & WXUNUSED(event))
{
   mSensitivityT->GetValue().ToDouble(&mSensitivity);
   mSensitivityS->SetValue(TrapLong(mSensitivity*100.0 + (SENSITIVITY_MAX-SENSITIVITY_MIN+1)/2.0, SENSITIVITY_MIN, SENSITIVITY_MAX));
}

void NoiseRemovalDialog::OnGainText(wxCommandEvent & WXUNUSED(event))
{
   mGainT->GetValue().ToDouble(&mGain);
   mGainS->SetValue(TrapLong(mGain, GAIN_MIN, GAIN_MAX));
}

void NoiseRemovalDialog::OnFreqText(wxCommandEvent & WXUNUSED(event))
{
   mFreqT->GetValue().ToDouble(&mFreq);
   mFreqS->SetValue(TrapLong(mFreq / 10, FREQ_MIN, FREQ_MAX));
}

void NoiseRemovalDialog::OnTimeText(wxCommandEvent & WXUNUSED(event))
{
   mTimeT->GetValue().ToDouble(&mTime);
   mTimeS->SetValue(TrapLong(mTime * TIME_MAX + 0.5, TIME_MIN, TIME_MAX));
}

void NoiseRemovalDialog::OnSensitivitySlider(wxCommandEvent & WXUNUSED(event))
{
   mSensitivity = mSensitivityS->GetValue()/100.0 - 20.0;
   mSensitivityT->SetValue(wxString::Format(wxT("%.2f"), mSensitivity));
}

void NoiseRemovalDialog::OnGainSlider(wxCommandEvent & WXUNUSED(event))
{
   mGain = mGainS->GetValue();
   mGainT->SetValue(wxString::Format(wxT("%d"), (int)mGain));
}

void NoiseRemovalDialog::OnFreqSlider(wxCommandEvent & WXUNUSED(event))
{
   mFreq = mFreqS->GetValue() * 10;
   mFreqT->SetValue(wxString::Format(wxT("%d"), (int)mFreq));
}

void NoiseRemovalDialog::OnTimeSlider(wxCommandEvent & WXUNUSED(event))
{
   mTime = mTimeS->GetValue() / (TIME_MAX*1.0);
   mTimeT->SetValue(wxString::Format(wxT("%.2f"), mTime));
}

#endif