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
  and then the inverse FFT is applied, followed by a Hann window;
  the output signal is then pieced together using overlap/add of
  half the window size.

*//****************************************************************//**

\class NoiseRemovalDialog
\brief Dialog used with EffectNoiseRemoval

*//*******************************************************************/


#include "NoiseRemoval.h"

#if !defined(EXPERIMENTAL_NOISE_REDUCTION)

#include "LoadEffects.h"

#include "WaveTrack.h"
#include "Prefs.h"
#include "FileNames.h"
#include "ShuttleGui.h"

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
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/valtext.h>


#include "PlatformCompatibility.h"

const ComponentInterfaceSymbol EffectNoiseRemoval::Symbol
{ XO("Noise Removal") };

namespace{ BuiltinEffectsModule::Registration< EffectNoiseRemoval > reg; }

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

   mNoiseThreshold.reinit(mSpectrumSize);

   Init();
}

EffectNoiseRemoval::~EffectNoiseRemoval()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectNoiseRemoval::GetSymbol()
{
   return Symbol;
}

TranslatableString EffectNoiseRemoval::GetDescription() const
{
   return XO("Removes constant background noise such as fans, tape noise, or hums");
}

// EffectDefinitionInterface implementation

EffectType EffectNoiseRemoval::GetType() const
{
   return EffectTypeProcess;
}

bool EffectNoiseRemoval::SupportsAutomation() const
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

bool EffectNoiseRemoval::CheckWhetherSkipEffect(const EffectSettings &) const
{
   return (mLevel == 0);
}

//! An override still here for historical reasons, ignoring the factory
//! and the access
/*! We would like to make this effect behave more like others, but it does have
 its unusual two-pass nature.  First choose and analyze an example of noise,
 then apply noise reduction to another selection.  That is difficult to fit into
 the framework for managing settings of other effects. */
int EffectNoiseRemoval::ShowHostInterface(
   wxWindow &parent, const EffectDialogFactory &,
   std::shared_ptr<EffectInstance> &pInstance, EffectSettingsAccess &access,
   bool forceModal )
{
   // Assign the out parameter
   pInstance = MakeInstance();
   if (pInstance && !pInstance->Init())
      pInstance.reset();

   // to do: use forceModal correctly
   NoiseRemovalDialog dlog(this, access, &parent);
   dlog.mSensitivity = mSensitivity;
   dlog.mGain = -mNoiseGain;
   dlog.mFreq = mFreqSmoothingHz;
   dlog.mTime = mAttackDecayTime;
   dlog.mbLeaveNoise = mbLeaveNoise;
   dlog.mKeepSignal->SetValue(!mbLeaveNoise);
   dlog.mKeepNoise->SetValue(mbLeaveNoise);

   // We may want to twiddle the levels if we are setting
   // from a macro editing dialog
   bool bAllowTwiddleSettings = forceModal;

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

   const auto returnCode = dlog.GetReturnCode();
   if (!returnCode)
      return 0;

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
   if (!gPrefs->Flush())
      return 0;
   return returnCode;
}

bool EffectNoiseRemoval::Process(EffectInstance &, EffectSettings &)
{
   Initialize();

   // This same code will both remove noise and profile it,
   // depending on 'mDoProfile'
   this->CopyInputTracks(); // Set up mOutputTracks.
   bool bGoodResult = true;

   int count = 0;
   for( auto track : mOutputTracks->Selected< WaveTrack >() ) {
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();
      double t0 = mT0 < trackStart? trackStart: mT0;
      double t1 = mT1 > trackEnd? trackEnd: mT1;

      if (t1 > t0) {
         auto start = track->TimeToLongSamples(t0);
         auto end = track->TimeToLongSamples(t1);
         auto len = end - start;

         if (!ProcessOne(count, track, start, len)) {
            bGoodResult = false;
            break;
         }
      }
      count++;
   }

   if (bGoodResult && mDoProfile) {
      mHasProfile = true;
      mDoProfile = false;
   }

   this->ReplaceProcessedTracks(bGoodResult);
   return bGoodResult;
}

void EffectNoiseRemoval::ApplyFreqSmoothing(float *spec)
{
   Floats tmp{ mSpectrumSize };
   int j, j0, j1;

   for(int i = 0; i < mSpectrumSize; i++) {
      j0 = wxMax(0, i - mFreqSmoothingBins);
      j1 = wxMin(mSpectrumSize-1, i + mFreqSmoothingBins);
      tmp[i] = 0.0;
      for(j = j0; j <= j1; j++) {
         tmp[i] += spec[j];
      }
      tmp[i] /= (j1 - j0 + 1);
   }

   for(size_t i = 0; i < mSpectrumSize; i++)
      spec[i] = tmp[i];
}

void EffectNoiseRemoval::Initialize()
{
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

   mSpectrums.reinit(mHistoryLen, mSpectrumSize);
   mGains.reinit(mHistoryLen, mSpectrumSize);
   mRealFFTs.reinit(mHistoryLen, mSpectrumSize);
   mImagFFTs.reinit(mHistoryLen, mSpectrumSize);

   // Initialize the FFT
   hFFT = GetFFT(mWindowSize);

   mFFTBuffer.reinit(mWindowSize);
   mInWaveBuffer.reinit(mWindowSize);
   mWindow.reinit(mWindowSize);
   mOutOverlapBuffer.reinit(mWindowSize);

   // Create a Hann window function
   for(size_t i=0; i<mWindowSize; i++)
      mWindow[i] = 0.5 - 0.5 * cos((2.0*M_PI*i) / mWindowSize);

   if (mDoProfile) {
      for (size_t i = 0; i < mSpectrumSize; i++)
         mNoiseThreshold[i] = float(0);
   }
}

void EffectNoiseRemoval::End()
{
   hFFT.reset();

   if (mDoProfile) {
      ApplyFreqSmoothing(mNoiseThreshold.get());
   }

   mSpectrums.reset();
   mGains.reset();
   mRealFFTs.reset();
   mImagFFTs.reset();

   mFFTBuffer.reset();
   mInWaveBuffer.reset();
   mWindow.reset();
   mOutOverlapBuffer.reset();

   mOutputTrack.reset();
}

void EffectNoiseRemoval::StartNewTrack()
{
   for(size_t i = 0; i < mHistoryLen; i++) {
      for(size_t j = 0; j < mSpectrumSize; j++) {
         mSpectrums[i][j] = 0;
         mGains[i][j] = mNoiseAttenFactor;
         mRealFFTs[i][j] = 0.0;
         mImagFFTs[i][j] = 0.0;
      }
   }

   for(size_t j = 0; j < mWindowSize; j++)
      mOutOverlapBuffer[j] = 0.0;

   mInputPos = 0;
   mInSampleCount = 0;
   mOutSampleCount = -(int)((mWindowSize / 2) * (mHistoryLen - 1));
}

void EffectNoiseRemoval::ProcessSamples(size_t len, float *buffer)
{
   while(len && mOutSampleCount < mInSampleCount) {
      size_t avail = wxMin(len, mWindowSize - mInputPos);
      for(size_t i = 0; i < avail; i++)
         mInWaveBuffer[mInputPos + i] = buffer[i];
      buffer += avail;
      len -= avail;
      mInputPos += avail;

      if (mInputPos == int(mWindowSize)) {
         FillFirstHistoryWindow();
         if (mDoProfile)
            GetProfile();
         else
            RemoveNoise();
         RotateHistoryWindows();

         // Rotate halfway for overlap-add
         for(size_t i = 0; i < mWindowSize / 2; i++) {
            mInWaveBuffer[i] = mInWaveBuffer[i + mWindowSize / 2];
         }
         mInputPos = mWindowSize / 2;
      }
   }
}

void EffectNoiseRemoval::FillFirstHistoryWindow()
{
   for(size_t i = 0; i < mWindowSize; i++)
      mFFTBuffer[i] = mInWaveBuffer[i];
   RealFFTf(mFFTBuffer.get(), hFFT.get());
   for(size_t i = 1; i + 1 < mSpectrumSize; i++) {
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

namespace {
   inline void Rotate(ArraysOf<float> &arrays, size_t historyLen)
   {
      Floats temp = std::move( arrays[ historyLen - 1 ] );

      for ( size_t nn = historyLen - 1; nn--; )
         arrays[ nn + 1 ] = std::move( arrays[ nn ] );
      arrays[0] = std::move( temp );
   }
}

void EffectNoiseRemoval::RotateHistoryWindows()
{
   // Remember the last window so we can reuse it
   Rotate(mSpectrums, mHistoryLen);
   Rotate(mGains, mHistoryLen);
   Rotate(mRealFFTs, mHistoryLen);
   Rotate(mImagFFTs, mHistoryLen);
}

void EffectNoiseRemoval::FinishTrack()
{
   // Keep flushing empty input buffers through the history
   // windows until we've output exactly as many samples as
   // were input.
   // Well, not exactly, but not more than mWindowSize/2 extra samples at the end.
   // We'll DELETE them later in ProcessOne.

   Floats empty{ mWindowSize / 2 };
   for(size_t i = 0; i < mWindowSize / 2; i++)
      empty[i] = 0.0;

   while (mOutSampleCount < mInSampleCount) {
      ProcessSamples(mWindowSize / 2, empty.get());
   }
}

void EffectNoiseRemoval::GetProfile()
{
   // The noise threshold for each frequency is the maximum
   // level achieved at that frequency for a minimum of
   // mMinSignalBlocks blocks in a row - the max of a min.

   int start = mHistoryLen - mMinSignalBlocks;
   int finish = mHistoryLen;
   int i;

   for (size_t j = 0; j < mSpectrumSize; j++) {
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
   size_t center = mHistoryLen / 2;
   size_t start = center - mMinSignalBlocks/2;
   size_t finish = start + mMinSignalBlocks;

   // Raise the gain for elements in the center of the sliding history
   for (size_t j = 0; j < mSpectrumSize; j++) {
      float min = mSpectrums[start][j];
      for (size_t i = start+1; i < finish; i++) {
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
   for (size_t j = 0; j < mSpectrumSize; j++) {
      for (size_t i = center + 1; i < mHistoryLen; i++) {
         if (mGains[i][j] < mGains[i - 1][j] * mOneBlockAttackDecay)
            mGains[i][j] = mGains[i - 1][j] * mOneBlockAttackDecay;
         if (mGains[i][j] < mNoiseAttenFactor)
            mGains[i][j] = mNoiseAttenFactor;
      }
      for (size_t i = center; i--;) {
         if (mGains[i][j] < mGains[i + 1][j] * mOneBlockAttackDecay)
            mGains[i][j] = mGains[i + 1][j] * mOneBlockAttackDecay;
         if (mGains[i][j] < mNoiseAttenFactor)
            mGains[i][j] = mNoiseAttenFactor;
      }
   }


   // Apply frequency smoothing to output gain
   int out = mHistoryLen - 1;  // end of the queue

   ApplyFreqSmoothing(mGains[out].get());

   // Apply gain to FFT
   for (size_t j = 0; j < (mSpectrumSize-1); j++) {
      mFFTBuffer[j*2  ] = mRealFFTs[out][j] * mGains[out][j];
      mFFTBuffer[j*2+1] = mImagFFTs[out][j] * mGains[out][j];
   }
   // The Fs/2 component is stored as the imaginary part of the DC component
   mFFTBuffer[1] = mRealFFTs[out][mSpectrumSize-1] * mGains[out][mSpectrumSize-1];

   // Invert the FFT into the output buffer
   InverseRealFFTf(mFFTBuffer.get(), hFFT.get());

   // Overlap-add
   for(size_t j = 0; j < (mSpectrumSize-1); j++) {
      mOutOverlapBuffer[j*2  ] += mFFTBuffer[hFFT->BitReversed[j]  ] * mWindow[j*2  ];
      mOutOverlapBuffer[j*2+1] += mFFTBuffer[hFFT->BitReversed[j]+1] * mWindow[j*2+1];
   }

   // Output the first half of the overlap buffer, they're done -
   // and then shift the next half over.
   if (mOutSampleCount >= 0) {   // ...but not if it's the first half-window
      mOutputTrack->Append((samplePtr)mOutOverlapBuffer.get(), floatSample,
                           mWindowSize / 2);
   }
   mOutSampleCount += mWindowSize / 2;
   for(size_t j = 0; j < mWindowSize / 2; j++) {
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
      mOutputTrack = track->EmptyCopy();

   auto bufferSize = track->GetMaxBlockSize();
   Floats buffer{ bufferSize };

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
      track->Get((samplePtr)buffer.get(), floatSample, samplePos, blockSize);

      mInSampleCount += blockSize;
      ProcessSamples(blockSize, buffer.get());

      samplePos += blockSize;

      // Update the Progress meter
      if (TrackProgress(count, (samplePos - start).as_double() / len.as_double())) {
         bLoopSuccess = false;
         break;
      }
   }

   FinishTrack();

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
         track->ClearAndPaste(t0, t0 + tLen, mOutputTrack.get(), true, false);
      }
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

NoiseRemovalDialog::NoiseRemovalDialog(
   EffectNoiseRemoval * effect, EffectSettingsAccess &access, wxWindow *parent)
   : EffectDialog( parent, XO("Noise Removal"), EffectTypeProcess)
   , mAccess{ access }
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

   auto cleanup = finally( [&] {
      m_pEffect->mSensitivity = oldSensitivity;
      m_pEffect->mNoiseGain = oldGain;
      m_pEffect->mFreqSmoothingHz =  oldFreq;
      m_pEffect->mAttackDecayTime =  oldTime;
      m_pEffect->mbLeaveNoise = oldLeaveNoise;
      m_pEffect->mDoProfile = oldDoProfile;
   } );

   m_pEffect->Preview(mAccess, false);
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
   S.StartStatic(XO("Step 1"));
   {
      S.AddVariableText(XO(
"Select a few seconds of just noise so Audacity knows what to filter out,\nthen click Get Noise Profile:"));
      m_pButton_GetProfile = S.Id(ID_BUTTON_GETPROFILE).AddButton(XXO("&Get Noise Profile"));
   }
   S.EndStatic();

   S.StartStatic(XO("Step 2"));
   {
      S.AddVariableText(XO(
"Select all of the audio you want filtered, choose how much noise you want\nfiltered out, and then click 'OK' to remove noise.\n"));

      S.StartMultiColumn(3, wxEXPAND);
      S.SetStretchyCol(2);
      {
         mGainT = S.Id(ID_GAIN_TEXT)
            .Validator<wxTextValidator>(wxFILTER_NUMERIC)
            .AddTextBox(XXO("Noise re&duction (dB):"), wxT(""), 0);

         mGainS = S.Id(ID_GAIN_SLIDER)
            .Name(XO("Noise reduction"))
            .Style(wxSL_HORIZONTAL)
            .MinSize( { 150, -1 } )
            .AddSlider( {}, 0, GAIN_MAX, GAIN_MIN);

         mSensitivityT = S.Id(ID_SENSITIVITY_TEXT)
            .Validator<wxTextValidator>(wxFILTER_NUMERIC)
            .AddTextBox(XXO("&Sensitivity (dB):"), wxT(""), 0);
         mSensitivityS = S.Id(ID_SENSITIVITY_SLIDER)
            .Name(XO("Sensitivity"))
            .Style(wxSL_HORIZONTAL)
            .MinSize( { 150, -1 } )
            .AddSlider( {}, 0, SENSITIVITY_MAX, SENSITIVITY_MIN);

         mFreqT = S.Id(ID_FREQ_TEXT)
            .Validator<wxTextValidator>(wxFILTER_NUMERIC)
            .AddTextBox(XXO("Fr&equency smoothing (Hz):"), wxT(""), 0);
         mFreqS = S.Id(ID_FREQ_SLIDER)
            .Name(XO("Frequency smoothing"))
            .Style(wxSL_HORIZONTAL)
            .MinSize( { 150, -1 } )
            .AddSlider( {}, 0, FREQ_MAX, FREQ_MIN);

         mTimeT = S.Id(ID_TIME_TEXT)
            .Validator<wxTextValidator>(wxFILTER_NUMERIC)
            .AddTextBox(XXO("Attac&k/decay time (secs):"), wxT(""), 0);
         mTimeS = S.Id(ID_TIME_SLIDER)
            .Name(XO("Attack/decay time"))
            .Style(wxSL_HORIZONTAL)
            .MinSize( { 150, -1 } )
            .AddSlider( {}, 0, TIME_MAX, TIME_MIN);

         S.AddPrompt(XXO("Noise:"));
         mKeepSignal = S.Id(ID_RADIOBUTTON_KEEPSIGNAL)
               .AddRadioButton(XXO("Re&move"));
         mKeepNoise = S.Id(ID_RADIOBUTTON_KEEPNOISE)
               .AddRadioButtonToGroup(XXO("&Isolate"));
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

   mSensitivityS->SetValue(std::clamp<long>(mSensitivity*100.0 + (SENSITIVITY_MAX-SENSITIVITY_MIN+1)/2.0, SENSITIVITY_MIN, SENSITIVITY_MAX));
   mGainS->SetValue(std::clamp<long>(mGain, GAIN_MIN, GAIN_MAX));
   mFreqS->SetValue(std::clamp<long>(mFreq / 10, FREQ_MIN, FREQ_MAX));
   mTimeS->SetValue(std::clamp<long>(mTime * TIME_MAX + 0.5, TIME_MIN, TIME_MAX));

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
   mSensitivityS->SetValue(std::clamp<long>(mSensitivity*100.0 + (SENSITIVITY_MAX-SENSITIVITY_MIN+1)/2.0, SENSITIVITY_MIN, SENSITIVITY_MAX));
}

void NoiseRemovalDialog::OnGainText(wxCommandEvent & WXUNUSED(event))
{
   mGainT->GetValue().ToDouble(&mGain);
   mGainS->SetValue(std::clamp<long>(mGain, GAIN_MIN, GAIN_MAX));
}

void NoiseRemovalDialog::OnFreqText(wxCommandEvent & WXUNUSED(event))
{
   mFreqT->GetValue().ToDouble(&mFreq);
   mFreqS->SetValue(std::clamp<long>(mFreq / 10, FREQ_MIN, FREQ_MAX));
}

void NoiseRemovalDialog::OnTimeText(wxCommandEvent & WXUNUSED(event))
{
   mTimeT->GetValue().ToDouble(&mTime);
   mTimeS->SetValue(std::clamp<long>(mTime * TIME_MAX + 0.5, TIME_MIN, TIME_MAX));
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
