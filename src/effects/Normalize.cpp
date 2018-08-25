/**********************************************************************

  Audacity: A Digital Audio Editor

  Normalize.cpp

  Dominic Mazzoni
  Vaughan Johnson (Preview)
  Max Maisel (Loudness)

*******************************************************************//**

\class EffectNormalize
\brief An Effect to bring the peak level up to a chosen level.

*//*******************************************************************/


#include "../Audacity.h" // for rint from configwin.h
#include "Normalize.h"

#include <math.h>

#include <wx/intl.h>
#include <wx/valgen.h>

#include "../Internat.h"
#include "../Prefs.h"
#include "../ShuttleGui.h"
#include "../WaveTrack.h"
#include "../widgets/valnum.h"

// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name         Type     Key                        Def      Min      Max   Scale
Param( PeakLevel,   double,  wxT("PeakLevel"),           -1.0,    -145.0,  0.0,  1  );
Param( LUFSLevel,   double,  wxT("LUFSLevel"),           -23.0,   -145.0,  0.0,  1  );
Param( RemoveDC,    bool,    wxT("RemoveDcOffset"),      true,    false,   true, 1  );
Param( ApplyGain,   bool,    wxT("ApplyGain"),           true,    false,   true, 1  );
Param( StereoInd,   bool,    wxT("StereoIndependent"),   false,   false,   true, 1  );
Param( UseLoudness, bool,    wxT("UseLoudness"),         false,   false,   true, 1  );

BEGIN_EVENT_TABLE(EffectNormalize, wxEvtHandler)
   EVT_CHECKBOX(wxID_ANY, EffectNormalize::OnUpdateUI)
   EVT_TEXT(wxID_ANY, EffectNormalize::OnUpdateUI)
END_EVENT_TABLE()

EffectNormalize::EffectNormalize()
{
   mPeakLevel = DEF_PeakLevel;
   mLUFSLevel = DEF_LUFSLevel;
   mDC = DEF_RemoveDC;
   mGain = DEF_ApplyGain;
   mStereoInd = DEF_StereoInd;
   mUseLoudness = DEF_UseLoudness;

   SetLinearEffectFlag(false);
}

EffectNormalize::~EffectNormalize()
{
}

// IdentInterface implementation

IdentInterfaceSymbol EffectNormalize::GetSymbol()
{
   return NORMALIZE_PLUGIN_SYMBOL;
}

wxString EffectNormalize::GetDescription()
{
   return _("Sets the peak amplitude or loudness of one or more tracks");
}

wxString EffectNormalize::ManualPage()
{
   return wxT("Normalize");
}

// EffectDefinitionInterface implementation

EffectType EffectNormalize::GetType()
{
   return EffectTypeProcess;
}

// EffectClientInterface implementation
bool EffectNormalize::DefineParams( ShuttleParams & S ){
   S.SHUTTLE_PARAM( mPeakLevel, PeakLevel );
   S.SHUTTLE_PARAM( mLUFSLevel, LUFSLevel );
   S.SHUTTLE_PARAM( mGain, ApplyGain );
   S.SHUTTLE_PARAM( mDC, RemoveDC );
   S.SHUTTLE_PARAM( mStereoInd, StereoInd );
   S.SHUTTLE_PARAM( mUseLoudness, UseLoudness );
   return true;
}

bool EffectNormalize::GetAutomationParameters(CommandParameters & parms)
{
   parms.Write(KEY_PeakLevel, mPeakLevel);
   parms.Write(KEY_LUFSLevel, mLUFSLevel);
   parms.Write(KEY_ApplyGain, mGain);
   parms.Write(KEY_RemoveDC, mDC);
   parms.Write(KEY_StereoInd, mStereoInd);
   parms.Write(KEY_UseLoudness, mUseLoudness);

   return true;
}

bool EffectNormalize::SetAutomationParameters(CommandParameters & parms)
{
   ReadAndVerifyDouble(PeakLevel);
   ReadAndVerifyDouble(LUFSLevel);
   ReadAndVerifyBool(ApplyGain);
   ReadAndVerifyBool(RemoveDC);
   ReadAndVerifyBool(StereoInd);
   ReadAndVerifyBool(UseLoudness);

   mPeakLevel = PeakLevel;
   mLUFSLevel = LUFSLevel;
   mGain = ApplyGain;
   mDC = RemoveDC;
   mStereoInd = StereoInd;
   mUseLoudness = UseLoudness;

   return true;
}

// Effect implementation

bool EffectNormalize::CheckWhetherSkipEffect()
{
   return ((mGain == false) && (mDC == false));
}

bool EffectNormalize::Startup()
{
   wxString base = wxT("/Effects/Normalize/");

   // Migrate settings from 2.1.0 or before

   // Already migrated, so bail
   if (gPrefs->Exists(base + wxT("Migrated")))
   {
      return true;
   }

   // Load the old "current" settings
   if (gPrefs->Exists(base))
   {
      int boolProxy = gPrefs->Read(base + wxT("RemoveDcOffset"), 1);
      mDC = (boolProxy == 1);
      boolProxy = gPrefs->Read(base + wxT("Normalize"), 1);
      mGain = (boolProxy == 1);
      gPrefs->Read(base + wxT("Level"), &mPeakLevel, -1.0);
      if(mPeakLevel > 0.0)  // this should never happen
         mPeakLevel = -mPeakLevel;
      boolProxy = gPrefs->Read(base + wxT("StereoIndependent"), 0L);
      mStereoInd = (boolProxy == 1);
      mUseLoudness = false;
      mLUFSLevel = DEF_LUFSLevel;

      SaveUserPreset(GetCurrentSettingsGroup());

      // Do not migrate again
      gPrefs->Write(base + wxT("Migrated"), true);
      gPrefs->Flush();
   }

   return true;
}

bool EffectNormalize::Process()
{
   if (mGain == false && mDC == false)
      return true;

   float ratio;
   if( mGain )
   {
      if(mUseLoudness)
         // LU use 10*log10(...) instead of 20*log10(...)
         // so multiply level by 2 and use standard DB_TO_LINEAR macro.
         ratio = DB_TO_LINEAR(TrapDouble(mLUFSLevel*2, MIN_LUFSLevel, MAX_LUFSLevel));
      else
         // same value used for all tracks
         ratio = DB_TO_LINEAR(TrapDouble(mPeakLevel, MIN_PeakLevel, MAX_PeakLevel));
   }
   else
      ratio = 1.0;

   //Iterate over each track
   this->CopyInputTracks(); // Set up mOutputTracks.
   bool bGoodResult = true;
   SelectedTrackListOfKindIterator iter(Track::Wave, mOutputTracks.get());
   WaveTrack *track = (WaveTrack *) iter.First();
   wxString topMsg;

   if(mDC && mGain)
      topMsg = _("Removing DC offset and Normalizing...\n");
   else if(mDC && !mGain)
      topMsg = _("Removing DC offset...\n");
   else if(!mDC && mGain)
      topMsg = _("Normalizing without removing DC offset...\n");
   else if(!mDC && !mGain)
      topMsg = _("Not doing anything...\n");   // shouldn't get here

   AllocBuffers(iter);
   mProgressVal = 0;

   while (track)
   {
      //Get start and end times from track
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();

      //Set the current bounds to whichever left marker is
      //greater and whichever right marker is less:
      mCurT0 = mT0 < trackStart? trackStart: mT0;
      mCurT1 = mT1 > trackEnd? trackEnd: mT1;

      // Get the track rate
      mCurRate = track->GetRate();

      wxString msg;
      auto trackName = track->GetName();
      mSteps = 2;

      if(!mStereoInd && track->GetLinked())
         mProcStereo = true;

      mProgressMsg =
         topMsg + wxString::Format( _("Analyzing: %s"), trackName );

      InitTrackAnalysis();

      // Get track min/max in peak mode
      if(!mUseLoudness)
      {
         if(!GetTrackMinMax(track, mMin[0], mMax[0]))
            return false;
         if(mProcStereo)
         {
            // Get next WaveTrack* without incrementing iter.
            track = (WaveTrack *) iter.Next();
            iter.Prev();
            if(!GetTrackMinMax(track, mMin[1], mMax[1]))
               return false;
         }
      }
      // Skip analyze pass if it is not necessary.
      if(mUseLoudness || mDC)
      {
         mBlockSize = 0.4 * mCurRate; // 400 ms blocks
         mBlockOverlap = 0.1 * mCurRate; // 100 ms overlap
         if(!ProcessOne(iter, true))
            // Processing failed -> abort
            return false;
      }
      else
         mSteps = 1;

      // Calculate normalization values the analysis results
      // mMin[], mMax[], mSum[], mLoudnessHist.
      if(mCount > 0 && mDC)
      {
         mOffset[0] = -mSum[0] / mCount.as_double();
         mOffset[1] = -mSum[1] / mCount.as_double();
         mMin[0] += mOffset[0];
         mMin[1] += mOffset[1];
         mMax[0] += mOffset[0];
         mMax[1] += mOffset[1];
      }
      else
      {
         mOffset[0] = 0.0;
         mOffset[1] = 0.0;
      }

      float extent;
      // EBU R128: z_i = mean square without root
      if(mUseLoudness)
      {
         // Calculate Gamma_R from histogram.
         double sum_v = 0;
         double val;
         long int sum_c = 0;
         for(size_t i = 0; i < HIST_BIN_COUNT; ++i)
         {
            val = -GAMMA_A / double(HIST_BIN_COUNT) * (i+1) + GAMMA_A;
            sum_v += pow(10, val) * mLoudnessHist[i];
            sum_c += mLoudnessHist[i];
         }

         // Histogram values are simplified log(x^2) immediate values
         // without -0.691 + 10*(...) to safe computing power. This is
         // possible because they will cancel out anyway.
         // The -1 in the line below is the -10 LUFS from the EBU R128
         // specification without the scaling factor of 10.
         double Gamma_R = log10(sum_v/sum_c) - 1;
         size_t idx_R = round((Gamma_R - GAMMA_A) * double(HIST_BIN_COUNT) / -GAMMA_A - 1);

         // Apply Gamma_R threshold and calculate gated loudness (extent).
         sum_v = 0;
         sum_c = 0;
         for(size_t i = idx_R+1; i < HIST_BIN_COUNT; ++i)
         {
            val = -GAMMA_A / double(HIST_BIN_COUNT) * (i+1) + GAMMA_A;
            sum_v += pow(10, val) * mLoudnessHist[i];
            sum_c += mLoudnessHist[i];
         }
         // LUFS is defined as -0.691 dB + 10*log10(sum(channels))
         extent = 0.8529037031 * sum_v / sum_c;
      }
      else
         extent = fmax(fabs(mMin[0]), fabs(mMax[0]));

      if(mProcStereo)
      {
         if(!mUseLoudness)
         {
            // Peak: use maximum of both tracks.
            float extent2;
            extent2 = fmax(fabs(mMin[1]), fabs(mMax[1]));;
            extent = fmax(extent, extent2);
         }
         // else: mNormalizeTo == kLoudness : do nothing (this is already handled in histogram)
      }

      if( (extent > 0) && mGain )
      {
         mMult = ratio / extent;
         if(mUseLoudness)
         {
            // LUFS are related to square values so the multiplier must be the root.
            mMult = sqrt(mMult);
         }
      }
      else
         mMult = 1.0;

      mProgressMsg =
         topMsg + wxString::Format( _("Processing: %s"), trackName );

      if(!ProcessOne(iter, false))
         // Processing failed -> abort
         return false;

      //Iterate to the next track
      track = (WaveTrack *) iter.Next();
      if(mProcStereo)
         // Stereo track is already processed so advance again.
         track = (WaveTrack *) iter.Next();
   }

   this->ReplaceProcessedTracks(bGoodResult);

   // Free memory
   mTrackBuffer[0].reset();
   mTrackBuffer[1].reset();
   mBlockRingBuffer.reset();
   mLoudnessHist.reset();

   return bGoodResult;
}

void EffectNormalize::PopulateOrExchange(ShuttleGui & S)
{
   mCreating = true;

   S.StartVerticalLay(0);
   {
      S.StartMultiColumn(2, wxALIGN_CENTER);
      {
         S.StartVerticalLay(false);
         {
            mDCCheckBox = S.AddCheckBox(_("Remove DC offset (center on 0.0 vertically)"),
                                        mDC ? wxT("true") : wxT("false"));
            mDCCheckBox->SetValidator(wxGenericValidator(&mDC));

            S.StartHorizontalLay(wxALIGN_LEFT, false);
            {
               // The checkbox needs to be sized for the longer prompt, and
               // which that is will depend on translation.  So decide that here.
               // (strictly we should count pixels, not characters).
               wxString prompt1 = _("Normalize peak amplitude to");
               wxString prompt2 = _("Normalize loudness to");
               wxString longerPrompt = ((prompt1.Length() > prompt2.Length()) ? prompt1 : prompt2) + "   ";

               // Now make the checkbox.
               mGainCheckBox = S.AddCheckBox(longerPrompt,
                                             mGain ? wxT("true") : wxT("false"));
               mGainCheckBox->SetValidator(wxGenericValidator(&mGain));
               mGainCheckBox->SetMinSize( mGainCheckBox->GetSize());

               FloatingPointValidator<double> vldLevel(2, &mPeakLevel,
                                                       NumValidatorStyle::ONE_TRAILING_ZERO);
               vldLevel.SetRange( MIN_PeakLevel, MAX_PeakLevel);

               mLevelTextCtrl = S.AddTextBox( {}, wxT(""), 10);
               mLevelTextCtrl->SetName( _("Peak amplitude dB"));
               mLevelTextCtrl->SetValidator(vldLevel);
               mLeveldB = S.AddVariableText(_("dB"), false,
                                            wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
               mWarning = S.AddVariableText( {}, false,
                                            wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
            }
            S.EndHorizontalLay();

            mUseLoudnessCheckBox = S.AddCheckBox(_("Use loudness instead of peak amplitude"),
                                                 mUseLoudness ? wxT("true") : wxT("false"));
            mUseLoudnessCheckBox->SetValidator(wxGenericValidator(&mGUIUseLoudness));

            mStereoIndCheckBox = S.AddCheckBox(_("Normalize stereo channels independently"),
                                               mStereoInd ? wxT("true") : wxT("false"));
            mStereoIndCheckBox->SetValidator(wxGenericValidator(&mStereoInd));
         }
         S.EndVerticalLay();
      }
      S.EndMultiColumn();
   }
   S.EndVerticalLay();
   // To ensure that the UpdateUI on creation sets the prompts correctly.
   mUseLoudness = !mGUIUseLoudness;
   mCreating = false;
}

bool EffectNormalize::TransferDataToWindow()
{
   if (!mUIParent->TransferDataToWindow())
   {
      return false;
   }

   UpdateUI();

   return true;
}

bool EffectNormalize::TransferDataFromWindow()
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }

   return true;
}

// EffectNormalize implementation

bool EffectNormalize::GetTrackMinMax(WaveTrack* track, float& min, float& max)
{
   // Since we need complete summary data, we need to block until the OD tasks are done for this track
   // This is needed for track->GetMinMax
   // TODO: should we restrict the flags to just the relevant block files (for selections)
   while (track->GetODFlags()) {
      // update the gui
      if (ProgressResult::Cancelled == mProgress->Update(
         0, _("Waiting for waveform to finish computing...")) )
         return false;
      wxMilliSleep(100);
   }

   // set mMin, mMax.  No progress bar here as it's fast.
   auto pair = track->GetMinMax(mCurT0, mCurT1); // may throw
   min = pair.first, max = pair.second;
   return true;
}

/// Get required buffer size for the largest whole track and allocate buffers.
/// This reduces the amount of allocations required.
void EffectNormalize::AllocBuffers(SelectedTrackListOfKindIterator iter)
{
   mTrackBufferCapacity = 0;
   bool stereoTrackFound = false;
   double maxSampleRate = 0;
   mProcStereo = false;

   WaveTrack *track = (WaveTrack *) iter.First();
   while (track)
   {
      mTrackBufferCapacity = std::max(mTrackBufferCapacity, track->GetMaxBlockSize());
      maxSampleRate = std::max(maxSampleRate, track->GetRate());

      // There is a stereo track
      if(track->GetLinked())
         stereoTrackFound = true;

      // Iterate to the next track
      track = (WaveTrack *) iter.Next();
   }

   // Allocate histogram buffers
   mLoudnessHist.reinit(HIST_BIN_COUNT, false);
   mBlockRingBuffer.reinit(static_cast<size_t>(ceil(0.4 * maxSampleRate))); // 400 ms blocks

   //Initiate a processing buffer.  This buffer will (most likely)
   //be shorter than the length of the track being processed.
   mTrackBuffer[0].reinit(mTrackBufferCapacity);

   if(!mStereoInd && stereoTrackFound)
      mTrackBuffer[1].reinit(mTrackBufferCapacity);
}

/// ProcessOne() takes a track, transforms it to bunch of buffer-blocks,
/// and executes ProcessData, on it...
///  uses mMult and offset to normalize a track.
///  mMult must be set before this is called
/// In analyse mode, it executes the selected analyse operation on it...
///  mMult does not have to be set before this is called
bool EffectNormalize::ProcessOne(SelectedTrackListOfKindIterator iter, bool analyse)
{
   WaveTrack* track1 = (WaveTrack *) *iter;
   WaveTrack* track2 = 0;

   //Transform the marker timepoints to samples
   auto start = track1->TimeToLongSamples(mCurT0);
   auto end   = track1->TimeToLongSamples(mCurT1);

   //Get the length of the buffer (as double). len is
   //used simply to calculate a progress meter, so it is easier
   //to make it a double now than it is to do it later
   mTrackLen = (end - start).as_double();

   // Abort if the right marker is not to the right of the left marker
   if (mCurT1 <= mCurT0)
      return false;

   if(mProcStereo)
      track2 = (WaveTrack *) iter.Next();

   //Go through the track one buffer at a time. s counts which
   //sample the current buffer starts at.
   auto s = start;
   while (s < end)
   {
      //Get a block of samples (smaller than the size of the buffer)
      //Adjust the block size if it is the final block in the track
      const auto blockLen = limitSampleBufferSize(
         track1->GetBestBlockSize(s),
         mTrackBufferCapacity
      );

      if(!LoadBufferBlock(track1, track2, s, blockLen))
         return false;

      //Process the buffer.
      if(analyse)
      {
         if(!AnalyseBufferBlock())
            return false;
      }
      else
      {
         if(!ProcessBufferBlock())
            return false;
      }

      if(!analyse)
         StoreBufferBlock(track1, track2, s, blockLen);

      //Increment s one blockfull of samples
      s += blockLen;
   }

   //Return true because the effect processing succeeded ... unless cancelled
   return true;
}

bool EffectNormalize::LoadBufferBlock(WaveTrack* track1, WaveTrack* track2,
                                      sampleCount pos, size_t len)
{
   sampleCount read_size;
   // Get the samples from the track and put them in the buffer
   track1->Get((samplePtr) mTrackBuffer[0].get(), floatSample, pos, mTrackBufferCapacity,
               fillZero, true, &read_size);
   mTrackBufferLen = read_size.as_size_t();

   // Get linked stereo track as well.
   if(mProcStereo)
   {
      track2->Get((samplePtr) mTrackBuffer[1].get(), floatSample, pos, mTrackBufferCapacity,
                  fillZero, true, &read_size);
      // Fail if we read different sample count from stereo pair tracks.
      if(read_size.as_size_t() != mTrackBufferLen)
         return false;
   }
   return true;
}

/// Calculates sample sum (for DC) and EBU R128 weighted square sum
/// (for loudness).
bool EffectNormalize::AnalyseBufferBlock()
{
   for(size_t i = 0; i < mTrackBufferLen; i++)
   {
      if(mOp & ANALYSE_DC)
      {
         mSum[0] += (double)mTrackBuffer[0][i];
         if(mProcStereo)
            mSum[1] += (double)mTrackBuffer[1][i];
      }
      if(mOp & ANALYSE_LOUDNESS)
      {
         double value;
         value = mR128HSF[0].ProcessOne(mTrackBuffer[0][i]);
         value = mR128HPF[0].ProcessOne(value);
         mBlockRingBuffer[mBlockRingPos] = value * value;
         if(mProcStereo)
         {
            value = mR128HSF[1].ProcessOne(mTrackBuffer[1][i]);
            value = mR128HPF[1].ProcessOne(value);
            // Add the power of second channel to the power of first channel.
            // As a result, stereo tracks appear about 3 LUFS louder, as specified.
            mBlockRingBuffer[mBlockRingPos] += value * value;
         }
         ++mBlockRingPos;
         ++mBlockRingSize;

         if(mBlockRingPos % mBlockOverlap == 0)
         {
            // Process new full block. As incomplete blocks shall be discarded
            // according to the EBU R128 specification there is no need for
            // some special logic for the last blocks.
            if(mBlockRingSize >= mBlockSize)
            {
               // Reset mBlockRingSize to full state to avoid overflow.
               // The actual value of mBlockRingSize does not matter
               // since this is only used to detect if blocks are complete (>= mBlockSize).
               mBlockRingSize = mBlockSize;

               size_t idx;
               double blockVal = 0;
               for(size_t i = 0; i < mBlockSize; ++i)
                  blockVal += mBlockRingBuffer[i];

               // Histogram values are simplified log10() immediate values
               // without -0.691 + 10*(...) to safe computing power. This is
               // possible because these constant cancel out anyway during the
               // following processing steps.
               blockVal = log10(blockVal/double(mBlockSize));
               // log(blockVal) is within ]-inf, 1]
               idx = round((blockVal - GAMMA_A) * double(HIST_BIN_COUNT) / -GAMMA_A - 1);

               // idx is within ]-inf, HIST_BIN_COUNT-1], discard indices below 0
               // as they are below the EBU R128 absolute threshold anyway.
               if(idx >= 0 && idx < HIST_BIN_COUNT)
                  ++mLoudnessHist[idx];
            }
         }
         // Close the ring.
         if(mBlockRingPos == mBlockSize)
            mBlockRingPos = 0;
      }
   }
   mCount += mTrackBufferLen;

   if(!UpdateProgress())
      return false;
   return true;
}

bool EffectNormalize::ProcessBufferBlock()
{
   for(size_t i = 0; i < mTrackBufferLen; i++)
   {
      mTrackBuffer[0][i] = (mTrackBuffer[0][i] + mOffset[0]) * mMult;
      if(mProcStereo)
         mTrackBuffer[1][i] = (mTrackBuffer[1][i] + mOffset[1]) * mMult;
   }

   if(!UpdateProgress())
      return false;
   return true;
}

void EffectNormalize::StoreBufferBlock(WaveTrack* track1, WaveTrack* track2,
                                       sampleCount pos, size_t len)
{
   // Copy the newly-changed samples back onto the track.
   track1->Set((samplePtr) mTrackBuffer[0].get(), floatSample, pos, len);
   // Store linked stereo track as well.
   if(mProcStereo)
      track2->Set((samplePtr) mTrackBuffer[1].get(), floatSample, pos, len);
}

void EffectNormalize::InitTrackAnalysis()
{
   mSum[0]   = 0.0; // dc offset inits
   mSum[0]   = 0.0;
   mCount = 0;

   mBlockRingPos = 0;
   mBlockRingSize = 0;
   memset(mLoudnessHist.get(), 0, HIST_BIN_COUNT*sizeof(long int));

   // Normalize ?
   if(mGain)
   {
      if(mUseLoudness)
      {
         CalcEBUR128HPF(mCurRate);
         CalcEBUR128HSF(mCurRate);
         if(mDC)
            mOp = ANALYSE_LOUDNESS_DC;
         else
         {
            mOp = ANALYSE_LOUDNESS;
            mOffset[0] = 0.0;
            mOffset[1] = 0.0;
         }
      }
      else if(mDC)
         mOp = ANALYSE_DC;
   }
   // Just remove DC ?
   else if(mDC)
   {
      mMin[0] = -1.0, mMax[0] = 1.0;   // sensible defaults?
      mMin[1] = -1.0, mMax[1] = 1.0;
      mOp = ANALYSE_NONE;
   }
   // Do nothing
   else
   {
      mMin[0] = -1.0, mMax[0] = 1.0;   // sensible defaults?
      mMin[1] = -1.0, mMax[1] = 1.0;
      mOffset[0] = 0.0;
      mOffset[1] = 0.0;
      mOp = ANALYSE_NONE;
   }
}

// EBU R128 parameter sampling rate adaption after
// Mansbridge, Stuart, Saoirse Finn, and Joshua D. Reiss.
// "Implementation and Evaluation of Autonomous Multi-track Fader Control."
// Paper presented at the 132nd Audio Engineering Society Convention,
// Budapest, Hungary, 2012."
void EffectNormalize::CalcEBUR128HPF(float fs)
{
   double f0 = 38.13547087602444;
   double Q  =  0.5003270373238773;
   double K  = tan(M_PI * f0 / fs);

   mR128HPF[0].Reset();

   mR128HPF[0].fNumerCoeffs[Biquad::B0] =  1.0;
   mR128HPF[0].fNumerCoeffs[Biquad::B1] = -2.0;
   mR128HPF[0].fNumerCoeffs[Biquad::B2] =  1.0;

   mR128HPF[0].fDenomCoeffs[Biquad::A1] = 2.0 * (K * K - 1.0) / (1.0 + K / Q + K * K);
   mR128HPF[0].fDenomCoeffs[Biquad::A2] = (1.0 - K / Q + K * K) / (1.0 + K / Q + K * K);

   if(mProcStereo)
   {
      mR128HPF[1].Reset();
      mR128HPF[1].fNumerCoeffs[Biquad::B0] = mR128HPF[0].fNumerCoeffs[Biquad::B0];
      mR128HPF[1].fNumerCoeffs[Biquad::B1] = mR128HPF[0].fNumerCoeffs[Biquad::B1];
      mR128HPF[1].fNumerCoeffs[Biquad::B2] = mR128HPF[0].fNumerCoeffs[Biquad::B2];
      mR128HPF[1].fDenomCoeffs[Biquad::A1] = mR128HPF[0].fDenomCoeffs[Biquad::A1];
      mR128HPF[1].fDenomCoeffs[Biquad::A2] = mR128HPF[0].fDenomCoeffs[Biquad::A2];
   }
}

// EBU R128 parameter sampling rate adaption after
// Mansbridge, Stuart, Saoirse Finn, and Joshua D. Reiss.
// "Implementation and Evaluation of Autonomous Multi-track Fader Control."
// Paper presented at the 132nd Audio Engineering Society Convention,
// Budapest, Hungary, 2012."
void EffectNormalize::CalcEBUR128HSF(float fs)
{
   double db =    3.999843853973347;
   double f0 = 1681.974450955533;
   double Q  =    0.7071752369554196;
   double K  = tan(M_PI * f0 / fs);

   double Vh = pow(10.0, db / 20.0);
   double Vb = pow(Vh, 0.4996667741545416);

   double a0 = 1.0 + K / Q + K * K;

   mR128HSF[0].Reset();

   mR128HSF[0].fNumerCoeffs[Biquad::B0] = (Vh + Vb * K / Q + K * K) / a0;
   mR128HSF[0].fNumerCoeffs[Biquad::B1] =       2.0 * (K * K -  Vh) / a0;
   mR128HSF[0].fNumerCoeffs[Biquad::B2] = (Vh - Vb * K / Q + K * K) / a0;

   mR128HSF[0].fDenomCoeffs[Biquad::A1] =   2.0 * (K * K - 1.0) / a0;
   mR128HSF[0].fDenomCoeffs[Biquad::A2] = (1.0 - K / Q + K * K) / a0;

   if(mProcStereo)
   {
      mR128HSF[1].Reset();
      mR128HSF[1].fNumerCoeffs[Biquad::B0] = mR128HSF[0].fNumerCoeffs[Biquad::B0];
      mR128HSF[1].fNumerCoeffs[Biquad::B1] = mR128HSF[0].fNumerCoeffs[Biquad::B1];
      mR128HSF[1].fNumerCoeffs[Biquad::B2] = mR128HSF[0].fNumerCoeffs[Biquad::B2];
      mR128HSF[1].fDenomCoeffs[Biquad::A1] = mR128HSF[0].fDenomCoeffs[Biquad::A1];
      mR128HSF[1].fDenomCoeffs[Biquad::A2] = mR128HSF[0].fDenomCoeffs[Biquad::A2];
   }
}

bool EffectNormalize::UpdateProgress()
{
   mProgressVal += (double(1+mProcStereo) * double(mTrackBufferLen)
                 / (double(GetNumWaveTracks()) * double(mSteps) * mTrackLen));
   return !TotalProgress(mProgressVal, mProgressMsg);
}

void EffectNormalize::OnUpdateUI(wxCommandEvent & WXUNUSED(evt))
{
   UpdateUI();
}

void EffectNormalize::UpdateUI()
{
   if (!mUIParent->TransferDataFromWindow())
   {
      mWarning->SetLabel(_(".  Maximum 0dB."));
      EnableApply(false);
      return;
   }
   mWarning->SetLabel(wxT(""));

   // Changing the prompts causes an unwanted UpdateUI event.  
   // This 'guard' stops that becoming an infinite recursion.
   if (mUseLoudness != mGUIUseLoudness)
   {
      mUseLoudness = mGUIUseLoudness;
      if (mUseLoudness)
      {
         FloatingPointValidator<double> vldLevel(2, &mLUFSLevel, NumValidatorStyle::ONE_TRAILING_ZERO);
         vldLevel.SetRange(MIN_LUFSLevel, MAX_LUFSLevel);
         mLevelTextCtrl->SetValidator(vldLevel);
         /* i18n-hint: LUFS is a particular method for measuring loudnesss */
         mLevelTextCtrl->SetName(_("Loudness LUFS"));
         mLevelTextCtrl->SetValue(wxString::FromDouble(mLUFSLevel));
         /* i18n-hint: LUFS is a particular method for measuring loudnesss */
         mLeveldB->SetLabel(_("LUFS"));
         mGainCheckBox->SetLabelText(_("Normalize loudness to"));
      }
      else
      {
         FloatingPointValidator<double> vldLevel(2, &mPeakLevel, NumValidatorStyle::ONE_TRAILING_ZERO);
         vldLevel.SetRange(MIN_PeakLevel, MAX_PeakLevel);
         mLevelTextCtrl->SetValidator(vldLevel);
         mLevelTextCtrl->SetName(_("Peak amplitude dB"));
         mLevelTextCtrl->SetValue(wxString::FromDouble(mPeakLevel));
         mLeveldB->SetLabel(_("dB"));
         mGainCheckBox->SetLabelText(_("Normalize peak amplitude to"));
      }
   }

   // Disallow level stuff if not normalizing
   mLevelTextCtrl->Enable(mGain);
   mLeveldB->Enable(mGain);
   mStereoIndCheckBox->Enable(mGain);
   mUseLoudnessCheckBox->Enable(mGain);

   // Disallow OK/Preview if doing nothing
   EnableApply(mGain || mDC);
}
