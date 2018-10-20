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

enum kNormalizeTargets
{
   kAmplitude,
   kLoudness,
   kRMS,
   nAlgos
};

static const ComponentInterfaceSymbol kNormalizeTargetStrings[nAlgos] =
{
   { XO("peak amplitude") },
   { XO("perceived loudness") },
   { XO("RMS") }
};
// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name         Type     Key                        Def         Min      Max       Scale
Param( PeakLevel,   double,  wxT("PeakLevel"),           -1.0,       -145.0,  0.0,      1  );
Param( LUFSLevel,   double,  wxT("LUFSLevel"),           -23.0,      -145.0,  0.0,      1  );
Param( RMSLevel,    double,  wxT("RMSLevel"),            -20.0,      -145.0,  0.0,      1  );
Param( RemoveDC,    bool,    wxT("RemoveDcOffset"),      true,       false,   true,     1  );
Param( ApplyGain,   bool,    wxT("ApplyGain"),           true,       false,   true,     1  );
Param( StereoInd,   bool,    wxT("StereoIndependent"),   false,      false,   true,     1  );
Param( DualMono,    bool,    wxT("DualMono"),            true,       false,   true,     1  );
Param( NormalizeTo, int,     wxT("NormalizeTo"),         kAmplitude, 0    ,   nAlgos-1, 1  );

BEGIN_EVENT_TABLE(EffectNormalize, wxEvtHandler)
   EVT_CHOICE(wxID_ANY, EffectNormalize::OnUpdateUI)
   EVT_CHECKBOX(wxID_ANY, EffectNormalize::OnUpdateUI)
   EVT_TEXT(wxID_ANY, EffectNormalize::OnUpdateUI)
END_EVENT_TABLE()

EffectNormalize::EffectNormalize(bool isLoudness)
{
   mIsLoudness = isLoudness;

   mPeakLevel = DEF_PeakLevel;
   mLUFSLevel = DEF_LUFSLevel;
   mRMSLevel = DEF_RMSLevel;
   mDC = DEF_RemoveDC;
   mGain = DEF_ApplyGain;
   mStereoInd = DEF_StereoInd;
   mDualMono = DEF_DualMono;
   mNormalizeTo = DEF_NormalizeTo;

   SetLinearEffectFlag(false);
}

EffectNormalize::~EffectNormalize()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectNormalize::GetSymbol()
{
   if(mIsLoudness)
      return LOUDNESS_PLUGIN_SYMBOL;
   else
      return NORMALIZE_PLUGIN_SYMBOL;
}

wxString EffectNormalize::GetDescription()
{
   if(mIsLoudness)
      return _("Sets the peak amplitude or loudness of one or more tracks");
   else
      return _("Sets the peak amplitude of one or more tracks");
}

wxString EffectNormalize::ManualPage()
{
   if(mIsLoudness)
      return wxT("Loudness");
   else
      return wxT("Normalize");
}

// EffectDefinitionInterface implementation

EffectType EffectNormalize::GetType()
{
   return EffectTypeProcess;
}

// EffectClientInterface implementation
bool EffectNormalize::DefineParams( ShuttleParams & S )
{
   S.SHUTTLE_PARAM( mPeakLevel, PeakLevel );
   S.SHUTTLE_PARAM( mGain, ApplyGain );
   S.SHUTTLE_PARAM( mDC, RemoveDC );
   S.SHUTTLE_PARAM( mStereoInd, StereoInd );
   if(mIsLoudness)
   {
      S.SHUTTLE_PARAM( mLUFSLevel, LUFSLevel );
      S.SHUTTLE_PARAM( mRMSLevel, RMSLevel );
      S.SHUTTLE_PARAM( mDualMono, DualMono );
      S.SHUTTLE_PARAM( mNormalizeTo, NormalizeTo );
   }
   else
      mNormalizeTo = kAmplitude;
   return true;
}

bool EffectNormalize::GetAutomationParameters(CommandParameters & parms)
{
   parms.Write(KEY_PeakLevel, mPeakLevel);
   parms.Write(KEY_ApplyGain, mGain);
   parms.Write(KEY_RemoveDC, mDC);
   parms.Write(KEY_StereoInd, mStereoInd);
   if(mIsLoudness)
   {
      parms.Write(KEY_LUFSLevel, mLUFSLevel);
      parms.Write(KEY_RMSLevel, mRMSLevel);
      parms.Write(KEY_DualMono, mDualMono);
      parms.Write(KEY_NormalizeTo, mNormalizeTo);
   }

   return true;
}

bool EffectNormalize::SetAutomationParameters(CommandParameters & parms)
{
   ReadAndVerifyDouble(PeakLevel);
   ReadAndVerifyBool(ApplyGain);
   ReadAndVerifyBool(RemoveDC);
   ReadAndVerifyBool(StereoInd);

   mPeakLevel = PeakLevel;
   mGain = ApplyGain;
   mDC = RemoveDC;
   mStereoInd = StereoInd;

   if(mIsLoudness)
   {
      ReadAndVerifyDouble(LUFSLevel);
      ReadAndVerifyDouble(RMSLevel);
      ReadAndVerifyBool(DualMono);
      ReadAndVerifyBool(NormalizeTo);

      mLUFSLevel = LUFSLevel;
      mRMSLevel = RMSLevel;
      mDualMono = DualMono;
      mNormalizeTo = NormalizeTo;
   }
   else
      mNormalizeTo = kAmplitude;

   return true;
}

// Effect implementation

bool EffectNormalize::CheckWhetherSkipEffect()
{
   return ((mGain == false) && (mDC == false));
}

bool EffectNormalize::Startup()
{
   wxString base;
   if(mIsLoudness)
      base = wxT("/Effects/Loudness/");
   else
      base = wxT("/Effects/Normalize/");

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
      if(mIsLoudness)
      {
         mDualMono = DEF_DualMono;
         mNormalizeTo = kAmplitude;
         mLUFSLevel = DEF_LUFSLevel;
         mRMSLevel = DEF_RMSLevel;
      }

      SaveUserPreset(GetCurrentSettingsGroup());

      // Do not migrate again
      gPrefs->Write(base + wxT("Migrated"), true);
      gPrefs->Flush();
   }

   return true;
}

bool EffectNormalize::Process()
{
   // Use temporary copy of mDC so that the checkbox
   // state is unaffected by the operation below.
   bool dc = mDC && mNormalizeTo != kRMS;
   if (mGain == false && dc == false)
      return true;

   float ratio;
   if( mGain )
   {
      if(mNormalizeTo == kAmplitude)
         ratio = DB_TO_LINEAR(TrapDouble(mPeakLevel, MIN_PeakLevel, MAX_PeakLevel));
      else if(mNormalizeTo == kLoudness)
         // LU use 10*log10(...) instead of 20*log10(...)
         // so multiply level by 2 and use standard DB_TO_LINEAR macro.
         ratio = DB_TO_LINEAR(TrapDouble(mLUFSLevel*2, MIN_LUFSLevel, MAX_LUFSLevel));
      else // RMS
         ratio = DB_TO_LINEAR(TrapDouble(mRMSLevel, MIN_RMSLevel, MAX_RMSLevel));

   }
   else
      ratio = 1.0;

   //Iterate over each track
   this->CopyInputTracks(); // Set up mOutputTracks.
   bool bGoodResult = true;
   wxString topMsg;

   if(dc && mGain)
      topMsg = _("Removing DC offset and Normalizing...\n");
   else if(dc && !mGain)
      topMsg = _("Removing DC offset...\n");
   else if(!dc && mGain)
      topMsg = _("Normalizing without removing DC offset...\n");
   else if(!dc && !mGain)
      topMsg = _("Not doing anything...\n");   // shouldn't get here

   AllocBuffers();
   mProgressVal = 0;

   for(auto track : mOutputTracks->Selected<WaveTrack>()
       + (mStereoInd ? &Track::Any : &Track::IsLeader))
   {
      //Get start and end times from track
      // PRL: No accounting for multiple channels ?
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

      mProgressMsg =
         topMsg + wxString::Format( _("Analyzing: %s"), trackName );

      InitTrackAnalysis(dc);

      auto range = mStereoInd
         ? TrackList::SingletonRange(track)
         : TrackList::Channels(track);

      mProcStereo = range.size() > 1;

      // Get track min/max/rms in peak/rms mode
      if(mGain && (mNormalizeTo == kAmplitude || mNormalizeTo == kRMS))
      {
         size_t idx = 0;
         for(auto channel : range)
         {
            if(mNormalizeTo == kAmplitude)
            {
               if(!GetTrackMinMax(channel, mMin[idx], mMax[idx]))
                  return false;
            }
            else // RMS
            {
               if(!GetTrackRMS(channel, mRMS[idx]))
                  return false;
            }
            ++idx;
         }
      }
      // Skip analyze pass if it is not necessary.
      if(mNormalizeTo == kLoudness || dc)
      {
         mBlockSize = 0.4 * mCurRate; // 400 ms blocks
         mBlockOverlap = 0.1 * mCurRate; // 100 ms overlap
         if(!ProcessOne(range, true))
            // Processing failed -> abort
            return false;
      }
      else
         mSteps = 1;

      // Calculate normalization values the analysis results
      // mMin[], mMax[], mSum[], mRMS[], mLoudnessHist.
      if(mCount > 0 && dc)
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
      if(mNormalizeTo == kAmplitude)
      {
         extent = fmax(fabs(mMin[0]), fabs(mMax[0]));
      }
      else if(mNormalizeTo == kLoudness)
      {
         // EBU R128: z_i = mean square without root

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
      else // RMS
      {
         extent = mRMS[0];
      }

      if(mProcStereo)
      {
         if(mNormalizeTo == kAmplitude)
         {
            // Peak: use maximum of both tracks.
            float extent2;
            extent2 = fmax(fabs(mMin[1]), fabs(mMax[1]));;
            extent = fmax(extent, extent2);
         }
         else if(mNormalizeTo == kRMS)
         {
            // RMS: use average RMS
            extent = (extent + mRMS[1]) / 2.0;
         }
         // else: mNormalizeTo == kLoudness : do nothing (this is already handled in histogram)
      }

      if( (extent > 0) && mGain )
      {
         mMult = ratio / extent;

         if(mNormalizeTo == kLoudness)
         {
            // Target half the LUFS value if mono (or independent processed stereo)
            // shall be treated as dual mono.
            if(range.size() == 1 && (mDualMono || track->GetChannel() != Track::MonoChannel))
               mMult /= 2.0;

            // LUFS are related to square values so the multiplier must be the root.
            mMult = sqrt(mMult);
         }
      }
      else
         mMult = 1.0;

      mProgressMsg =
         topMsg + wxString::Format( _("Processing: %s"), trackName );

      if(!ProcessOne(range, false))
         // Processing failed -> abort
         return false;
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
               // Now make the checkbox.
               wxString checkboxLabel = mIsLoudness
                                      ? _("Normalize")
                                      : _("Normalize peak amplitude to");

               mGainCheckBox = S.AddCheckBox(checkboxLabel,
                                             mGain ? wxT("true") : wxT("false"));
               mGainCheckBox->SetValidator(wxGenericValidator(&mGain));
               mGainCheckBox->SetMinSize( mGainCheckBox->GetSize());

               if(mIsLoudness)
               {
                  auto targetChoices = LocalizedStrings(kNormalizeTargetStrings, nAlgos);
                  mNormalizeToCtl = S.AddChoice(wxEmptyString, wxT(""), &targetChoices);
                  mNormalizeToCtl->SetValidator(wxGenericValidator(&mNormalizeTo));
                  S.AddVariableText(_("to"), false,
                                    wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
               }

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

            mStereoIndCheckBox = S.AddCheckBox(_("Normalize stereo channels independently"),
                                               mStereoInd ? wxT("true") : wxT("false"));
            mStereoIndCheckBox->SetValidator(wxGenericValidator(&mStereoInd));

            if(mIsLoudness)
            {
               mDualMonoCheckBox = S.AddCheckBox(_("Treat mono as dual-mono (recommended)"),
                                                 mDualMono ? wxT("true") : wxT("false"));
               mDualMonoCheckBox->SetValidator(wxGenericValidator(&mDualMono));
            }
         }
         S.EndVerticalLay();
      }
      S.EndMultiColumn();
   }
   S.EndVerticalLay();
   // To ensure that the UpdateUI on creation sets the prompts correctly.
   mGUINormalizeTo = !mNormalizeTo;
}

bool EffectNormalize::TransferDataToWindow()
{
   // Force mNormalizeTo to kAmplitude if simple GUI is used, just in case.
   if(mIsLoudness == false)
      mNormalizeTo = kAmplitude;
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
   // Force mNormalizeTo to kAmplitude if simple GUI is used, just in case.
   if(mIsLoudness == false)
      mNormalizeTo = kAmplitude;

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

bool EffectNormalize::GetTrackRMS(WaveTrack* track, float& rms)
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

   // set mRMS.  No progress bar here as it's fast.
   float _rms = track->GetRMS(mCurT0, mCurT1); // may throw
   rms = _rms;
   return true;
}

/// Get required buffer size for the largest whole track and allocate buffers.
/// This reduces the amount of allocations required.
void EffectNormalize::AllocBuffers()
{
   mTrackBufferCapacity = 0;
   bool stereoTrackFound = false;
   double maxSampleRate = 0;
   mProcStereo = false;

   for(auto track : mOutputTracks->Selected<WaveTrack>() + &Track::Any)
   {
      mTrackBufferCapacity = std::max(mTrackBufferCapacity, track->GetMaxBlockSize());
      maxSampleRate = std::max(maxSampleRate, track->GetRate());

      // There is a stereo track
      if(track->IsLeader())
         stereoTrackFound = true;
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
bool EffectNormalize::ProcessOne(TrackIterRange<WaveTrack> range, bool analyse)
{
   WaveTrack* track = *range.begin();

   //Transform the marker timepoints to samples
   auto start = track->TimeToLongSamples(mCurT0);
   auto end   = track->TimeToLongSamples(mCurT1);

   //Get the length of the buffer (as double). len is
   //used simply to calculate a progress meter, so it is easier
   //to make it a double now than it is to do it later
   mTrackLen = (end - start).as_double();

   // Abort if the right marker is not to the right of the left marker
   if (mCurT1 <= mCurT0)
      return false;

   //Go through the track one buffer at a time. s counts which
   //sample the current buffer starts at.
   auto s = start;
   while (s < end)
   {
      //Get a block of samples (smaller than the size of the buffer)
      //Adjust the block size if it is the final block in the track
      auto blockLen = limitSampleBufferSize(
         track->GetBestBlockSize(s),
         mTrackBufferCapacity
      );

      const size_t remainingLen = (end - s).as_size_t();
      blockLen = blockLen > remainingLen ? remainingLen : blockLen;
      if(!LoadBufferBlock(range, s, blockLen))
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
         StoreBufferBlock(range, s, blockLen);

      //Increment s one blockfull of samples
      s += blockLen;
   }

   //Return true because the effect processing succeeded ... unless cancelled
   return true;
}

bool EffectNormalize::LoadBufferBlock(TrackIterRange<WaveTrack> range,
                                      sampleCount pos, size_t len)
{
   sampleCount read_size = -1;
   // Get the samples from the track and put them in the buffer
   int idx = 0;
   for(auto channel : range)
   {
      channel->Get((samplePtr) mTrackBuffer[idx].get(), floatSample, pos, len,
                   fillZero, true, &read_size);
      mTrackBufferLen = read_size.as_size_t();

      // Fail if we read different sample count from stereo pair tracks.
      // Ignore this check during first iteration (read_size == -1).
      if(read_size.as_size_t() != mTrackBufferLen && read_size != -1)
         return false;

      ++idx;
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

void EffectNormalize::StoreBufferBlock(TrackIterRange<WaveTrack> range,
                                       sampleCount pos, size_t len)
{
   int idx = 0;
   for(auto channel : range)
   {
      // Copy the newly-changed samples back onto the track.
      channel->Set((samplePtr) mTrackBuffer[idx].get(), floatSample, pos, len);
      ++idx;
   }
}

void EffectNormalize::InitTrackAnalysis(bool dc)
{
   mSum[0]   = 0.0; // dc offset inits
   mSum[1]   = 0.0;
   mCount = 0;

   mBlockRingPos = 0;
   mBlockRingSize = 0;
   memset(mLoudnessHist.get(), 0, HIST_BIN_COUNT*sizeof(long int));

   // Normalize ?
   if(mGain)
   {
      if(mNormalizeTo == kLoudness)
      {
         CalcEBUR128HPF(mCurRate);
         CalcEBUR128HSF(mCurRate);
         if(dc)
            mOp = ANALYSE_LOUDNESS_DC;
         else
         {
            mOp = ANALYSE_LOUDNESS;
            mOffset[0] = 0.0;
            mOffset[1] = 0.0;
         }
      }
      else if(dc)
         mOp = ANALYSE_DC;
   }
   // Just remove DC ?
   else if(dc)
   {
      mMin[0] = -1.0, mMax[0] = 1.0;   // sensible defaults?
      mMin[1] = -1.0, mMax[1] = 1.0;
      mOp = ANALYSE_DC;
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
      mWarning->SetLabel(_("(Maximum 0dB)"));
      EnableApply(false);
      return;
   }
   mWarning->SetLabel(wxT(""));

   // Changing the prompts causes an unwanted UpdateUI event.  
   // This 'guard' stops that becoming an infinite recursion.
   if (mNormalizeTo != mGUINormalizeTo)
   {
      mGUINormalizeTo = mNormalizeTo;
      if (mNormalizeTo == kAmplitude)
      {
         FloatingPointValidator<double> vldLevel(2, &mPeakLevel, NumValidatorStyle::ONE_TRAILING_ZERO);
         vldLevel.SetRange(MIN_PeakLevel, MAX_PeakLevel);
         mLevelTextCtrl->SetValidator(vldLevel);
         mLevelTextCtrl->SetName(_("Peak amplitude dB"));
         mLevelTextCtrl->SetValue(wxString::FromDouble(mPeakLevel));
         mLeveldB->SetLabel(_("dB"));
      }
      else if(mNormalizeTo == kLoudness)
      {
         FloatingPointValidator<double> vldLevel(2, &mLUFSLevel, NumValidatorStyle::ONE_TRAILING_ZERO);
         vldLevel.SetRange(MIN_LUFSLevel, MAX_LUFSLevel);
         mLevelTextCtrl->SetValidator(vldLevel);
         /* i18n-hint: LUFS is a particular method for measuring loudnesss */
         mLevelTextCtrl->SetName(_("Loudness LUFS"));
         mLevelTextCtrl->SetValue(wxString::FromDouble(mLUFSLevel));
         /* i18n-hint: LUFS is a particular method for measuring loudnesss */
         mLeveldB->SetLabel(_("LUFS"));
      }
      else // RMS
      {
         FloatingPointValidator<double> vldLevel(2, &mRMSLevel, NumValidatorStyle::ONE_TRAILING_ZERO);
         vldLevel.SetRange(MIN_RMSLevel, MAX_RMSLevel);
         mLevelTextCtrl->SetValidator(vldLevel);
         mLevelTextCtrl->SetName(_("RMS dB"));
         mLevelTextCtrl->SetValue(wxString::FromDouble(mRMSLevel));
         mLeveldB->SetLabel(_("dB"));
      }
   }

   // Disallow level stuff if not normalizing
   mLevelTextCtrl->Enable(mGain);
   mLeveldB->Enable(mGain);
   mStereoIndCheckBox->Enable(mGain);
   if(mIsLoudness)
   {
      mDualMonoCheckBox->Enable(mGain && mNormalizeTo == kLoudness);
      mNormalizeToCtl->Enable(mGain);

      // Disallow DC removal in RMS mode because this is impossible with a
      // single analyze pass due to center 2ab term of the binomial formula.
      // Loudness is immune to this effect because is has highpass characteristic.
      mDCCheckBox->Enable(mNormalizeTo != kRMS);
   }

   // Disallow OK/Preview if doing nothing
   EnableApply(mGain || mDC);
}
