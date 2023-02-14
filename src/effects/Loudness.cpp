/**********************************************************************

  Audacity: A Digital Audio Editor

  Loudness.cpp

  Max Maisel

*******************************************************************//**

\class EffectLoudness
\brief An Effect to bring the loudness level up to a chosen level.

*//*******************************************************************/



#include "Loudness.h"

#include <math.h>

#include <wx/simplebook.h>
#include <wx/valgen.h>

#include "Internat.h"
#include "Prefs.h"
#include "../ProjectFileManager.h"
#include "ShuttleGui.h"
#include "WaveTrack.h"
#include "valnum.h"
#include "../widgets/ProgressDialog.h"

#include "LoadEffects.h"

static const EnumValueSymbol kNormalizeTargetStrings[EffectLoudness::nAlgos] =
{
   { XO("perceived loudness") },
   { XO("RMS") }
};

const EffectParameterMethods& EffectLoudness::Parameters() const
{
   static CapturedParameters<EffectLoudness,
      StereoInd, LUFSLevel, RMSLevel, DualMono, NormalizeTo
   > parameters;
   return parameters;
}

BEGIN_EVENT_TABLE(EffectLoudness, wxEvtHandler)
   EVT_CHOICE(wxID_ANY, EffectLoudness::OnChoice)
   EVT_CHECKBOX(wxID_ANY, EffectLoudness::OnUpdateUI)
   EVT_TEXT(wxID_ANY, EffectLoudness::OnUpdateUI)
END_EVENT_TABLE()

const ComponentInterfaceSymbol EffectLoudness::Symbol
{ XO("Loudness Normalization") };

namespace{ BuiltinEffectsModule::Registration< EffectLoudness > reg; }

EffectLoudness::EffectLoudness()
{
   Parameters().Reset(*this);
   SetLinearEffectFlag(false);
}

EffectLoudness::~EffectLoudness()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectLoudness::GetSymbol() const
{
   return Symbol;
}

TranslatableString EffectLoudness::GetDescription() const
{
   return XO("Sets the loudness of one or more tracks");
}

ManualPageID EffectLoudness::ManualPage() const
{
   return L"Loudness_Normalization";
}

// EffectDefinitionInterface implementation

EffectType EffectLoudness::GetType() const
{
   return EffectTypeProcess;
}

// Effect implementation

bool EffectLoudness::Process(EffectInstance &, EffectSettings &)
{
   if(mNormalizeTo == kLoudness)
      // LU use 10*log10(...) instead of 20*log10(...)
      // so multiply level by 2 and use standard DB_TO_LINEAR macro.
      mRatio = DB_TO_LINEAR(std::clamp<double>(mLUFSLevel*2, LUFSLevel.min, LUFSLevel.max));
   else // RMS
      mRatio = DB_TO_LINEAR(std::clamp<double>(mRMSLevel, RMSLevel.min, RMSLevel.max));

   // Iterate over each track
   this->CopyInputTracks(); // Set up mOutputTracks.
   bool bGoodResult = true;
   auto topMsg = XO("Normalizing Loudness...\n");

   AllocBuffers();
   mProgressVal = 0;

   for(auto track : mOutputTracks->Selected<WaveTrack>()
       + (mStereoInd ? &Track::Any : &Track::IsLeader))
   {
      // Get start and end times from track
      // PRL: No accounting for multiple channels ?
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();

      // Set the current bounds to whichever left marker is
      // greater and whichever right marker is less:
      mCurT0 = mT0 < trackStart? trackStart: mT0;
      mCurT1 = mT1 > trackEnd? trackEnd: mT1;

      // Get the track rate
      mCurRate = track->GetRate();

      wxString msg;
      auto trackName = track->GetName();
      mSteps = 2;

      mProgressMsg =
         topMsg + XO("Analyzing: %s").Format( trackName );

      auto range = mStereoInd
         ? TrackList::SingletonRange(track)
         : TrackList::Channels(track);

      mProcStereo = range.size() > 1;

      if(mNormalizeTo == kLoudness)
      {
         mLoudnessProcessor.reset(safenew EBUR128(mCurRate, range.size()));
         mLoudnessProcessor->Initialize();
         if(!ProcessOne(range, true))
         {
            // Processing failed -> abort
            bGoodResult = false;
            break;
         }
      }
      else // RMS
      {
         size_t idx = 0;
         for(auto channel : range)
         {
            if(!GetTrackRMS(channel, mRMS[idx]))
            {
               bGoodResult = false;
               return false;
            }
            ++idx;
         }
         mSteps = 1;
      }

      // Calculate normalization values the analysis results
      float extent;
      if(mNormalizeTo == kLoudness)
         extent = mLoudnessProcessor->IntegrativeLoudness();
      else // RMS
      {
         extent = mRMS[0];
         if(mProcStereo)
            // RMS: use average RMS, average must be calculated in quadratic domain.
            extent = sqrt((mRMS[0] * mRMS[0] + mRMS[1] * mRMS[1]) / 2.0);
      }

      if(extent == 0.0)
      {
         mLoudnessProcessor.reset();
         FreeBuffers();
         return false;
      }
      mMult = mRatio / extent;

      if(mNormalizeTo == kLoudness)
      {
         // Target half the LUFS value if mono (or independent processed stereo)
         // shall be treated as dual mono.
         if(range.size() == 1 && (mDualMono || track->GetChannel() != Track::MonoChannel))
            mMult /= 2.0;

         // LUFS are related to square values so the multiplier must be the root.
         mMult = sqrt(mMult);
      }

      mProgressMsg = topMsg + XO("Processing: %s").Format( trackName );
      if(!ProcessOne(range, false))
      {
         // Processing failed -> abort
         bGoodResult = false;
         break;
      }
   }

   this->ReplaceProcessedTracks(bGoodResult);
   mLoudnessProcessor.reset();
   FreeBuffers();
   return bGoodResult;
}

std::unique_ptr<EffectUIValidator> EffectLoudness::PopulateOrExchange(
   ShuttleGui & S, EffectInstance &, EffectSettingsAccess &,
   const EffectOutputs *)
{
   mUIParent = S.GetParent();
   S.StartVerticalLay(0);
   {
      S.StartMultiColumn(2, wxALIGN_CENTER);
      {
         S.StartVerticalLay(false);
         {
            S.StartHorizontalLay(wxALIGN_LEFT, false);
            {
               S.AddVariableText(XO("&Normalize"), false,
                  wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);

               mChoice = S
                  .Validator<wxGenericValidator>( &mNormalizeTo )
                  .AddChoice( {},
                     Msgids(kNormalizeTargetStrings, nAlgos),
                     mNormalizeTo );
               S
                  .AddVariableText(XO("t&o"), false,
                     wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);

               // Use a notebook so we can have two controls but show only one
               // They target different variables with their validators
               mBook =
               S
                  .StartSimplebook();
               {
                  S.StartNotebookPage({});
                  {
                     S.StartHorizontalLay(wxALIGN_LEFT, false);
                     {
                        S
                           /* i18n-hint: LUFS is a particular method for measuring loudnesss */
                           .Name( XO("Loudness LUFS") )
                           .Validator<FloatingPointValidator<double>>(
                              2, &mLUFSLevel,
                              NumValidatorStyle::ONE_TRAILING_ZERO,
                              LUFSLevel.min, LUFSLevel.max )
                           .AddTextBox( {}, L"", 10);

                        /* i18n-hint: LUFS is a particular method for measuring loudnesss */
                        S
                           .AddVariableText(XO("LUFS"), false,
                              wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
                     }
                     S.EndHorizontalLay();
                  }
                  S.EndNotebookPage();

                  S.StartNotebookPage({});
                  {
                     S.StartHorizontalLay(wxALIGN_LEFT, false);
                     {
                        S
                           .Name( XO("RMS dB") )
                           .Validator<FloatingPointValidator<double>>(
                              2, &mRMSLevel,
                              NumValidatorStyle::ONE_TRAILING_ZERO,
                              RMSLevel.min, RMSLevel.max )
                           .AddTextBox( {}, L"", 10);

                        S
                           .AddVariableText(XO("dB"), false,
                              wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
                     }
                     S.EndHorizontalLay();
                  }
                  S.EndNotebookPage();
               }
               S.EndSimplebook();

               mWarning =
               S
                  .AddVariableText( {}, false,
                     wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
            }
            S.EndHorizontalLay();

            mStereoIndCheckBox = S
               .Validator<wxGenericValidator>( &mStereoInd )
               .AddCheckBox(XXO("Normalize &stereo channels independently"),
                  mStereoInd );

            mDualMonoCheckBox = S
               .Validator<wxGenericValidator>( &mDualMono )
               .AddCheckBox(XXO("&Treat mono as dual-mono (recommended)"),
                  mDualMono );
         }
         S.EndVerticalLay();
      }
      S.EndMultiColumn();
   }
   S.EndVerticalLay();
   return nullptr;
}

bool EffectLoudness::TransferDataToWindow(const EffectSettings &)
{
   if (!mUIParent->TransferDataToWindow())
   {
      return false;
   }

   // adjust controls which depend on mchoice
   wxCommandEvent dummy;
   OnChoice(dummy);
   return true;
}

bool EffectLoudness::TransferDataFromWindow(EffectSettings &)
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }
   return true;
}

// EffectLoudness implementation

/// Get required buffer size for the largest whole track and allocate buffers.
/// This reduces the amount of allocations required.
void EffectLoudness::AllocBuffers()
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

   // Initiate a processing buffer. This buffer will (most likely)
   // be shorter than the length of the track being processed.
   mTrackBuffer[0].reinit(mTrackBufferCapacity);

   if(!mStereoInd && stereoTrackFound)
      mTrackBuffer[1].reinit(mTrackBufferCapacity);
}

void EffectLoudness::FreeBuffers()
{
   mTrackBuffer[0].reset();
   mTrackBuffer[1].reset();
}

bool EffectLoudness::GetTrackRMS(WaveTrack* track, float& rms)
{
   // set mRMS.  No progress bar here as it's fast.
   float _rms = track->GetRMS(mCurT0, mCurT1); // may throw
   rms = _rms;
   return true;
}

/// ProcessOne() takes a track, transforms it to bunch of buffer-blocks,
/// and executes ProcessData, on it...
///  uses mMult to normalize a track.
///  mMult must be set before this is called
/// In analyse mode, it executes the selected analyse operation on it...
///  mMult does not have to be set before this is called
bool EffectLoudness::ProcessOne(TrackIterRange<WaveTrack> range, bool analyse)
{
   WaveTrack* track = *range.begin();

   // Transform the marker timepoints to samples
   auto start = track->TimeToLongSamples(mCurT0);
   auto end   = track->TimeToLongSamples(mCurT1);

   // Get the length of the buffer (as double). len is
   // used simply to calculate a progress meter, so it is easier
   // to make it a double now than it is to do it later
   mTrackLen = (end - start).as_double();

   // Abort if the right marker is not to the right of the left marker
   if(mCurT1 <= mCurT0)
      return false;

   // Go through the track one buffer at a time. s counts which
   // sample the current buffer starts at.
   auto s = start;
   while(s < end)
   {
      // Get a block of samples (smaller than the size of the buffer)
      // Adjust the block size if it is the final block in the track
      auto blockLen = limitSampleBufferSize(
         track->GetBestBlockSize(s),
         mTrackBufferCapacity);

      const size_t remainingLen = (end - s).as_size_t();
      blockLen = blockLen > remainingLen ? remainingLen : blockLen;
      LoadBufferBlock(range, s, blockLen);

      // Process the buffer.
      if(analyse)
      {
         if(!AnalyseBufferBlock())
            return false;
      }
      else
      {
         if(!ProcessBufferBlock())
            return false;
         StoreBufferBlock(range, s, blockLen);
      }

      // Increment s one blockfull of samples
      s += blockLen;
   }

   // Return true because the effect processing succeeded ... unless cancelled
   return true;
}

void EffectLoudness::LoadBufferBlock(TrackIterRange<WaveTrack> range,
                                     sampleCount pos, size_t len)
{
   // Get the samples from the track and put them in the buffer
   int idx = 0;
   for(auto channel : range)
   {
      channel->GetFloats(mTrackBuffer[idx].get(), pos, len );
      ++idx;
   }
   mTrackBufferLen = len;
}

/// Calculates sample sum (for DC) and EBU R128 weighted square sum
/// (for loudness).
bool EffectLoudness::AnalyseBufferBlock()
{
   for(size_t i = 0; i < mTrackBufferLen; i++)
   {
      mLoudnessProcessor->ProcessSampleFromChannel(mTrackBuffer[0][i], 0);
      if(mProcStereo)
         mLoudnessProcessor->ProcessSampleFromChannel(mTrackBuffer[1][i], 1);
      mLoudnessProcessor->NextSample();
   }

   if(!UpdateProgress())
      return false;
   return true;
}

bool EffectLoudness::ProcessBufferBlock()
{
   for(size_t i = 0; i < mTrackBufferLen; i++)
   {
      mTrackBuffer[0][i] = mTrackBuffer[0][i] * mMult;
      if(mProcStereo)
         mTrackBuffer[1][i] = mTrackBuffer[1][i] * mMult;
   }

   if(!UpdateProgress())
      return false;
   return true;
}

void EffectLoudness::StoreBufferBlock(TrackIterRange<WaveTrack> range,
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

bool EffectLoudness::UpdateProgress()
{
   mProgressVal += (double(1+mProcStereo) * double(mTrackBufferLen)
                 / (double(GetNumWaveTracks()) * double(mSteps) * mTrackLen));
   return !TotalProgress(mProgressVal, mProgressMsg);
}

void EffectLoudness::OnChoice(wxCommandEvent & WXUNUSED(evt))
{
   mChoice->GetValidator()->TransferFromWindow();
   mBook->SetSelection( mNormalizeTo );
   UpdateUI();
   mDualMonoCheckBox->Enable(mNormalizeTo == kLoudness);
}

void EffectLoudness::OnUpdateUI(wxCommandEvent & WXUNUSED(evt))
{
   UpdateUI();
}

void EffectLoudness::UpdateUI()
{
   if (!mUIParent->TransferDataFromWindow())
   {
      mWarning->SetLabel(_("(Maximum 0dB)"));
      // TODO: recalculate layout here
      EffectUIValidator::EnableApply(mUIParent, false);
      return;
   }
   mWarning->SetLabel(wxT(""));
   EffectUIValidator::EnableApply(mUIParent, true);
}
