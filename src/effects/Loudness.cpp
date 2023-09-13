/**********************************************************************

  Audacity: A Digital Audio Editor

  Loudness.cpp

  Max Maisel

*******************************************************************//**

\class EffectLoudness
\brief An Effect to bring the loudness level up to a chosen level.

*//*******************************************************************/
#include "Loudness.h"
#include "EBUR128.h"
#include "EffectEditor.h"
#include "EffectOutputTracks.h"

#include <math.h>

#include <wx/simplebook.h>
#include <wx/valgen.h>

#include "Internat.h"
#include "Prefs.h"
#include "../ProjectFileManager.h"
#include "ShuttleGui.h"
#include "WaveTrack.h"
#include "../widgets/valnum.h"
#include "ProgressDialog.h"

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
   const float ratio = DB_TO_LINEAR(
      (mNormalizeTo == kLoudness)
         ?  // LU use 10*log10(...) instead of 20*log10(...)
            // so multiply level by 2
            std::clamp<double>(mLUFSLevel * 2, LUFSLevel.min, LUFSLevel.max)
         :  // RMS
            std::clamp<double>(mRMSLevel, RMSLevel.min, RMSLevel.max)
   );

   // Iterate over each track
   EffectOutputTracks outputs { *mTracks, {{ mT0, mT1 }} };
   bool bGoodResult = true;
   auto topMsg = XO("Normalizing Loudness...\n");

   AllocBuffers(outputs.Get());
   mProgressVal = 0;

   for (auto pTrack : outputs.Get().Selected<WaveTrack>()) {
      // Get start and end times from track
      double trackStart = pTrack->GetStartTime();
      double trackEnd = pTrack->GetEndTime();

      // Set the current bounds to whichever left marker is
      // greater and whichever right marker is less:
      const double curT0 = std::max(trackStart, mT0);
      const double curT1 = std::min(trackEnd, mT1);

      // Get the track rate
      mCurRate = pTrack->GetRate();

      wxString msg;
      auto trackName = pTrack->GetName();
      // This affects only the progress indicator update during ProcessOne
      mSteps = (mNormalizeTo == kLoudness) ? 2 : 1;

      mProgressMsg =
         topMsg + XO("Analyzing: %s").Format(trackName);

      const auto channels = pTrack->Channels();
      auto nChannels = mStereoInd ? 1 : channels.size();
      mProcStereo = nChannels > 1;

      const auto processOne = [&](WaveChannel &track){
         std::optional<EBUR128> loudnessProcessor;
         float RMS[2];

         if (mNormalizeTo == kLoudness) {
            loudnessProcessor.emplace(mCurRate, nChannels);
            if (!ProcessOne(track, nChannels,
               curT0, curT1, 0, &*loudnessProcessor))
               // Processing failed -> abort
               return false;
         }
         else {
            // RMS
            if (mProcStereo) {
               size_t idx = 0;
               for (const auto pChannel : channels) {
                  if (!GetTrackRMS(*pChannel, curT0, curT1, RMS[idx]))
                     return false;
                  ++idx;
               }
            }
            else {
               if (!GetTrackRMS(track, curT0, curT1, RMS[0]))
                  return false;
            }
         }

         // Calculate normalization values the analysis results
         float extent;
         if (mNormalizeTo == kLoudness)
            extent = loudnessProcessor->IntegrativeLoudness();
         else {
            // RMS
            extent = RMS[0];
            if (mProcStereo)
               // RMS: use average RMS, average must be calculated in quadratic
               // domain.
               extent = sqrt((RMS[0] * RMS[0] + RMS[1] * RMS[1]) / 2.0);
         }

         if (extent == 0.0) {
            FreeBuffers();
            return false;
         }
         float mult = ratio / extent;

         if (mNormalizeTo == kLoudness) {
            // Target half the LUFS value if mono (or independent processed
            // stereo) shall be treated as dual mono.
            if (nChannels == 1 &&
               (mDualMono || !IsMono(track)))
               mult /= 2.0;

            // LUFS are related to square values so the multiplier must be the
            // xroot.
            mult = sqrt(mult);
         }

         mProgressMsg = topMsg + XO("Processing: %s").Format( trackName );
         if (!ProcessOne(track, nChannels, curT0, curT1, mult, nullptr)) {
            // Processing failed -> abort
            return false;
         }
         return true;
      };

      if (mStereoInd) {
         for (const auto pChannel : channels)
            if (!(bGoodResult = processOne(*pChannel)))
               goto done;
      }
      else {
         if (!(bGoodResult = processOne(*pTrack)))
            break;
      }
   }
done:

   if (bGoodResult)
      outputs.Commit();

   FreeBuffers();
   return bGoodResult;
}

std::unique_ptr<EffectEditor> EffectLoudness::PopulateOrExchange(
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
void EffectLoudness::AllocBuffers(TrackList &outputs)
{
   mTrackBufferCapacity = 0;
   bool stereoTrackFound = false;
   double maxSampleRate = 0;
   mProcStereo = false;

   for (auto track : outputs.Selected<WaveTrack>() + &Track::Any) {
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

bool EffectLoudness::GetTrackRMS(WaveChannel &track,
   const double curT0, const double curT1, float &rms)
{
   // set mRMS.  No progress bar here as it's fast.
   float _rms = track.GetRMS(curT0, curT1); // may throw
   rms = _rms;
   return true;
}

/// ProcessOne() takes a track, transforms it to bunch of buffer-blocks,
/// and executes ProcessData, on it...
///  uses mMult to normalize a track.
///  mMult must be set before this is called
/// In analyse mode, it executes the selected analyse operation on it...
///  mMult does not have to be set before this is called
bool EffectLoudness::ProcessOne(WaveChannel &track, size_t nChannels,
   const double curT0, const double curT1, const float mult,
   EBUR128 *pLoudnessProcessor)
{
   // Transform the marker timepoints to samples
   auto start = track.TimeToLongSamples(curT0);
   auto end   = track.TimeToLongSamples(curT1);

   // Get the length of the buffer (as double). len is
   // used simply to calculate a progress meter, so it is easier
   // to make it a double now than it is to do it later
   mTrackLen = (end - start).as_double();

   // Abort if the right marker is not to the right of the left marker
   if (curT1 <= curT0)
      return false;

   // Go through the track one buffer at a time. s counts which
   // sample the current buffer starts at.
   auto s = start;
   while (s < end) {
      // Get a block of samples (smaller than the size of the buffer)
      // Adjust the block size if it is the final block in the track
      auto blockLen = limitSampleBufferSize(
         track.GetBestBlockSize(s),
         mTrackBufferCapacity);

      const size_t remainingLen = (end - s).as_size_t();
      blockLen = blockLen > remainingLen ? remainingLen : blockLen;
      LoadBufferBlock(track, nChannels, s, blockLen);

      // Process the buffer.
      if (pLoudnessProcessor) {
         if (!AnalyseBufferBlock(*pLoudnessProcessor))
            return false;
      }
      else {
         if (!ProcessBufferBlock(mult))
            return false;
         if (!StoreBufferBlock(track, nChannels, s, blockLen))
            return false;
      }

      // Increment s one blockfull of samples
      s += blockLen;
   }

   // Return true because the effect processing succeeded ... unless cancelled
   return true;
}

void EffectLoudness::LoadBufferBlock(WaveChannel &track, size_t nChannels,
   sampleCount pos, size_t len)
{
   size_t idx = 0;
   const auto getOne = [&](WaveChannel &channel) {
      // Get the samples from the track and put them in the buffer
      channel.GetFloats(mTrackBuffer[idx].get(), pos, len);
   };

   if (nChannels == 1)
      getOne(track);
   else
      for (const auto channel : track.GetTrack().Channels()) {
         getOne(*channel);
         ++idx;
      }
   mTrackBufferLen = len;
}

/// Calculates sample sum (for DC) and EBU R128 weighted square sum
/// (for loudness).
bool EffectLoudness::AnalyseBufferBlock(EBUR128 &loudnessProcessor)
{
   for(size_t i = 0; i < mTrackBufferLen; i++)
   {
      loudnessProcessor.ProcessSampleFromChannel(mTrackBuffer[0][i], 0);
      if (mProcStereo)
         loudnessProcessor.ProcessSampleFromChannel(mTrackBuffer[1][i], 1);
      loudnessProcessor.NextSample();
   }

   if (!UpdateProgress())
      return false;
   return true;
}

bool EffectLoudness::ProcessBufferBlock(const float mult)
{
   for(size_t i = 0; i < mTrackBufferLen; i++)
   {
      mTrackBuffer[0][i] = mTrackBuffer[0][i] * mult;
      if (mProcStereo)
         mTrackBuffer[1][i] = mTrackBuffer[1][i] * mult;
   }

   if(!UpdateProgress())
      return false;
   return true;
}

bool EffectLoudness::StoreBufferBlock(WaveChannel &track, size_t nChannels,
   sampleCount pos, size_t len)
{
   size_t idx = 0;
   const auto setOne = [&](WaveChannel &channel){
      // Copy the newly-changed samples back onto the track.
      return channel.Set(
         (samplePtr) mTrackBuffer[idx].get(), floatSample, pos, len);
   };

   if (nChannels == 1)
      return setOne(track);
   else {
      for (auto channel : track.GetTrack().Channels()) {
         if (!setOne(*channel))
            return false;
         ++idx;
      }
      return true;
   }
}

bool EffectLoudness::UpdateProgress()
{
   mProgressVal += (double(1 + mProcStereo) * double(mTrackBufferLen)
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
      EffectEditor::EnableApply(mUIParent, false);
      return;
   }
   mWarning->SetLabel(wxT(""));
   EffectEditor::EnableApply(mUIParent, true);
}
