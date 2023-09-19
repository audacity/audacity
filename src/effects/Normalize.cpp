/**********************************************************************

  Audacity: A Digital Audio Editor

  Normalize.cpp

  Dominic Mazzoni
  Vaughan Johnson (Preview)

*******************************************************************//**

\class EffectNormalize
\brief An Effect to bring the peak level up to a chosen level.

*//*******************************************************************/
#include "Normalize.h"
#include "EffectEditor.h"
#include "EffectOutputTracks.h"
#include "LoadEffects.h"

#include <math.h>

#include <wx/checkbox.h>
#include <wx/stattext.h>
#include <wx/valgen.h>

#include "Prefs.h"
#include "../ProjectFileManager.h"
#include "ShuttleGui.h"
#include "WaveTrack.h"
#include "../widgets/valnum.h"
#include "ProgressDialog.h"

const EffectParameterMethods& EffectNormalize::Parameters() const
{
   static CapturedParameters<EffectNormalize,
      PeakLevel, ApplyGain, RemoveDC, StereoInd
   > parameters;
   return parameters;
}

const ComponentInterfaceSymbol EffectNormalize::Symbol
{ XO("Normalize") };

namespace{ BuiltinEffectsModule::Registration< EffectNormalize > reg; }

BEGIN_EVENT_TABLE(EffectNormalize, wxEvtHandler)
   EVT_CHECKBOX(wxID_ANY, EffectNormalize::OnUpdateUI)
   EVT_TEXT(wxID_ANY, EffectNormalize::OnUpdateUI)
END_EVENT_TABLE()

EffectNormalize::EffectNormalize()
{
   Parameters().Reset(*this);
   SetLinearEffectFlag(false);
}

EffectNormalize::~EffectNormalize()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectNormalize::GetSymbol() const
{
   return Symbol;
}

TranslatableString EffectNormalize::GetDescription() const
{
   return XO("Sets the peak amplitude of one or more tracks");
}

ManualPageID EffectNormalize::ManualPage() const
{
   return L"Normalize";
}

// EffectDefinitionInterface implementation

EffectType EffectNormalize::GetType() const
{
   return EffectTypeProcess;
}

// Effect implementation

bool EffectNormalize::CheckWhetherSkipEffect(const EffectSettings &) const
{
   return ((mGain == false) && (mDC == false));
}

bool EffectNormalize::Process(EffectInstance &, EffectSettings &)
{
   if (mGain == false && mDC == false)
      return true;

   float ratio;
   if( mGain )
   {
      // same value used for all tracks
      ratio = DB_TO_LINEAR(std::clamp<double>(mPeakLevel, PeakLevel.min, PeakLevel.max));
   }
   else {
      ratio = 1.0;
   }

   //Iterate over each track
   EffectOutputTracks outputs { *mTracks, {{ mT0, mT1 }} };
   bool bGoodResult = true;
   double progress = 0;
   TranslatableString topMsg;
   if(mDC && mGain)
      topMsg = XO("Removing DC offset and Normalizing...\n");
   else if(mDC && !mGain)
      topMsg = XO("Removing DC offset...\n");
   else if(!mDC && mGain)
      topMsg = XO("Normalizing without removing DC offset...\n");
   else if(!mDC && !mGain)
      topMsg = XO("Not doing anything...\n");   // shouldn't get here

   for (auto track : outputs.Get().Selected<WaveTrack>()) {
      // Get start and end times from track
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();

      // Set the current bounds to whichever left marker is
      // greater and whichever right marker is less:
      mCurT0 = std::max(trackStart, mT0);
      mCurT1 = std::min(trackEnd, mT1);

      // Process only if the right marker is to the right of the left marker
      if (mCurT1 > mCurT0) {
         wxString trackName = track->GetName();

         std::vector<float> extents;
         float maxExtent{ std::numeric_limits<float>::lowest() };
         std::vector<float> offsets;

         const auto channels = track->Channels();
         // mono or 'stereo tracks independently'
         const bool oneChannel = (channels.size() == 1 || mStereoInd);
         auto msg = oneChannel
            ? topMsg +
               XO("Analyzing: %s").Format(trackName)
            : topMsg +
               // TODO: more-than-two-channels-message
               XO("Analyzing first track of stereo pair: %s").Format(trackName);

         const auto progressReport = [&](double fraction){
            return !TotalProgress(
               (progress + fraction / double(2 * GetNumWaveTracks())), msg);
         };

         // Analysis loop over channels collects offsets and extent
         for (auto channel : channels) {
            float offset = 0;
            float extent = 0;
            bGoodResult = AnalyseTrack(*channel, progressReport, mGain, mDC,
               mCurT0, mCurT1, offset, extent);
            if (!bGoodResult)
               goto break2;
            progress += 1.0 / double(2 * GetNumWaveTracks());
            extents.push_back(extent);
            maxExtent = std::max(maxExtent, extent);
            offsets.push_back(offset);
            // TODO: more-than-two-channels-message
            if (!oneChannel)
               msg = topMsg +
                  XO("Analyzing second track of stereo pair: %s")
                     .Format(trackName);
         }

         if (oneChannel) {
            if (TrackList::NChannels(*track) == 1)
               // really mono
               msg = topMsg +
                  XO("Processing: %s").Format(trackName);
            else
               //'stereo tracks independently'
               // TODO: more-than-two-channels-message
               msg = topMsg +
                  XO("Processing stereo channels independently: %s")
                     .Format(trackName);
         }
         else
            msg = topMsg +
               // TODO: more-than-two-channels-message
               XO("Processing first track of stereo pair: %s")
                  .Format(trackName);

         // Use multiplier in the second, processing loop over channels
         auto pOffset = offsets.begin();
         auto pExtent = extents.begin();
         for (const auto channel : channels) {
            const auto extent = oneChannel ? *pExtent++: maxExtent;
            if ((extent > 0) && mGain)
               mMult = ratio / extent;
            else
               mMult = 1.0;
            if (false ==
                (bGoodResult = ProcessOne(*channel, msg, progress, *pOffset++)))
               goto break2;
            // TODO: more-than-two-channels-message
            msg = topMsg +
               XO("Processing second track of stereo pair: %s")
                  .Format(trackName);
         }
      }
   }

   break2:

   if (bGoodResult)
      outputs.Commit();

   return bGoodResult;
}

std::unique_ptr<EffectEditor> EffectNormalize::PopulateOrExchange(
   ShuttleGui & S, EffectInstance &, EffectSettingsAccess &,
   const EffectOutputs *)
{
   mUIParent = S.GetParent();
   mCreating = true;

   S.StartVerticalLay(0);
   {
      S.StartMultiColumn(2, wxALIGN_CENTER);
      {
         S.StartVerticalLay(false);
         {
            mDCCheckBox = S.Validator<wxGenericValidator>(&mDC)
               .AddCheckBox(XXO("&Remove DC offset (center on 0.0 vertically)"),
                                        mDC);

            S.StartHorizontalLay(wxALIGN_LEFT, false);
            {
               mGainCheckBox = S
                  .MinSize()
                  .Validator<wxGenericValidator>(&mGain)
                  .AddCheckBox(XXO("&Normalize peak amplitude to   "),
                     mGain);

               mLevelTextCtrl = S
                  .Name(XO("Peak amplitude dB"))
                  .Validator<FloatingPointValidator<double>>(
                     2,
                     &mPeakLevel,
                     NumValidatorStyle::ONE_TRAILING_ZERO,
                     PeakLevel.min,
                     PeakLevel.max )
                  .AddTextBox( {}, L"", 10);
               mLeveldB = S.AddVariableText(XO("dB"), false,
                  wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
               mWarning = S.AddVariableText( {}, false,
                  wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
            }
            S.EndHorizontalLay();

            mStereoIndCheckBox = S
               .Validator<wxGenericValidator>(&mStereoInd)
               .AddCheckBox(XXO("N&ormalize stereo channels independently"),
                                               mStereoInd);
         }
         S.EndVerticalLay();
      }
      S.EndMultiColumn();
   }
   S.EndVerticalLay();
   mCreating = false;
   return nullptr;
}

bool EffectNormalize::TransferDataToWindow(const EffectSettings &)
{
   if (!mUIParent->TransferDataToWindow())
   {
      return false;
   }

   UpdateUI();

   return true;
}

bool EffectNormalize::TransferDataFromWindow(EffectSettings &)
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }

   return true;
}

// EffectNormalize implementation

bool EffectNormalize::AnalyseTrack(const WaveChannel &track,
   const ProgressReport &report,
   const bool gain, const bool dc, const double curT0, const double curT1,
   float &offset, float &extent)
{
   bool result = true;
   float min, max;
   if (gain) {
      // set mMin, mMax.  No progress bar here as it's fast.
      auto pair = track.GetMinMax(curT0, curT1); // may throw
      min = pair.first, max = pair.second;

      if (dc) {
         result = AnalyseTrackData(track, report, curT0, curT1, offset);
         min += offset;
         max += offset;
      }
   }
   else if (dc) {
      min = -1.0, max = 1.0;   // sensible defaults?
      result = AnalyseTrackData(track, report, curT0, curT1, offset);
      min += offset;
      max += offset;
   }
   else {
      wxFAIL_MSG("Analysing Track when nothing to do!");
      min = -1.0, max = 1.0;   // sensible defaults?
      offset = 0.0;
   }
   extent = fmax(fabs(min), fabs(max));
   return result;
}

//AnalyseTrackData() takes a track, transforms it to bunch of buffer-blocks,
//and executes selected AnalyseOperation on it...
bool EffectNormalize::AnalyseTrackData(const WaveChannel &track,
   const ProgressReport &report, const double curT0, const double curT1,
   float &offset)
{
   bool rc = true;

   //Transform the marker timepoints to samples
   auto start = track.TimeToLongSamples(curT0);
   auto end = track.TimeToLongSamples(curT1);

   //Get the length of the buffer (as double). len is
   //used simply to calculate a progress meter, so it is easier
   //to make it a double now than it is to do it later
   auto len = (end - start).as_double();

   //Initiate a processing buffer.  This buffer will (most likely)
   //be shorter than the length of the track being processed.
   Floats buffer{ track.GetMaxBlockSize() };

   double sum = 0.0; // dc offset inits

   sampleCount blockSamples;
   sampleCount totalSamples = 0;

   //Go through the track one buffer at a time. s counts which
   //sample the current buffer starts at.
   auto s = start;
   while (s < end) {
      //Get a block of samples (smaller than the size of the buffer)
      //Adjust the block size if it is the final block in the track
      const auto block = limitSampleBufferSize(
         track.GetBestBlockSize(s),
         end - s
      );

      //Get the samples from the track and put them in the buffer
      track.GetFloats(
         buffer.get(), s, block, FillFormat::fillZero, true, &blockSamples);
      totalSamples += blockSamples;

      //Process the buffer.
      sum = AnalyseDataDC(buffer.get(), block, sum);

      //Increment s one blockfull of samples
      s += block;

      //Update the Progress meter
      if (!report((s - start).as_double() / len)) {
         rc = false; //lda .. break, not return, so that buffer is deleted
         break;
      }
   }
   if (totalSamples > 0)
      // calculate actual offset (amount that needs to be added on)
      offset = -sum / totalSamples.as_double();
   else
      offset = 0.0;

   //Return true because the effect processing succeeded ... unless cancelled
   return rc;
}

//ProcessOne() takes a track, transforms it to bunch of buffer-blocks,
//and executes ProcessData, on it...
// uses mMult and offset to normalize a track.
// mMult must be set before this is called
bool EffectNormalize::ProcessOne(WaveChannel &track,
   const TranslatableString &msg, double &progress, float offset)
{
   bool rc = true;

   //Transform the marker timepoints to samples
   auto start = track.TimeToLongSamples(mCurT0);
   auto end = track.TimeToLongSamples(mCurT1);

   //Get the length of the buffer (as double). len is
   //used simply to calculate a progress meter, so it is easier
   //to make it a double now than it is to do it later
   auto len = (end - start).as_double();

   //Initiate a processing buffer.  This buffer will (most likely)
   //be shorter than the length of the track being processed.
   Floats buffer{ track.GetMaxBlockSize() };

   //Go through the track one buffer at a time. s counts which
   //sample the current buffer starts at.
   auto s = start;
   while (s < end) {
      //Get a block of samples (smaller than the size of the buffer)
      //Adjust the block size if it is the final block in the track
      const auto block = limitSampleBufferSize(
         track.GetBestBlockSize(s),
         end - s
      );

      //Get the samples from the track and put them in the buffer
      track.GetFloats(buffer.get(), s, block);

      //Process the buffer.
      ProcessData(buffer.get(), block, offset);

      //Copy the newly-changed samples back onto the track.
      if (!track.Set((samplePtr) buffer.get(), floatSample, s, block)) {
         rc = false;
         break;
      }

      //Increment s one blockfull of samples
      s += block;

      //Update the Progress meter
      if (TotalProgress(progress +
                        ((s - start).as_double() / len)/double(2*GetNumWaveTracks()), msg)) {
         rc = false; //lda .. break, not return, so that buffer is deleted
         break;
      }
   }
   progress += 1.0/double(2*GetNumWaveTracks());

   //Return true because the effect processing succeeded ... unless cancelled
   return rc;
}

/// @see AnalyseDataLoudnessDC
double EffectNormalize::AnalyseDataDC(float *buffer, size_t len, double sum)
{
   for(decltype(len) i = 0; i < len; i++)
      sum += (double)buffer[i];
   return sum;
}

void EffectNormalize::ProcessData(float *buffer, size_t len, float offset)
{
   for(decltype(len) i = 0; i < len; i++) {
      float adjFrame = (buffer[i] + offset) * mMult;
      buffer[i] = adjFrame;
   }
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
      EffectEditor::EnableApply(mUIParent, false);
      return;
   }
   mWarning->SetLabel(wxT(""));

   // Disallow level stuff if not normalizing
   mLevelTextCtrl->Enable(mGain);
   mLeveldB->Enable(mGain);
   mStereoIndCheckBox->Enable(mGain);

   // Disallow OK/Preview if doing nothing
   EffectEditor::EnableApply(mUIParent, mGain || mDC);
}
