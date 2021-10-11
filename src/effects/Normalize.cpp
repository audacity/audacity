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
#include "LoadEffects.h"

#include <math.h>

#include <wx/checkbox.h>
#include <wx/intl.h>
#include <wx/stattext.h>
#include <wx/valgen.h>

#include "Prefs.h"
#include "../ProjectFileManager.h"
#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "../WaveTrack.h"
#include "../widgets/valnum.h"
#include "../widgets/ProgressDialog.h"

// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name         Type     Key                        Def      Min      Max   Scale
Param( PeakLevel,   double,  wxT("PeakLevel"),           -1.0,    -145.0,  0.0,  1  );
Param( RemoveDC,    bool,    wxT("RemoveDcOffset"),      true,    false,   true, 1  );
Param( ApplyGain,   bool,    wxT("ApplyGain"),           true,    false,   true, 1  );
Param( StereoInd,   bool,    wxT("StereoIndependent"),   false,   false,   true, 1  );

const ComponentInterfaceSymbol EffectNormalize::Symbol
{ XO("Normalize") };

namespace{ BuiltinEffectsModule::Registration< EffectNormalize > reg; }

BEGIN_EVENT_TABLE(EffectNormalize, wxEvtHandler)
   EVT_CHECKBOX(wxID_ANY, EffectNormalize::OnUpdateUI)
   EVT_TEXT(wxID_ANY, EffectNormalize::OnUpdateUI)
END_EVENT_TABLE()

EffectNormalize::EffectNormalize()
{
   mPeakLevel = DEF_PeakLevel;
   mDC = DEF_RemoveDC;
   mGain = DEF_ApplyGain;
   mStereoInd = DEF_StereoInd;

   SetLinearEffectFlag(false);
}

EffectNormalize::~EffectNormalize()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectNormalize::GetSymbol()
{
   return Symbol;
}

TranslatableString EffectNormalize::GetDescription()
{
   return XO("Sets the peak amplitude of one or more tracks");
}

ManualPageID EffectNormalize::ManualPage()
{
   return L"Normalize";
}

// EffectDefinitionInterface implementation

EffectType EffectNormalize::GetType()
{
   return EffectTypeProcess;
}

// EffectClientInterface implementation
bool EffectNormalize::DefineParams( ShuttleParams & S ){
   S.SHUTTLE_PARAM( mPeakLevel, PeakLevel );
   S.SHUTTLE_PARAM( mGain, ApplyGain );
   S.SHUTTLE_PARAM( mDC, RemoveDC );
   S.SHUTTLE_PARAM( mStereoInd, StereoInd );
   return true;
}

bool EffectNormalize::GetAutomationParameters(CommandParameters & parms)
{
   parms.Write(KEY_PeakLevel, mPeakLevel);
   parms.Write(KEY_ApplyGain, mGain);
   parms.Write(KEY_RemoveDC, mDC);
   parms.Write(KEY_StereoInd, mStereoInd);

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
      // same value used for all tracks
      ratio = DB_TO_LINEAR(TrapDouble(mPeakLevel, MIN_PeakLevel, MAX_PeakLevel));
   }
   else {
      ratio = 1.0;
   }

   //Iterate over each track
   this->CopyInputTracks(); // Set up mOutputTracks.
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

   for ( auto track : mOutputTracks->Selected< WaveTrack >()
            + ( mStereoInd ? &Track::Any : &Track::IsLeader ) ) {
      //Get start and end times from track
      // PRL:  No accounting for multiple channels?
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();

      //Set the current bounds to whichever left marker is
      //greater and whichever right marker is less:
      mCurT0 = mT0 < trackStart? trackStart: mT0;
      mCurT1 = mT1 > trackEnd? trackEnd: mT1;

      auto range = mStereoInd
         ? TrackList::SingletonRange(track)
         : TrackList::Channels(track);

      // Process only if the right marker is to the right of the left marker
      if (mCurT1 > mCurT0) {
         wxString trackName = track->GetName();

         float extent;
         // Will compute a maximum
         extent = std::numeric_limits<float>::lowest();
         std::vector<float> offsets;

         auto msg = (range.size() == 1)
            // mono or 'stereo tracks independently'
            ? topMsg +
               XO("Analyzing: %s").Format( trackName )
            : topMsg +
               // TODO: more-than-two-channels-message
               XO("Analyzing first track of stereo pair: %s").Format( trackName );
         
         // Analysis loop over channels collects offsets and extent
         for (auto channel : range) {
            float offset = 0;
            float extent2 = 0;
            bGoodResult =
               AnalyseTrack( channel, msg, progress, offset, extent2 );
            if ( ! bGoodResult )
               goto break2;
            extent = std::max( extent, extent2 );
            offsets.push_back(offset);
            // TODO: more-than-two-channels-message
            msg = topMsg +
               XO("Analyzing second track of stereo pair: %s").Format( trackName );
         }

         // Compute the multiplier using extent
         if( (extent > 0) && mGain ) {
            mMult = ratio / extent;
         }
         else
            mMult = 1.0;

         if (range.size() == 1) {
            if (TrackList::Channels(track).size() == 1)
               // really mono
               msg = topMsg +
                  XO("Processing: %s").Format( trackName );
            else
               //'stereo tracks independently'
               // TODO: more-than-two-channels-message
               msg = topMsg +
                  XO("Processing stereo channels independently: %s").Format( trackName );
         }
         else
            msg = topMsg +
               // TODO: more-than-two-channels-message
               XO("Processing first track of stereo pair: %s").Format( trackName );

         // Use multiplier in the second, processing loop over channels
         auto pOffset = offsets.begin();
         for (auto channel : range) {
            if (false ==
                (bGoodResult = ProcessOne(channel, msg, progress, *pOffset++)) )
               goto break2;
            // TODO: more-than-two-channels-message
            msg = topMsg +
               XO("Processing second track of stereo pair: %s").Format( trackName );
         }
      }
   }

   break2:

   this->ReplaceProcessedTracks(bGoodResult);
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
                     MIN_PeakLevel,
                     MAX_PeakLevel
                  )
                  .AddTextBox( {}, wxT(""), 10);
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

bool EffectNormalize::AnalyseTrack(const WaveTrack * track, const TranslatableString &msg,
                                   double &progress, float &offset, float &extent)
{
   bool result = true;
   float min, max;

   if(mGain)
   {
      // set mMin, mMax.  No progress bar here as it's fast.
      auto pair = track->GetMinMax(mCurT0, mCurT1); // may throw
      min = pair.first, max = pair.second;

      if(mDC)
      {
         result = AnalyseTrackData(track, msg, progress, offset);
         min += offset;
         max += offset;
      }
   }
   else if(mDC)
   {
      min = -1.0, max = 1.0;   // sensible defaults?
      result = AnalyseTrackData(track, msg, progress, offset);
      min += offset;
      max += offset;
   }
   else
   {
      wxFAIL_MSG("Analysing Track when nothing to do!");
      min = -1.0, max = 1.0;   // sensible defaults?
      offset = 0.0;
   }
   extent = fmax(fabs(min), fabs(max));

   return result;
}

//AnalyseTrackData() takes a track, transforms it to bunch of buffer-blocks,
//and executes selected AnalyseOperation on it...
bool EffectNormalize::AnalyseTrackData(const WaveTrack * track, const TranslatableString &msg,
                                double &progress, float &offset)
{
   bool rc = true;

   //Transform the marker timepoints to samples
   auto start = track->TimeToLongSamples(mCurT0);
   auto end = track->TimeToLongSamples(mCurT1);

   //Get the length of the buffer (as double). len is
   //used simply to calculate a progress meter, so it is easier
   //to make it a double now than it is to do it later
   auto len = (end - start).as_double();

   //Initiate a processing buffer.  This buffer will (most likely)
   //be shorter than the length of the track being processed.
   Floats buffer{ track->GetMaxBlockSize() };

   mSum   = 0.0; // dc offset inits

   sampleCount blockSamples;
   sampleCount totalSamples = 0;

   //Go through the track one buffer at a time. s counts which
   //sample the current buffer starts at.
   auto s = start;
   while (s < end) {
      //Get a block of samples (smaller than the size of the buffer)
      //Adjust the block size if it is the final block in the track
      const auto block = limitSampleBufferSize(
         track->GetBestBlockSize(s),
         end - s
      );

      //Get the samples from the track and put them in the buffer
      track->GetFloats(buffer.get(), s, block, fillZero, true, &blockSamples);
      totalSamples += blockSamples;

      //Process the buffer.
      AnalyseDataDC(buffer.get(), block);

      //Increment s one blockfull of samples
      s += block;

      //Update the Progress meter
      if (TotalProgress(progress +
                        ((s - start).as_double() / len)/double(2*GetNumWaveTracks()), msg)) {
         rc = false; //lda .. break, not return, so that buffer is deleted
         break;
      }
   }
   if( totalSamples > 0 )
      offset = -mSum / totalSamples.as_double();  // calculate actual offset (amount that needs to be added on)
   else
      offset = 0.0;

   progress += 1.0/double(2*GetNumWaveTracks());
   //Return true because the effect processing succeeded ... unless cancelled
   return rc;
}

//ProcessOne() takes a track, transforms it to bunch of buffer-blocks,
//and executes ProcessData, on it...
// uses mMult and offset to normalize a track.
// mMult must be set before this is called
bool EffectNormalize::ProcessOne(
   WaveTrack * track, const TranslatableString &msg, double &progress, float offset)
{
   bool rc = true;

   //Transform the marker timepoints to samples
   auto start = track->TimeToLongSamples(mCurT0);
   auto end = track->TimeToLongSamples(mCurT1);

   //Get the length of the buffer (as double). len is
   //used simply to calculate a progress meter, so it is easier
   //to make it a double now than it is to do it later
   auto len = (end - start).as_double();

   //Initiate a processing buffer.  This buffer will (most likely)
   //be shorter than the length of the track being processed.
   Floats buffer{ track->GetMaxBlockSize() };

   //Go through the track one buffer at a time. s counts which
   //sample the current buffer starts at.
   auto s = start;
   while (s < end) {
      //Get a block of samples (smaller than the size of the buffer)
      //Adjust the block size if it is the final block in the track
      const auto block = limitSampleBufferSize(
         track->GetBestBlockSize(s),
         end - s
      );

      //Get the samples from the track and put them in the buffer
      track->GetFloats(buffer.get(), s, block);

      //Process the buffer.
      ProcessData(buffer.get(), block, offset);

      //Copy the newly-changed samples back onto the track.
      track->Set((samplePtr) buffer.get(), floatSample, s, block);

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
void EffectNormalize::AnalyseDataDC(float *buffer, size_t len)
{
   for(decltype(len) i = 0; i < len; i++)
      mSum += (double)buffer[i];
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
      EnableApply(false);
      return;
   }
   mWarning->SetLabel(wxT(""));

   // Disallow level stuff if not normalizing
   mLevelTextCtrl->Enable(mGain);
   mLeveldB->Enable(mGain);
   mStereoIndCheckBox->Enable(mGain);

   // Disallow OK/Preview if doing nothing
   EnableApply(mGain || mDC);
}
