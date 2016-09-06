/**********************************************************************

  Audacity: A Digital Audio Editor

  Normalize.cpp

  Dominic Mazzoni
  Vaughan Johnson (Preview)

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
//     Name       Type     Key                        Def      Min      Max   Scale
Param( Level,     double,  XO("Level"),               -1.0,    -145.0,  0.0,  1  );
Param( RemoveDC,  bool,    XO("RemoveDcOffset"),      true,    false,   true, 1  );
Param( ApplyGain, bool,    XO("ApplyGain"),           true,    false,   true, 1  );
Param( StereoInd, bool,    XO("StereoIndependent"),   false,   false,   true, 1  );

BEGIN_EVENT_TABLE(EffectNormalize, wxEvtHandler)
   EVT_CHECKBOX(wxID_ANY, EffectNormalize::OnUpdateUI)
   EVT_TEXT(wxID_ANY, EffectNormalize::OnUpdateUI)
END_EVENT_TABLE()

EffectNormalize::EffectNormalize()
{
   mLevel = DEF_Level;
   mDC = DEF_RemoveDC;
   mGain = DEF_ApplyGain;
   mStereoInd = DEF_StereoInd;

   SetLinearEffectFlag(false);
}

EffectNormalize::~EffectNormalize()
{
}

// IdentInterface implementation

wxString EffectNormalize::GetSymbol()
{
   return NORMALIZE_PLUGIN_SYMBOL;
}

wxString EffectNormalize::GetDescription()
{
   return XO("Sets the peak amplitude of one or more tracks");
}

// EffectIdentInterface implementation

EffectType EffectNormalize::GetType()
{
   return EffectTypeProcess;
}

// EffectClientInterface implementation

bool EffectNormalize::GetAutomationParameters(EffectAutomationParameters & parms)
{
   parms.Write(KEY_Level, mLevel);
   parms.Write(KEY_ApplyGain, mGain);
   parms.Write(KEY_RemoveDC, mDC);
   parms.Write(KEY_StereoInd, mStereoInd);

   return true;
}

bool EffectNormalize::SetAutomationParameters(EffectAutomationParameters & parms)
{
   ReadAndVerifyDouble(Level);
   ReadAndVerifyBool(ApplyGain);
   ReadAndVerifyBool(RemoveDC);
   ReadAndVerifyBool(StereoInd);

   mLevel = Level;
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
      gPrefs->Read(base + wxT("Level"), &mLevel, -1.0);
      if(mLevel > 0.0)  // this should never happen
         mLevel = -mLevel;
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
      // same value used for all tracks
      ratio = DB_TO_LINEAR(TrapDouble(mLevel, MIN_Level, MAX_Level));
   else
      ratio = 1.0;

   //Iterate over each track
   this->CopyInputTracks(); // Set up mOutputTracks.
   bool bGoodResult = true;
   SelectedTrackListOfKindIterator iter(Track::Wave, mOutputTracks.get());
   WaveTrack *track = (WaveTrack *) iter.First();
   WaveTrack *prevTrack;
   prevTrack = track;
   mCurTrackNum = 0;
   wxString topMsg;
   if(mDC && mGain)
      topMsg = _("Removing DC offset and Normalizing...\n");
   else if(mDC && !mGain)
      topMsg = _("Removing DC offset...\n");
   else if(!mDC && mGain)
      topMsg = _("Normalizing without removing DC offset...\n");
   else if(!mDC && !mGain)
      topMsg = wxT("Not doing anything)...\n");   // shouldn't get here

   while (track) {
      //Get start and end times from track
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();

      //Set the current bounds to whichever left marker is
      //greater and whichever right marker is less:
      mCurT0 = mT0 < trackStart? trackStart: mT0;
      mCurT1 = mT1 > trackEnd? trackEnd: mT1;

      // Process only if the right marker is to the right of the left marker
      if (mCurT1 > mCurT0) {
         wxString msg;
         wxString trackName = track->GetName();

         if(!track->GetLinked() || mStereoInd)
            msg = topMsg + _("Analyzing: ") + trackName;
         else
            msg = topMsg + _("Analyzing first track of stereo pair: ") + trackName;
         AnalyseTrack(track, msg);  // sets mOffset and offset-adjusted mMin and mMax
         if(!track->GetLinked() || mStereoInd) {   // mono or 'stereo tracks independently'
            float extent = wxMax(fabs(mMax), fabs(mMin));
            if( (extent > 0) && mGain )
               mMult = ratio / extent;
            else
               mMult = 1.0;
            msg = topMsg + _("Processing: ") + trackName;
            if(track->GetLinked() || prevTrack->GetLinked())  // only get here if there is a linked track but we are processing independently
               msg = topMsg + _("Processing stereo channels independently: ") + trackName;

            if (!ProcessOne(track, msg))
            {
               bGoodResult = false;
               break;
            }
         }
         else
         {
            // we have a linked stereo track
            // so we need to find it's min, max and offset
            // as they are needed to calc the multiplier for both tracks
            float offset1 = mOffset;   // remember ones from first track
            float min1 = mMin;
            float max1 = mMax;
            track = (WaveTrack *) iter.Next();  // get the next one
            mCurTrackNum++;   // keeps progress bar correct
            msg = topMsg + _("Analyzing second track of stereo pair: ") + trackName;
            AnalyseTrack(track, msg);  // sets mOffset and offset-adjusted mMin and mMax
            float offset2 = mOffset;   // ones for second track
            float min2 = mMin;
            float max2 = mMax;
            float extent = wxMax(fabs(min1), fabs(max1));
            extent = wxMax(extent, fabs(min2));
            extent = wxMax(extent, fabs(max2));
            if( (extent > 0) && mGain )
               mMult = ratio / extent; // we need to use this for both linked tracks
            else
               mMult = 1.0;
            mOffset = offset1;
            track = (WaveTrack *) iter.Prev();  // go back to the first linked one
            mCurTrackNum--;   // keeps progress bar correct
            msg = topMsg + _("Processing first track of stereo pair: ") + trackName;
            if (!ProcessOne(track, msg))
            {
               bGoodResult = false;
               break;
            }
            mOffset = offset2;
            track = (WaveTrack *) iter.Next();  // go to the second linked one
            mCurTrackNum++;   // keeps progress bar correct
            msg = topMsg + _("Processing second track of stereo pair: ") + trackName;
            if (!ProcessOne(track, msg))
            {
               bGoodResult = false;
               break;
            }
         }
      }

      //Iterate to the next track
      prevTrack = track;
      track = (WaveTrack *) iter.Next();
      mCurTrackNum++;
   }

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
            mDCCheckBox = S.AddCheckBox(_("Remove DC offset (center on 0.0 vertically)"),
                                        mDC ? wxT("true") : wxT("false"));
            mDCCheckBox->SetValidator(wxGenericValidator(&mDC));

            S.StartHorizontalLay(wxALIGN_CENTER, false);
            {
               mGainCheckBox = S.AddCheckBox(_("Normalize maximum amplitude to"),
                                             mGain ? wxT("true") : wxT("false"));
               mGainCheckBox->SetValidator(wxGenericValidator(&mGain));

               FloatingPointValidator<double> vldLevel(2, &mLevel, NUM_VAL_ONE_TRAILING_ZERO);
               vldLevel.SetRange(MIN_Level, MAX_Level);
               mLevelTextCtrl = S.AddTextBox(wxT(""), wxT(""), 10);
               mLevelTextCtrl->SetName(_("Maximum amplitude dB"));
               mLevelTextCtrl->SetValidator(vldLevel);
               mLeveldB = S.AddVariableText(_("dB"), false,
                                            wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
               mWarning = S.AddVariableText( wxT(""), false,
                                            wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
            }
            S.EndHorizontalLay();

         
            mStereoIndCheckBox = S.AddCheckBox(_("Normalize stereo channels independently"),
                                               mStereoInd ? wxT("true") : wxT("false"));
            mStereoIndCheckBox->SetValidator(wxGenericValidator(&mStereoInd));
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

void EffectNormalize::AnalyseTrack(WaveTrack * track, const wxString &msg)
{
   if(mGain) {
      // Since we need complete summary data, we need to block until the OD tasks are done for this track
      // TODO: should we restrict the flags to just the relevant block files (for selections)
      while (track->GetODFlags()) {
         // update the gui
         mProgress->Update(0, wxT("Waiting for waveform to finish computing..."));
         wxMilliSleep(100);
      }

      track->GetMinMax(&mMin, &mMax, mCurT0, mCurT1); // set mMin, mMax.  No progress bar here as it's fast.
   } else {
      mMin = -1.0, mMax = 1.0;   // sensible defaults?
   }

   if(mDC) {
      AnalyseDC(track, msg); // sets mOffset
      mMin += mOffset;
      mMax += mOffset;
   } else {
      mOffset = 0.0;
   }
}

//AnalyseDC() takes a track, transforms it to bunch of buffer-blocks,
//and executes AnalyzeData on it...
// sets mOffset
bool EffectNormalize::AnalyseDC(WaveTrack * track, const wxString &msg)
{
   bool rc = true;

   mOffset = 0.0; // we might just return

   if(!mDC)  // don't do analysis if not doing dc removal
      return(rc);

   //Transform the marker timepoints to samples
   auto start = track->TimeToLongSamples(mCurT0);
   auto end = track->TimeToLongSamples(mCurT1);

   //Get the length of the buffer (as double). len is
   //used simply to calculate a progress meter, so it is easier
   //to make it a double now than it is to do it later
   auto len = (end - start).as_double();

   //Initiate a processing buffer.  This buffer will (most likely)
   //be shorter than the length of the track being processed.
   float *buffer = new float[track->GetMaxBlockSize()];

   mSum = 0.0; // dc offset inits
   mCount = 0;

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
      track->Get((samplePtr) buffer, floatSample, s, block);

      //Process the buffer.
      AnalyzeData(buffer, block);

      //Increment s one blockfull of samples
      s += block;

      //Update the Progress meter
      if (TrackProgress(mCurTrackNum,
                        ((s - start).as_double() / len)/2.0, msg)) {
         rc = false; //lda .. break, not return, so that buffer is deleted
         break;
      }
   }

   //Clean up the buffer
   delete[] buffer;

   mOffset = -mSum / mCount.as_double();  // calculate actual offset (amount that needs to be added on)

   //Return true because the effect processing succeeded ... unless cancelled
   return rc;
}

//ProcessOne() takes a track, transforms it to bunch of buffer-blocks,
//and executes ProcessData, on it...
// uses mMult and mOffset to normalize a track.  Needs to have them set before being called
bool EffectNormalize::ProcessOne(WaveTrack * track, const wxString &msg)
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
   float *buffer = new float[track->GetMaxBlockSize()];

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
      track->Get((samplePtr) buffer, floatSample, s, block);

      //Process the buffer.
      ProcessData(buffer, block);

      //Copy the newly-changed samples back onto the track.
      track->Set((samplePtr) buffer, floatSample, s, block);

      //Increment s one blockfull of samples
      s += block;

      //Update the Progress meter
      if (TrackProgress(mCurTrackNum,
                        0.5+((s - start).as_double() / len)/2.0, msg)) {
         rc = false; //lda .. break, not return, so that buffer is deleted
         break;
      }
   }
   //Clean up the buffer
   delete[] buffer;

   //Return true because the effect processing succeeded ... unless cancelled
   return rc;
}

void EffectNormalize::AnalyzeData(float *buffer, size_t len)
{
   for(decltype(len) i = 0; i < len; i++)
      mSum += (double)buffer[i];
   mCount += len;
}

void EffectNormalize::ProcessData(float *buffer, size_t len)
{
   for(decltype(len) i = 0; i < len; i++) {
      float adjFrame = (buffer[i] + mOffset) * mMult;
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
      mWarning->SetLabel(_(".  Maximum 0dB."));
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
