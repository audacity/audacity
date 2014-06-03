/**********************************************************************

  Audacity: A Digital Audio Editor

  Normalize.cpp

  Dominic Mazzoni
  Vaughan Johnson (Preview)

*******************************************************************//**

\class EffectNormalize
\brief An Effect.

*//****************************************************************//**

\class NormalizeDialog
\brief Dialog used with EffectNormalize

*//*******************************************************************/


#include "../Audacity.h" // for rint from configwin.h

#include <math.h>

#include "Normalize.h"
#include "../ShuttleGui.h"
#include "../Internat.h"
#include "../WaveTrack.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Shuttle.h"

#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/defs.h>
#include <wx/intl.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textdlg.h>

#define NORMALIZE_DB_MIN -145
#define NORMALIZE_DB_MAX 60

EffectNormalize::EffectNormalize()
{
   Init();
}

static double gFrameSum; //lda odd ... having this as member var crashed on exit

bool EffectNormalize::Init()
{
   int boolProxy = gPrefs->Read(wxT("/Effects/Normalize/RemoveDcOffset"), 1);
   mDC = (boolProxy == 1);
   boolProxy = gPrefs->Read(wxT("/Effects/Normalize/Normalize"), 1);
   mGain = (boolProxy == 1);
   gPrefs->Read(wxT("/Effects/Normalize/Level"), &mLevel, -1.0);
   if(mLevel > 0.0)  // this should never happen
      mLevel = -mLevel;
   boolProxy = gPrefs->Read(wxT("/Effects/Normalize/StereoIndependent"), 0L);
   mStereoInd = (boolProxy == 1);
   return true;
}

wxString EffectNormalize::GetEffectDescription() // useful only after parameter values have been set
{
   // Note: This is useful only after ratio has been set.
   wxString strResult =
      /* i18n-hint: First %s is the effect name, 2nd and 3rd are either true or
       * false (translated below) if those options were selected */
      wxString::Format(_("Applied effect: %s remove dc offset = %s, normalize amplitude = %s, stereo independent %s"),
                        this->GetEffectName().c_str(),
                        /* i18n-hint: true here means that the option was
                         * selected. Opposite false if not selected */
                        mDC ? _("true") : _("false"),
                        mGain ? _("true") : _("false"),
                        mStereoInd ? _("true") : _("false"));
   if (mGain)
      strResult += wxString::Format(_(", maximum amplitude = %.1f dB"), mLevel);

   return strResult;
}

bool EffectNormalize::TransferParameters( Shuttle & shuttle )
{
   shuttle.TransferBool( wxT("ApplyGain"), mGain, true );
   shuttle.TransferBool( wxT("RemoveDcOffset"), mDC, true );
   shuttle.TransferDouble( wxT("Level"), mLevel, 0.0);
   shuttle.TransferBool( wxT("StereoIndependent"), mStereoInd, false );
   return true;
}

bool EffectNormalize::CheckWhetherSkipEffect()
{
   bool rc = ((mGain == false) && (mDC == false));
   return rc;
}

void EffectNormalize::End()
{
   bool bValidate;
   gPrefs->Read(wxT("/Validate/Enabled"), &bValidate, false ); // this never get written!  Why is this here? MJS
   if( bValidate )
   {
      int checkOffset = abs((int)(mOffset * 1000.0));
      gPrefs->Write(wxT("/Validate/Norm_Offset"), checkOffset);
      int checkMultiplier = abs((int)(mMult * 1000.0));
      gPrefs->Write(wxT("/Validate/Norm_Multiplier"), checkMultiplier);
      int checkFrameSum = (int)gFrameSum;
      gPrefs->Write(wxT("/Validate/Norm_FrameSum"), checkFrameSum);
      gPrefs->Flush();
   }
}

bool EffectNormalize::PromptUser()
{
   NormalizeDialog dlog(this, mParent);
   dlog.mGain = mGain;
   dlog.mDC = mDC;
   dlog.mLevel = mLevel;
   dlog.mStereoInd = mStereoInd;
   dlog.TransferDataToWindow();

   dlog.CentreOnParent();
   dlog.ShowModal();

   if (dlog.GetReturnCode() == wxID_CANCEL)
      return false;

   mGain = dlog.mGain;
   mDC = dlog.mDC;
   mLevel = dlog.mLevel;
   mStereoInd = dlog.mStereoInd;
   gPrefs->Write(wxT("/Effects/Normalize/RemoveDcOffset"), mDC);
   gPrefs->Write(wxT("/Effects/Normalize/Normalize"), mGain);
   gPrefs->Write(wxT("/Effects/Normalize/Level"), mLevel);
   gPrefs->Write(wxT("/Effects/Normalize/StereoIndependent"), mStereoInd);

   return gPrefs->Flush();
}

bool EffectNormalize::Process()
{
   if (mGain == false && mDC == false)
      return true;

   float ratio;
   if( mGain )
      ratio = pow(10.0,TrapDouble(mLevel, // same value used for all tracks
                               NORMALIZE_DB_MIN,
                               NORMALIZE_DB_MAX)/20.0);
   else
      ratio = 1.0;

   //Iterate over each track
   this->CopyInputTracks(); // Set up mOutputTracks.
   bool bGoodResult = true;
   SelectedTrackListOfKindIterator iter(Track::Wave, mOutputTracks);
   WaveTrack *track = (WaveTrack *) iter.First();
   WaveTrack *prevTrack;
   prevTrack = track;
   mCurTrackNum = 0;
   wxString topMsg;
   if(mDC & mGain)
      topMsg = _("Removing DC offset and Normalizing...\n");
   else if(mDC & !mGain)
      topMsg = _("Removing DC offset...\n");
   else if(!mDC & mGain)
      topMsg = _("Normalizing without removing DC offset...\n");
   else if(!mDC & !mGain)
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

void EffectNormalize::AnalyseTrack(WaveTrack * track, wxString msg)
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
bool EffectNormalize::AnalyseDC(WaveTrack * track, wxString msg)
{
   bool rc = true;
   sampleCount s;

   mOffset = 0.0; // we might just return

   if(!mDC)  // don't do analysis if not doing dc removal
      return(rc);

   //Transform the marker timepoints to samples
   sampleCount start = track->TimeToLongSamples(mCurT0);
   sampleCount end = track->TimeToLongSamples(mCurT1);

   //Get the length of the buffer (as double). len is
   //used simply to calculate a progress meter, so it is easier
   //to make it a double now than it is to do it later
   double len = (double)(end - start);

   //Initiate a processing buffer.  This buffer will (most likely)
   //be shorter than the length of the track being processed.
   float *buffer = new float[track->GetMaxBlockSize()];

   mSum = 0.0; // dc offset inits
   mCount = 0;

   //Go through the track one buffer at a time. s counts which
   //sample the current buffer starts at.
   s = start;
   while (s < end) {
      //Get a block of samples (smaller than the size of the buffer)
      sampleCount block = track->GetBestBlockSize(s);

      //Adjust the block size if it is the final block in the track
      if (s + block > end)
         block = end - s;

      //Get the samples from the track and put them in the buffer
      track->Get((samplePtr) buffer, floatSample, s, block);

      //Process the buffer.
      AnalyzeData(buffer, block);

      //Increment s one blockfull of samples
      s += block;

      //Update the Progress meter
      if (TrackProgress(mCurTrackNum,
                        ((double)(s - start) / len)/2.0, msg)) {
         rc = false; //lda .. break, not return, so that buffer is deleted
         break;
      }
   }

   //Clean up the buffer
   delete[] buffer;

   mOffset = (float)(-mSum / mCount);  // calculate actual offset (amount that needs to be added on)

   //Return true because the effect processing succeeded ... unless cancelled
   return rc;
}

//ProcessOne() takes a track, transforms it to bunch of buffer-blocks,
//and executes ProcessData, on it...
// uses mMult and mOffset to normalize a track.  Needs to have them set before being called
bool EffectNormalize::ProcessOne(WaveTrack * track, wxString msg)
{
   bool rc = true;
   sampleCount s;

   //Transform the marker timepoints to samples
   sampleCount start = track->TimeToLongSamples(mCurT0);
   sampleCount end = track->TimeToLongSamples(mCurT1);

   //Get the length of the buffer (as double). len is
   //used simply to calculate a progress meter, so it is easier
   //to make it a double now than it is to do it later
   double len = (double)(end - start);

   //Initiate a processing buffer.  This buffer will (most likely)
   //be shorter than the length of the track being processed.
   float *buffer = new float[track->GetMaxBlockSize()];

   //Go through the track one buffer at a time. s counts which
   //sample the current buffer starts at.
   s = start;
   while (s < end) {
      //Get a block of samples (smaller than the size of the buffer)
      sampleCount block = track->GetBestBlockSize(s);

      //Adjust the block size if it is the final block in the track
      if (s + block > end)
         block = end - s;

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
                        0.5+((double)(s - start) / len)/2.0, msg)) {
         rc = false; //lda .. break, not return, so that buffer is deleted
         break;
      }
   }
   //Clean up the buffer
   delete[] buffer;

   //Return true because the effect processing succeeded ... unless cancelled
   return rc;
}

void EffectNormalize::AnalyzeData(float *buffer, sampleCount len)
{
   sampleCount i;

   for(i=0; i<len; i++)
      mSum += (double)buffer[i];
   mCount += len;
}

void EffectNormalize::ProcessData(float *buffer, sampleCount len)
{
   sampleCount i;

   for(i=0; i<len; i++) {
      float adjFrame = (buffer[i] + mOffset) * mMult;
      buffer[i] = adjFrame;
      gFrameSum += fabs(adjFrame);  //lda: validation.
   }
}

//----------------------------------------------------------------------------
// NormalizeDialog
//----------------------------------------------------------------------------

#define ID_DC_REMOVE 10002
#define ID_NORMALIZE_AMPLITUDE 10003
#define ID_LEVEL_TEXT 10004

BEGIN_EVENT_TABLE(NormalizeDialog, EffectDialog)
   EVT_CHECKBOX(ID_DC_REMOVE, NormalizeDialog::OnUpdateUI)
   EVT_CHECKBOX(ID_NORMALIZE_AMPLITUDE, NormalizeDialog::OnUpdateUI)
   EVT_BUTTON(ID_EFFECT_PREVIEW, NormalizeDialog::OnPreview)
   EVT_TEXT(ID_LEVEL_TEXT, NormalizeDialog::OnUpdateUI)
END_EVENT_TABLE()

NormalizeDialog::NormalizeDialog(EffectNormalize *effect,
                                 wxWindow * parent)
:  EffectDialog(parent, _("Normalize"), PROCESS_EFFECT),
   mEffect(effect)
{
   mDC = false;
   mGain = false;
   mLevel = 0;
   mStereoInd = false;

   Init();
}

void NormalizeDialog::PopulateOrExchange(ShuttleGui & S)
{
   wxTextValidator vld(wxFILTER_NUMERIC);

   S.StartTwoColumn();
   {
      S.StartVerticalLay(false);
      {
         mDCCheckBox =
            S.Id(ID_DC_REMOVE).
               AddCheckBox(_("Remove DC offset (center on 0.0 vertically)"),
                           mDC ? wxT("true") : wxT("false"));

         S.StartHorizontalLay(wxALIGN_CENTER, false);
         {
            mGainCheckBox =
               S.Id(ID_NORMALIZE_AMPLITUDE).
                  AddCheckBox(_("Normalize maximum amplitude to"),
                              mGain ? wxT("true") : wxT("false"));

            mLevelTextCtrl = S.Id(ID_LEVEL_TEXT).AddTextBox(wxT(""), wxT(""), 10);
            mLevelTextCtrl->SetValidator(vld);
            mLevelTextCtrl->SetName(_("Maximum amplitude dB"));
            mLeveldB = S.AddVariableText(_("dB"), false,
                                         wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
            mWarning = S.AddVariableText( wxT(""), false,
                                         wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
         }
         S.EndHorizontalLay();
         mStereoIndCheckBox = S.AddCheckBox(_("Normalize stereo channels independently"),
                                     mStereoInd ? wxT("true") : wxT("false"));
      }
      S.EndVerticalLay();
   }
   S.EndTwoColumn();
}

bool NormalizeDialog::TransferDataToWindow()
{
   mGainCheckBox->SetValue(mGain);
   mDCCheckBox->SetValue(mDC);
   mLevelTextCtrl->SetValue(Internat::ToDisplayString(mLevel, 1));
   mStereoIndCheckBox->SetValue(mStereoInd);

   UpdateUI();

   TransferDataFromWindow();

   return true;
}

bool NormalizeDialog::TransferDataFromWindow()
{
   mGain = mGainCheckBox->GetValue();
   mDC = mDCCheckBox->GetValue();
   mLevel = Internat::CompatibleToDouble(mLevelTextCtrl->GetValue());
   mStereoInd = mStereoIndCheckBox->GetValue();

   return true;
}

void NormalizeDialog::OnUpdateUI(wxCommandEvent& WXUNUSED(event))
{
   UpdateUI();
}

void NormalizeDialog::UpdateUI()
{
   // Disallow level stuff if not normalizing
   bool enable = mGainCheckBox->GetValue();
   mLevelTextCtrl->Enable(enable);
   mLeveldB->Enable(enable);
   mStereoIndCheckBox->Enable(enable);

   // Disallow OK/Preview if doing nothing
   wxButton *ok = (wxButton *) FindWindow(wxID_OK);
   wxButton *preview = (wxButton *) FindWindow(ID_EFFECT_PREVIEW);
   bool dc = mDCCheckBox->GetValue();
   if( !enable && !dc )
   {
      ok->Enable(false);
      preview->Enable(false);
   }
   else
   {
      ok->Enable(true);
      preview->Enable(true);
   }

   // Disallow OK/Preview if requested level is > 0
   wxString val = mLevelTextCtrl->GetValue();
   double r;
   val.ToDouble(&r);
   if(r > 0.0)
   {
      ok->Enable(false);
      preview->Enable(false);
      mWarning->SetLabel(_(".  Maximum 0dB."));
   }
   else
      mWarning->SetLabel(wxT(""));
}

void NormalizeDialog::OnPreview(wxCommandEvent & WXUNUSED(event))
{
   TransferDataFromWindow();

   // Save & restore parameters around Preview, because we didn't do OK.
   bool oldGain = mEffect->mGain;
   bool oldDC = mEffect->mDC;
   double oldLevel = mEffect->mLevel;
   bool oldStereoInd = mEffect->mStereoInd;

   mEffect->mGain = mGain;
   mEffect->mDC = mDC;
   mEffect->mLevel = mLevel;
   mEffect->mStereoInd = mStereoInd;

   mEffect->Preview();

   mEffect->mGain = oldGain;
   mEffect->mDC = oldDC;
   mEffect->mLevel = oldLevel;
   mEffect->mStereoInd = oldStereoInd;
}
