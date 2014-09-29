/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangeSpeed.cpp

  Vaughan Johnson, Dominic Mazzoni

*******************************************************************//**

\class EffectChangeSpeed
\brief An Effect that affects both pitch & speed.

*//****************************************************************//**

\class ChangeSpeedDialog
\brief Dialog used with EffectChangeSpeed

*//*******************************************************************/

#include <math.h>

#include <wx/button.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/valtext.h>

#include "../Audacity.h"
#include "../Envelope.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../ShuttleGui.h"

#include "ChangeSpeed.h"
#include "TimeWarper.h"


// the standard vinyl RPM choices
// If the percent change is not one of these ratios, the choice control gets "n/a".
enum {
   kVinyl_33AndAThird = 0,
   kVinyl_45,
   kVinyl_78,
   kVinyl_NA
};


//
// EffectChangeSpeed
//

EffectChangeSpeed::EffectChangeSpeed()
{
   // control values
   m_PercentChange = 0.0;
   mFromVinyl = kVinyl_33AndAThird;
   mToVinyl = kVinyl_33AndAThird;
}

wxString EffectChangeSpeed::GetEffectDescription() {
   // Note: This is useful only after change amount has been set.
   return wxString::Format(_("Applied effect: %s %.1f%%"),
                           this->GetEffectName().c_str(),
                           m_PercentChange);
}

double EffectChangeSpeed::CalcPreviewInputLength(double previewLength)
{
   return previewLength * (100.0 + m_PercentChange) / 100.0;
}

bool EffectChangeSpeed::PromptUser()
{
   ChangeSpeedDialog dlog(this, mParent);
   dlog.m_PercentChange = m_PercentChange;
   dlog.mFromVinyl = mFromVinyl;
   dlog.mToVinyl = mToVinyl;
   // Don't need to call TransferDataToWindow, although other
   // Audacity dialogs (from which I derived this one) do it, because
   // ShowModal calls stuff that eventually calls wxWindowBase::OnInitDialog,
   // which calls dlog.TransferDataToWindow();
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (dlog.GetReturnCode() == wxID_CANCEL)
      return false;

   m_PercentChange = dlog.m_PercentChange;
   mFromVinyl = dlog.mFromVinyl;
   mToVinyl = dlog.mToVinyl;

   return true;
}

bool EffectChangeSpeed::TransferParameters(Shuttle& shuttle)
{
   shuttle.TransferDouble(wxT("Percentage"), m_PercentChange, 0.0);
   return true;
}

// Labels are time-scaled linearly inside the affected region, and labels after
// the region are shifted along according to how the region size changed.
bool EffectChangeSpeed::ProcessLabelTrack(Track *t)
{
   SetTimeWarper(new RegionTimeWarper(mT0, mT1,
                     new LinearTimeWarper(mT0, mT0,
                         mT1, mT0 + (mT1-mT0)*mFactor)));
   LabelTrack *lt = (LabelTrack*)t;
   if (lt == NULL) return false;
   lt->WarpLabels(*GetTimeWarper());
   return true;
}

bool EffectChangeSpeed::Process()
{
   // Similar to EffectSoundTouch::Process()

   // Iterate over each track.
   // Track::All is needed because this effect needs to introduce
   // silence in the sync-lock group tracks to keep sync
   this->CopyInputTracks(Track::All); // Set up mOutputTracks.
   bool bGoodResult = true;

   TrackListIterator iter(mOutputTracks);
   Track* t;
   mCurTrackNum = 0;
   mMaxNewLength = 0.0;

   mFactor = 100.0 / (100.0 + m_PercentChange);

   t = iter.First();
   while (t != NULL)
   {
      if (t->GetKind() == Track::Label) {
         if (t->GetSelected() || t->IsSyncLockSelected())
         {
            if (!ProcessLabelTrack(t)) {
               bGoodResult = false;
               break;
            }
         }
      }
      else if (t->GetKind() == Track::Wave && t->GetSelected())
      {
         WaveTrack *pOutWaveTrack = (WaveTrack*)t;
         //Get start and end times from track
         mCurT0 = pOutWaveTrack->GetStartTime();
         mCurT1 = pOutWaveTrack->GetEndTime();

         //Set the current bounds to whichever left marker is
         //greater and whichever right marker is less:
         mCurT0 = wxMax(mT0, mCurT0);
         mCurT1 = wxMin(mT1, mCurT1);

         // Process only if the right marker is to the right of the left marker
         if (mCurT1 > mCurT0) {
            //Transform the marker timepoints to samples
            sampleCount start = pOutWaveTrack->TimeToLongSamples(mCurT0);
            sampleCount end = pOutWaveTrack->TimeToLongSamples(mCurT1);

            //ProcessOne() (implemented below) processes a single track
            if (!ProcessOne(pOutWaveTrack, start, end))
            {
               bGoodResult = false;
               break;
            }
         }
         mCurTrackNum++;
      }
      else if (t->IsSyncLockSelected())
      {
         t->SyncLockAdjust(mT1, mT0 + (mT1 - mT0) * mFactor);
      }

      //Iterate to the next track
      t=iter.Next();
   }

   if (bGoodResult)
      ReplaceProcessedTracks(bGoodResult);

   mT1 = mT0 + mMaxNewLength; // Update selection.

   return bGoodResult;
}

// ProcessOne() takes a track, transforms it to bunch of buffer-blocks,
// and calls libsamplerate code on these blocks.
bool EffectChangeSpeed::ProcessOne(WaveTrack * track,
                           sampleCount start, sampleCount end)
{
   if (track == NULL)
      return false;

   // initialization, per examples of Mixer::Mixer and
   // EffectSoundTouch::ProcessOne

   WaveTrack * outputTrack = mFactory->NewWaveTrack(track->GetSampleFormat(),
                                                    track->GetRate());

   //Get the length of the selection (as double). len is
   //used simple to calculate a progress meter, so it is easier
   //to make it a double now than it is to do it later
   double len = (double)(end - start);

   // Initiate processing buffers, most likely shorter than
   // the length of the selection being processed.
   sampleCount inBufferSize = track->GetMaxBlockSize();

   float * inBuffer = new float[inBufferSize];

   sampleCount outBufferSize =
      (sampleCount)((mFactor * inBufferSize) + 10);
   float * outBuffer = new float[outBufferSize];

   // Set up the resampling stuff for this track.
   Resample resample(true, mFactor, mFactor); // constant rate resampling

   //Go through the track one buffer at a time. samplePos counts which
   //sample the current buffer starts at.
   bool bResult = true;
   sampleCount blockSize;
   sampleCount samplePos = start;
   while (samplePos < end) {
      //Get a blockSize of samples (smaller than the size of the buffer)
      blockSize = track->GetBestBlockSize(samplePos);

      //Adjust the block size if it is the final block in the track
      if (samplePos + blockSize > end)
         blockSize = end - samplePos;

      //Get the samples from the track and put them in the buffer
      track->Get((samplePtr) inBuffer, floatSample, samplePos, blockSize);

      int inUsed;
      int outgen = resample.Process(mFactor,
                                    inBuffer,
                                    blockSize,
                                    ((samplePos + blockSize) >= end),
                                    &inUsed,
                                    outBuffer,
                                    outBufferSize);
      if (outgen < 0) {
         bResult = false;
         break;
      }

      if (outgen > 0)
         outputTrack->Append((samplePtr)outBuffer, floatSample,
                             outgen);

      // Increment samplePos
      samplePos += inUsed;

      // Update the Progress meter
      if (TrackProgress(mCurTrackNum, (samplePos - start) / len)) {
         bResult = false;
         break;
      }
   }

   // Flush the output WaveTrack (since it's buffered, too)
   outputTrack->Flush();

   // Clean up the buffers
   delete [] inBuffer;
   delete [] outBuffer;

   // Take the output track and insert it in place of the original
   // sample data
   double newLength = outputTrack->GetEndTime();
   if (bResult)
   {
      SetTimeWarper(new LinearTimeWarper(mCurT0, mCurT0, mCurT1, mCurT0 + newLength));
      bResult = track->ClearAndPaste(mCurT0, mCurT1, outputTrack, true, false, GetTimeWarper());
   }

   if (newLength > mMaxNewLength)
      mMaxNewLength = newLength;

   // Delete the outputTrack now that its data is inserted in place
   delete outputTrack;

   return bResult;
}


//
// ChangeSpeedDialog
//

// -99 for PERCENTCHANGE_MIN because -100% is nonsensical.
#define PERCENTCHANGE_MIN -99

#define PERCENTCHANGE_MAX 100 // warped above zero to actually go up to 400%
#define PERCENTCHANGE_SLIDER_WARP 1.30105 // warp power takes max from 100 to 400.

enum {
   ID_TEXT_PERCENTCHANGE = 10001,
   ID_SLIDER_PERCENTCHANGE,
   ID_CHOICE_FROMVINYL,
   ID_CHOICE_TOVINYL
};


// event table for ChangeSpeedDialog

BEGIN_EVENT_TABLE(ChangeSpeedDialog, EffectDialog)
    EVT_TEXT(ID_TEXT_PERCENTCHANGE, ChangeSpeedDialog::OnText_PercentChange)
    EVT_SLIDER(ID_SLIDER_PERCENTCHANGE, ChangeSpeedDialog::OnSlider_PercentChange)
    EVT_CHOICE(ID_CHOICE_FROMVINYL, ChangeSpeedDialog::OnChoice_FromVinyl)
    EVT_CHOICE(ID_CHOICE_TOVINYL, ChangeSpeedDialog::OnChoice_ToVinyl)

    EVT_BUTTON(ID_EFFECT_PREVIEW, ChangeSpeedDialog::OnPreview)
END_EVENT_TABLE()

ChangeSpeedDialog::ChangeSpeedDialog(EffectChangeSpeed *effect, wxWindow *parent)
:  EffectDialog(parent,
   /* i18n-hint: Audacity's change speed effect changes the speed and pitch.*/
   _("Change Speed"),
   PROCESS_EFFECT),
   mEffect(effect)
{
   mbLoopDetect = false;

   // NULL out these control members because there are some cases where the
   // event table handlers get called during this method, and those handlers that
   // can cause trouble check for NULL.
   mpTextCtrl_PercentChange = NULL;
   mpSlider_PercentChange = NULL;
   mpChoice_FromVinyl = NULL;
   mpChoice_ToVinyl = NULL;

   // effect parameters
   m_PercentChange = 0.0;
   mFromVinyl = kVinyl_33AndAThird;
   mToVinyl = kVinyl_33AndAThird;

   Init();
}

void ChangeSpeedDialog::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(5);

   S.AddSpace(0, 5);
   S.AddTitle(_("Change Speed, affecting both Tempo and Pitch"));

   //
   S.StartMultiColumn(2, wxCENTER);
   {
      mpTextCtrl_PercentChange =
         S.Id(ID_TEXT_PERCENTCHANGE).AddTextBox(_("Percent Change:"), wxT(""), 12);
      wxTextValidator validator(wxFILTER_NUMERIC);
      mpTextCtrl_PercentChange->SetValidator(validator);
   }
   S.EndMultiColumn();

   //
   S.StartHorizontalLay(wxEXPAND);
   {
      S.SetStyle(wxSL_HORIZONTAL);
      mpSlider_PercentChange =
         S.Id(ID_SLIDER_PERCENTCHANGE).AddSlider(wxT(""), 0, (int)PERCENTCHANGE_MAX, (int)PERCENTCHANGE_MIN);
      mpSlider_PercentChange->SetName(_("Percent Change"));
   }
   S.EndHorizontalLay();

   //
   S.StartMultiColumn(5, wxCENTER);
   {
      S.AddUnits(_("Standard Vinyl RPM:"));

      wxArrayString rpmStrings;
      rpmStrings.Add(wxT("33 1/3"));
      rpmStrings.Add(wxT("45"));
      rpmStrings.Add(wxT("78"));
      /* i18n-hint: n/a is an English abbreviation meaning "not applicable". */
      rpmStrings.Add(_("n/a"));

      mpChoice_FromVinyl =
         S.Id(ID_CHOICE_FROMVINYL).AddChoice(_("from"), wxT(""), &rpmStrings);
      mpChoice_FromVinyl->SetName(_("From RPM"));
      mpChoice_FromVinyl->SetSizeHints(100, -1);

      mpChoice_ToVinyl =
         S.Id(ID_CHOICE_TOVINYL).AddChoice(_("to"), wxT(""), &rpmStrings);
      mpChoice_ToVinyl->SetName(_("To RPM"));
      mpChoice_ToVinyl->SetSizeHints(100, -1);
   }
   S.EndMultiColumn();
}

bool ChangeSpeedDialog::TransferDataToWindow()
{
   mbLoopDetect = true;

   // percent change controls
   this->Update_Text_PercentChange();
   this->Update_Slider_PercentChange();

   // from/to Vinyl controls
   if (mpChoice_FromVinyl)
      mpChoice_FromVinyl->SetSelection(mFromVinyl);

   if (mpChoice_ToVinyl)
      mpChoice_ToVinyl->SetSelection(mToVinyl);

   mbLoopDetect = false;

   return true;
}

bool ChangeSpeedDialog::TransferDataFromWindow()
{
   // percent change
   // Ignore mpSlider_PercentChange because mpTextCtrl_PercentChange
   // always tracks it & is more precise (decimal points).
   if (mpTextCtrl_PercentChange)
   {
      double newValue = 0;
      wxString str = mpTextCtrl_PercentChange->GetValue();
      str.ToDouble(&newValue);
      m_PercentChange = newValue;
   }

   // from/to Vinyl controls
   if (mpChoice_FromVinyl)
      mFromVinyl = mpChoice_FromVinyl->GetSelection();

   if (mpChoice_ToVinyl)
      mToVinyl = mpChoice_ToVinyl->GetSelection();

   return true;
}


// handler implementations for ChangeSpeedDialog

void ChangeSpeedDialog::OnText_PercentChange(wxCommandEvent & WXUNUSED(event))
{
   if (mbLoopDetect)
      return;

   if (mpTextCtrl_PercentChange) {
      double newValue = 0;
      wxString str = mpTextCtrl_PercentChange->GetValue();
      str.ToDouble(&newValue);
      m_PercentChange = newValue;

      mbLoopDetect = true;
      this->Update_Slider_PercentChange();
      this->Update_Vinyl();
      mbLoopDetect = false;

      FindWindow(wxID_OK)->Enable(m_PercentChange > -100.0);
   }
}

void ChangeSpeedDialog::OnSlider_PercentChange(wxCommandEvent & WXUNUSED(event))
{
   if (mbLoopDetect)
      return;

   if (mpSlider_PercentChange) {
      m_PercentChange = (double)(mpSlider_PercentChange->GetValue());
      // Warp positive values to actually go up faster & further than negatives.
      if (m_PercentChange > 0.0)
         m_PercentChange = pow(m_PercentChange, PERCENTCHANGE_SLIDER_WARP);

      mbLoopDetect = true;
      this->Update_Text_PercentChange();
      this->Update_Vinyl();
      mbLoopDetect = false;
   }
}

void ChangeSpeedDialog::OnChoice_FromVinyl(wxCommandEvent & WXUNUSED(event))
{
   if (mbLoopDetect)
      return;

   if (mpChoice_FromVinyl) {
      mFromVinyl = mpChoice_FromVinyl->GetSelection();

      mbLoopDetect = true;
      this->Update_PercentChange();
      mbLoopDetect = false;
   }
}

void ChangeSpeedDialog::OnChoice_ToVinyl(wxCommandEvent & WXUNUSED(event))
{
   if (mbLoopDetect)
      return;

   if (mpChoice_ToVinyl) {
      mToVinyl = mpChoice_ToVinyl->GetSelection();

      mbLoopDetect = true;
      this->Update_PercentChange();
      mbLoopDetect = false;
   }
}

void ChangeSpeedDialog::OnPreview(wxCommandEvent & WXUNUSED(event))
{
   TransferDataFromWindow();

   // Save & restore parameters around Preview, because we didn't do OK.
   double oldPercentChange = mEffect->m_PercentChange;
   if( m_PercentChange < -99.0)
   {
      m_PercentChange = -99.0;
      this->Update_Text_PercentChange();
   }
   mEffect->m_PercentChange = m_PercentChange;
   mEffect->Preview();
   mEffect->m_PercentChange = oldPercentChange;
}

// helper fns

void ChangeSpeedDialog::Update_Text_PercentChange()
{
   if (mpTextCtrl_PercentChange) {
      wxString str;
      str.Printf(wxT("%.3f"), m_PercentChange);
      mpTextCtrl_PercentChange->SetValue(str);
      FindWindow(wxID_OK)->Enable(m_PercentChange > -100.0);
   }
}

void ChangeSpeedDialog::Update_Slider_PercentChange()
{
   if (mpSlider_PercentChange) {
      double unwarped = m_PercentChange;
      if (unwarped > 0.0)
         // Un-warp values above zero to actually go up to PERCENTCHANGE_MAX.
         unwarped = pow(m_PercentChange, (1.0 / PERCENTCHANGE_SLIDER_WARP));

      // Add 0.5 to unwarped so trunc -> round.
      mpSlider_PercentChange->SetValue((int)(unwarped + 0.5));
   }
}

void ChangeSpeedDialog::Update_Vinyl()
// Update Vinyl controls for new percent change.
{
   if (mpChoice_ToVinyl)
   {
      // Chances are so low that the slider will exactly match a
      // standard ratio, just turn it "n/a" unless it's 0.0.
      if ((m_PercentChange == 0.0) && mpChoice_FromVinyl)
         mpChoice_ToVinyl->SetSelection(mpChoice_FromVinyl->GetSelection());
      else
         mpChoice_ToVinyl->SetSelection(kVinyl_NA);
   }
}

void ChangeSpeedDialog::Update_PercentChange()
// Update percent change controls for new Vinyl values.
{
   // If mFromVinyl & mToVinyl are set, then there's a new percent change.
   if ((mFromVinyl != kVinyl_NA) && (mToVinyl != kVinyl_NA))
   {
      double fromRPM;
      double toRPM;
      switch (mFromVinyl) {
      default:
      case kVinyl_33AndAThird:   fromRPM = 33.0 + (1.0 / 3.0); break;
      case kVinyl_45:            fromRPM = 45.0; break;
      case kVinyl_78:            fromRPM = 78; break;
      }
      switch (mToVinyl) {
      default:
      case kVinyl_33AndAThird:   toRPM = 33.0 + (1.0 / 3.0); break;
      case kVinyl_45:            toRPM = 45.0; break;
      case kVinyl_78:            toRPM = 78; break;
      }
      m_PercentChange = ((toRPM * 100.0) / fromRPM) - 100.0;

      this->Update_Text_PercentChange();
      this->Update_Slider_PercentChange();
   }
}

