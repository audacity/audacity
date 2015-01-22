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
#include "../LabelTrack.h"
#include "../Envelope.h"
#include "../LabelTrack.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../ShuttleGui.h"

#include "ChangeSpeed.h"
#include "TimeWarper.h"


// the standard vinyl rpm choices
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
   // Retrieve last used control values
   gPrefs->Read(wxT("/Effects/ChangeSpeed/PercentChange"), &m_PercentChange, 0);
   // default format "4" is the same as the Selection toolbar: "hh:mm:ss + milliseconds";
   gPrefs->Read(wxT("/Effects/ChangeSpeed/TimeFormat"), &mTimeCtrlFormat, 4);

   gPrefs->Read(wxT("/Effects/ChangeSpeed/VinylChoice"), &mFromVinyl, 0);
   if (mFromVinyl == kVinyl_NA) {
      mFromVinyl = kVinyl_33AndAThird;
   }
}

wxString EffectChangeSpeed::GetEffectDescription() {
   // Note: This is useful only after change amount has been set.
   return wxString::Format(_("Applied effect: %s %.3f%%"),
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
   dlog.mFromLength = mFromLength;
   dlog.mTimeCtrlFormat = mTimeCtrlFormat;
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
   mTimeCtrlFormat = dlog.mTimeCtrlFormat;

   gPrefs->Write(wxT("/Effects/ChangeSpeed/PercentChange"), m_PercentChange);
   gPrefs->Write(wxT("/Effects/ChangeSpeed/TimeFormat"), mTimeCtrlFormat);
   gPrefs->Flush();
   return true;
}

bool EffectChangeSpeed::Init()
{
   // The selection might have changed since the last time EffectChangeSpeed
   // was invoked, so recalculate the Length parameters.
   mFromLength = mT1 - mT0;
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

   // Update selection.
   mT1 = mT0 + (((mT1 - mT0) * 100.0) / (100.0 + m_PercentChange));

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
#define PERCENTCHANGE_MAX_TEXT 4900 // maximum allowed by text entry (= 50 times)
#define PERCENTCHANGE_SLIDER_WARP 1.30105 // warp power takes max from 100 to 400.

enum {
   ID_TEXT_PERCENTCHANGE = 10001,
   ID_TEXT_MULTIPLIER,
   ID_SLIDER_PERCENTCHANGE,
   ID_CHOICE_FROMVINYL,
   ID_CHOICE_TOVINYL,
   ID_TIMECTRL_TOLENGTH
};


// event table for ChangeSpeedDialog

BEGIN_EVENT_TABLE(ChangeSpeedDialog, EffectDialog)
    EVT_TEXT(ID_TEXT_PERCENTCHANGE, ChangeSpeedDialog::OnText_PercentChange)
    EVT_TEXT(ID_TEXT_MULTIPLIER, ChangeSpeedDialog::OnText_Multiplier)
    EVT_SLIDER(ID_SLIDER_PERCENTCHANGE, ChangeSpeedDialog::OnSlider_PercentChange)
    EVT_CHOICE(ID_CHOICE_FROMVINYL, ChangeSpeedDialog::OnChoice_Vinyl)
    EVT_CHOICE(ID_CHOICE_TOVINYL, ChangeSpeedDialog::OnChoice_Vinyl)
    EVT_TEXT(ID_TIMECTRL_TOLENGTH, ChangeSpeedDialog::OnTimeCtrl_ToLength)
    EVT_COMMAND(ID_TIMECTRL_TOLENGTH, EVT_TIMETEXTCTRL_UPDATED, ChangeSpeedDialog::OnTimeCtrlUpdate)

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

   // effect parameters
   m_PercentChange = 0.0;
   mFromVinyl = kVinyl_33AndAThird;
   mToVinyl = kVinyl_33AndAThird;
   mFromLength = 0.0;
   mToLength = 0.0;
   mFormat = wxT("");
   mTimeCtrlFormat = 0;

   Init();
}

void ChangeSpeedDialog::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(5);

   S.AddSpace(0, 5);
   S.AddTitle(_("Change Speed, affecting both Tempo and Pitch"));
   S.AddSpace(0, 10);

   // Speed multiplier and percent change controls.
   S.StartMultiColumn(4, wxLEFT);
   {
      wxTextValidator validator(wxFILTER_NUMERIC);
      mpTextCtrl_Multiplier =
         S.Id(ID_TEXT_MULTIPLIER).AddTextBox(_("Speed Multiplier:"), wxT(""), 12);
      mpTextCtrl_Multiplier->SetValidator(validator);

      mpTextCtrl_PercentChange =
         S.Id(ID_TEXT_PERCENTCHANGE).AddTextBox(_("Percent Change:"), wxT(""), 12);
      mpTextCtrl_PercentChange->SetValidator(validator);
   }
   S.EndMultiColumn();

   // Percent change slider.
   S.StartHorizontalLay(wxEXPAND);
   {
      S.SetStyle(wxSL_HORIZONTAL);
      mpSlider_PercentChange =
         S.Id(ID_SLIDER_PERCENTCHANGE).AddSlider(wxT(""), 0, (int)PERCENTCHANGE_MAX, (int)PERCENTCHANGE_MIN);
      mpSlider_PercentChange->SetName(_("Percent Change"));
   }
   S.EndHorizontalLay();

   // Vinyl rpm controls.
   S.StartMultiColumn(5, wxCENTER);
   {
      /* i18n-hint: "rpm" is an English abbreviation meaning "revolutions per minute". */
      S.AddUnits(_("Standard Vinyl rpm:"));

      wxArrayString rpmStrings;
      rpmStrings.Add(wxT("33 1/3"));
      rpmStrings.Add(wxT("45"));
      rpmStrings.Add(wxT("78"));
      /* i18n-hint: n/a is an English abbreviation meaning "not applicable". */
      rpmStrings.Add(_("n/a"));

      mpChoice_FromVinyl =
         S.Id(ID_CHOICE_FROMVINYL).AddChoice(_("from"), wxT(""), &rpmStrings);
      mpChoice_FromVinyl->SetName(_("From rpm"));
      mpChoice_FromVinyl->SetSizeHints(100, -1);

      mpChoice_ToVinyl =
         S.Id(ID_CHOICE_TOVINYL).AddChoice(_("to"), wxT(""), &rpmStrings);
      mpChoice_ToVinyl->SetName(_("To rpm"));
      mpChoice_ToVinyl->SetSizeHints(100, -1);
   }
   S.EndMultiColumn();

   // From/To time controls.
   S.StartStatic(_("Selection Length"), 0);
   {
      S.StartMultiColumn(2, wxLEFT);
      {
         S.AddPrompt(_("Current Length") + wxString(wxT(":")));

         mpFromLengthCtrl = new
               NumericTextCtrl(NumericConverter::TIME, this,
                            wxID_ANY,
                            mFormat,
                            mFromLength,
                            mEffect->mProjectRate);

         mpFromLengthCtrl->SetName(_("from"));
         S.AddWindow(mpFromLengthCtrl, wxALIGN_LEFT);
#if wxUSE_TOOLTIPS
         wxString tip(_("Current length of selection."));
         mpFromLengthCtrl->SetToolTip(tip);
#endif
         mpFromLengthCtrl->SetReadOnly(true);
         mpFromLengthCtrl->EnableMenu(false);


         S.AddPrompt(_("New Length") + wxString(wxT(":")));

         mpToLengthCtrl = new
               NumericTextCtrl(NumericConverter::TIME, this,
                            ID_TIMECTRL_TOLENGTH,
                            mFormat,
                            mToLength,
                            mEffect->mProjectRate);

         mpToLengthCtrl->SetName(_("to"));
         S.AddWindow(mpToLengthCtrl, wxALIGN_LEFT);
         mpToLengthCtrl->EnableMenu();
      }
      S.EndMultiColumn();
   }
   S.EndStatic();
}

bool ChangeSpeedDialog::TransferDataToWindow()
{
   mbLoopDetect = true;

   this->Update_Text_PercentChange();
   this->Update_Text_Multiplier();
   this->Update_Slider_PercentChange();
   this->Update_TimeCtrl_ToLength();

   // Set from/to Vinyl controls - mFromVinyl must be set first.
   mpChoice_FromVinyl->SetSelection(mFromVinyl);
   // Then update to get correct mToVinyl.
   this->Update_Vinyl();
   // Then update ToVinyl control.
   mpChoice_ToVinyl->SetSelection(mToVinyl);

   // Set From Length control.
   // Set the format first so we can get sample accuracy.
   mpFromLengthCtrl->SetFormatName(mFormat);
   mpFromLengthCtrl->SetValue(mFromLength);

   mbLoopDetect = false;

   return true;
}

bool ChangeSpeedDialog::Validate()
{
   TransferDataFromWindow();
   m_PercentChange = TrapDouble(m_PercentChange, PERCENTCHANGE_MIN, PERCENTCHANGE_MAX_TEXT);
   return true;
}


// handler implementations for ChangeSpeedDialog

void ChangeSpeedDialog::OnText_PercentChange(wxCommandEvent & WXUNUSED(event))
{
   if (mbLoopDetect)
      return;

   double newValue = 0;
   wxString str = mpTextCtrl_PercentChange->GetValue();
   str.ToDouble(&newValue);
   m_PercentChange = newValue;
   this->UpdateUI();

   mbLoopDetect = true;
   this->Update_Text_Multiplier();
   this->Update_Slider_PercentChange();
   this->Update_Vinyl();
   this->Update_TimeCtrl_ToLength();
   mbLoopDetect = false;
}

void ChangeSpeedDialog::OnText_Multiplier(wxCommandEvent & WXUNUSED(event))
{
   if (mbLoopDetect)
      return;

   double newValue = 0;
   wxString str = mpTextCtrl_Multiplier->GetValue();
   str.ToDouble(&newValue);
   m_PercentChange = 100 * (newValue - 1);
   this->UpdateUI();

   mbLoopDetect = true;
   this->Update_Text_PercentChange();
   this->Update_Slider_PercentChange();
   this->Update_Vinyl();
   this->Update_TimeCtrl_ToLength();
   mbLoopDetect = false;
}

void ChangeSpeedDialog::OnSlider_PercentChange(wxCommandEvent & WXUNUSED(event))
{
   if (mbLoopDetect)
      return;

   m_PercentChange = (double)(mpSlider_PercentChange->GetValue());
   // Warp positive values to actually go up faster & further than negatives.
   if (m_PercentChange > 0.0)
      m_PercentChange = pow(m_PercentChange, PERCENTCHANGE_SLIDER_WARP);
   this->UpdateUI();

   mbLoopDetect = true;
   this->Update_Text_PercentChange();
   this->Update_Text_Multiplier();
   this->Update_Vinyl();
   this->Update_TimeCtrl_ToLength();
   mbLoopDetect = false;
}

void ChangeSpeedDialog::OnChoice_Vinyl(wxCommandEvent & WXUNUSED(event))
{
   // Treat mpChoice_FromVinyl and mpChoice_ToVinyl as one control since we need
   // both to calculate Percent Change.
   mFromVinyl = mpChoice_FromVinyl->GetSelection();
   mToVinyl = mpChoice_ToVinyl->GetSelection();
   // Use this as the 'preferred' choice.
   if (mFromVinyl != kVinyl_NA) {
      gPrefs->Write(wxT("/Effects/ChangeSpeed/VinylChoice"), mFromVinyl);
      gPrefs->Flush();
   }

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
      this->UpdateUI();

      mbLoopDetect = true;
      this->Update_Text_PercentChange();
      this->Update_Text_Multiplier();
      this->Update_Slider_PercentChange();
      this->Update_TimeCtrl_ToLength();
   }
   mbLoopDetect = false;
}

void ChangeSpeedDialog::OnTimeCtrl_ToLength(wxCommandEvent & WXUNUSED(event))
{
   if (mbLoopDetect)
      return;

      mToLength = mpToLengthCtrl->GetValue();
      m_PercentChange = ((mFromLength * 100.0) / mToLength) - 100.0;
      this->UpdateUI();

      mbLoopDetect = true;

      this->Update_Text_PercentChange();
      this->Update_Text_Multiplier();
      this->Update_Slider_PercentChange();
      this->Update_Vinyl();

      mbLoopDetect = false;
}

void ChangeSpeedDialog::OnTimeCtrlUpdate(wxCommandEvent &evt)
{
   mTimeCtrlFormat = evt.GetInt();

   mFormat = mpToLengthCtrl->GetBuiltinName(mTimeCtrlFormat);
   mpFromLengthCtrl->SetFormatName(mFormat);
   // Update From/To Length controls (precision has changed).
   mpToLengthCtrl->SetValue(mToLength);
   mpFromLengthCtrl->SetValue(mFromLength);
}

void ChangeSpeedDialog::OnPreview(wxCommandEvent & WXUNUSED(event))
{
   TransferDataFromWindow();

   // Save & restore parameters around Preview, because we didn't do OK.
   double oldPercentChange = mEffect->m_PercentChange;

   // bug 773 allows effects to bypass greyed out OK button,
   // but validate() should now catch that.
   wxASSERT((m_PercentChange >= PERCENTCHANGE_MIN) && (m_PercentChange <= PERCENTCHANGE_MAX_TEXT));

   mEffect->m_PercentChange = m_PercentChange;
   mEffect->Preview();
   mEffect->m_PercentChange = oldPercentChange;
}

// helper functions

void ChangeSpeedDialog::Update_Text_PercentChange()
// Update Text Percent control from percent change.
{
   wxString str;
   str.Printf(wxT("%.3f"), m_PercentChange);
   mpTextCtrl_PercentChange->SetValue(str);
}

void ChangeSpeedDialog::Update_Text_Multiplier()
// Update Multiplier control from percent change.
{
   wxString str;
   str.Printf(wxT("%.3f"), 1 + (m_PercentChange) / 100.0);
   mpTextCtrl_Multiplier->SetValue(str);
}

void ChangeSpeedDialog::Update_Slider_PercentChange()
// Update Slider Percent control from percent change.
{
   double unwarped = m_PercentChange;
   if (unwarped > 0.0)
      // Un-warp values above zero to actually go up to PERCENTCHANGE_MAX.
      unwarped = pow(m_PercentChange, (1.0 / PERCENTCHANGE_SLIDER_WARP));

   // Add 0.5 to unwarped so trunc -> round.
   mpSlider_PercentChange->SetValue((int)(unwarped + 0.5));
}

void ChangeSpeedDialog::Update_Vinyl()
// Update Vinyl controls from percent change.
{
   // Match Vinyl rpm when within 0.01% of a standard ratio.
   // Ratios calculated as: ((toRPM / fromRPM) - 1) * 100 * 100
   int ratio = wxRound(m_PercentChange * 100);

   switch (ratio)
   {
      case 0: // toRPM is the same as fromRPM
         if (mFromVinyl != kVinyl_NA) {
            mpChoice_ToVinyl->SetSelection(mpChoice_FromVinyl->GetSelection());
         } else {
            // Use the last saved option.
            gPrefs->Read(wxT("/Effects/ChangeSpeed/VinylChoice"), &mFromVinyl, 0);
            mpChoice_FromVinyl->SetSelection(mFromVinyl);
            mpChoice_ToVinyl->SetSelection(mFromVinyl);
         }
         break;
      case 3500:
         mpChoice_FromVinyl->SetSelection(kVinyl_33AndAThird);
         mpChoice_ToVinyl->SetSelection(kVinyl_45);
         break;
      case 13400:
         mpChoice_FromVinyl->SetSelection(kVinyl_33AndAThird);
         mpChoice_ToVinyl->SetSelection(kVinyl_78);
         break;
      case -2593:
         mpChoice_FromVinyl->SetSelection(kVinyl_45);
         mpChoice_ToVinyl->SetSelection(kVinyl_33AndAThird);
         break;
      case 7333:
         mpChoice_FromVinyl->SetSelection(kVinyl_45);
         mpChoice_ToVinyl->SetSelection(kVinyl_78);
         break;
      case -5727:
         mpChoice_FromVinyl->SetSelection(kVinyl_78);
         mpChoice_ToVinyl->SetSelection(kVinyl_33AndAThird);
         break;
      case -4231:
         mpChoice_FromVinyl->SetSelection(kVinyl_78);
         mpChoice_ToVinyl->SetSelection(kVinyl_45);
         break;
      default:
         mpChoice_ToVinyl->SetSelection(kVinyl_NA);
   }
   // and update variables.
   mFromVinyl = mpChoice_FromVinyl->GetSelection();
   mToVinyl = mpChoice_ToVinyl->GetSelection();
}

void ChangeSpeedDialog::Update_TimeCtrl_ToLength()
// Update ToLength control from percent change.
{
   mToLength = (mFromLength * 100.0) / (100.0 + m_PercentChange);

   // Set the format first so we can get sample accuracy.
   mFormat = mpToLengthCtrl->GetBuiltinName(mTimeCtrlFormat);
   mpToLengthCtrl->SetFormatName(mFormat);
   // Negative times do not make sense.
   // 359999 = 99h:59m:59s which is a little less disturbing than overflow characters
   // though it may still look a bit strange with some formats.
   mToLength = TrapDouble(mToLength, 0.0, 359999.0);
   mpToLengthCtrl->SetValue(mToLength);
}

void ChangeSpeedDialog::UpdateUI()
// Disable OK and Preview if not in sensible range.
{
   bool enabled = (m_PercentChange < PERCENTCHANGE_MIN ||
                   m_PercentChange > PERCENTCHANGE_MAX_TEXT)? false : true;
   FindWindow(wxID_OK)->Enable(enabled);
   FindWindow(ID_EFFECT_PREVIEW)->Enable(enabled);
}
