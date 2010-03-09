/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangeSpeed.cpp

  Vaughan Johnson, Dominic Mazzoni
  
*******************************************************************//**

\class EffectChangeSpeed
\brief An Effect that affects both pitch & Speed.

*//****************************************************************//**

\class ChangeSpeedDialog
\brief Dialog used with EffectChangeSpeed

*//*******************************************************************/


#include "../Audacity.h"

#include "ChangeSpeed.h"

#include "../ShuttleGui.h"
#include "../Envelope.h"
#include "../Prefs.h"
#include "../Project.h"
#include "TimeWarper.h"

#include <math.h>

#include <wx/button.h>
#include <wx/msgdlg.h> // for wxMessageBox
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/valtext.h>


//
// EffectChangeSpeed
//

EffectChangeSpeed::EffectChangeSpeed()
{
   // control values
   m_PercentChange = 0.0;
   m_FromVinyl = 0; 
   m_ToVinyl = 0; 
}

wxString EffectChangeSpeed::GetEffectDescription() { 
   // Note: This is useful only after change amount has been set. 
   return wxString::Format(_("Applied effect: %s %.1f%%"), 
                           this->GetEffectName().c_str(), 
                           m_PercentChange); 
} 

bool EffectChangeSpeed::PromptUser()
{
   ChangeSpeedDialog dlog(this, mParent);
   dlog.m_PercentChange = m_PercentChange;
   dlog.m_FromVinyl = m_FromVinyl;
   dlog.m_ToVinyl = m_ToVinyl;
   // Don't need to call TransferDataToWindow, although other 
   // Audacity dialogs (from which I derived this one) do it, because 
   // ShowModal calls stuff that eventually calls wxWindowBase::OnInitDialog, 
   // which calls dlog.TransferDataToWindow();
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (dlog.GetReturnCode() == wxID_CANCEL)
      return false;

   m_PercentChange = dlog.m_PercentChange;
   m_FromVinyl = dlog.m_FromVinyl;
   m_ToVinyl = dlog.m_ToVinyl;

   return true;
}
bool EffectChangeSpeed::TransferParameters( Shuttle & shuttle )
{  
   shuttle.TransferDouble(wxT("Percentage"),m_PercentChange,0.0);
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

   //Iterate over each track
   //Track::All is needed because this effect needs to introduce silence in the group tracks to keep sync
   this->CopyInputTracks(Track::All); // Set up mOutputTracks.
   bool bGoodResult = true;

   TrackListIterator iter(mOutputTracks);
   Track* t;
   mCurTrackNum = 0;
   m_maxNewLength = 0.0;

   mFactor = 100.0 / (100.0 + m_PercentChange);

   t = iter.First();
   while (t != NULL)
   {
      if (t->GetKind() == Track::Label) {
         if (t->GetSelected() || t->IsSynchroSelected())
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
      else if (t->IsSynchroSelected())
      {
         t->SyncAdjust(mT1, mT0 + (mT1 - mT0) * mFactor);
      }

      //Iterate to the next track
      t=iter.Next();
   }

   if (bGoodResult)
      ReplaceProcessedTracks(bGoodResult);

   mT1 = mT0 + m_maxNewLength; // Update selection.

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
   Resample resample(true, mFactor, mFactor);

   //Go through the track one buffer at a time. samplePos counts which
   //sample the current buffer starts at.
   bool bLoopSuccess = true;
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
         bLoopSuccess = false;
         break;
      }

      if (outgen > 0)
         outputTrack->Append((samplePtr)outBuffer, floatSample, 
                             outgen);

      // Increment samplePos
      samplePos += inUsed;

      // Update the Progress meter
      if (TrackProgress(mCurTrackNum, (samplePos - start) / len)) {
         bLoopSuccess = false;
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
   if (bLoopSuccess) {
      SetTimeWarper(new LinearTimeWarper(
                       mCurT0, mCurT0, mCurT1, mCurT0 + newLength ));
      track->ClearAndPaste(mCurT0, mCurT1, outputTrack, true, false,
                           GetTimeWarper());
   }

   if (newLength > m_maxNewLength) 
      m_maxNewLength = newLength; 

   // Delete the outputTrack now that its data is inserted in place
   delete outputTrack;

   return bLoopSuccess;
}


//----------------------------------------------------------------------------
// ChangeSpeedDialog
//----------------------------------------------------------------------------

// -99 for PERCENTCHANGE_MIN because -100% is nonsensical.
#define PERCENTCHANGE_MIN -99

#define PERCENTCHANGE_MAX 100 // warped above zero to actually go up to 400%
#define PERCENTCHANGE_SLIDER_WARP 1.30105 // warp power takes max from 100 to 400.

// the standard vinyl RPM choices
// If the percent change is not one of these ratios, the choice control gets "n/a".
#define CHOICE_33ANDATHIRD 0
#define CHOICE_45 1
#define CHOICE_78 2
#define CHOICE_NA 3 

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
:  EffectDialog(parent, _("Change Speed"), PROCESS_EFFECT),
   mEffect(effect)
{
   m_bLoopDetect = false;

   // NULL out these control members because there are some cases where the 
   // event table handlers get called during this method, and those handlers that 
   // can cause trouble check for NULL.
   m_pTextCtrl_PercentChange = NULL;
   m_pSlider_PercentChange = NULL;
   m_pChoice_FromVinyl = NULL;
   m_pChoice_ToVinyl = NULL;

   // effect parameters
   m_PercentChange = 0.0;
   m_FromVinyl = 0; 
   m_ToVinyl = 0; 

   Init();
}

void ChangeSpeedDialog::PopulateOrExchange(ShuttleGui & S)
{
   wxTextValidator numvld(wxFILTER_NUMERIC);
   wxArrayString rpm;
   
   rpm.Add(wxT("33 1/3"));
   rpm.Add(wxT("45"));
   rpm.Add(wxT("78"));
  /* i18n-hint: n/a is an English abbreviation meaning "not applicable"
     (in other words, unimportant, not relevant). */
   rpm.Add(_("n/a"));

   S.SetBorder(10);
   S.StartHorizontalLay(wxCENTER, false);
   {
      S.AddTitle(_("Change Speed, affecting both Tempo and Pitch") +
                 wxString(wxT("\n\n")) +
                 _("by Vaughan Johnson && Dominic Mazzoni") +
                 wxString(wxT("\n")) +
                 _("using SampleRate, by Erik de Castro Lopo"));
   }
   S.EndHorizontalLay();
   S.SetBorder(5);

   //
   S.StartMultiColumn(2, wxCENTER);
   {
      m_pTextCtrl_PercentChange = S.Id(ID_TEXT_PERCENTCHANGE)
         .AddTextBox(_("Percent Change:"), wxT(""), 12);
      m_pTextCtrl_PercentChange->SetValidator(numvld);
   }
   S.EndMultiColumn();

   //
   S.StartHorizontalLay(wxEXPAND);
   {
      S.SetStyle(wxSL_HORIZONTAL);
      m_pSlider_PercentChange = S.Id(ID_SLIDER_PERCENTCHANGE)
         .AddSlider(wxT(""), 0, (int)PERCENTCHANGE_MAX, (int)PERCENTCHANGE_MIN);
      m_pSlider_PercentChange->SetName(_("Percent Change"));
   }
   S.EndHorizontalLay();

   // 
   S.StartMultiColumn(5, wxCENTER);
   {
      //
      S.AddUnits(_("Standard Vinyl RPM:"));

      m_pChoice_FromVinyl = S.Id(ID_CHOICE_FROMVINYL)
         .AddChoice(_("from"), wxT(""), &rpm);
      m_pChoice_FromVinyl->SetName(_("From RPM"));
      m_pChoice_FromVinyl->SetSizeHints(100, -1);

      m_pChoice_ToVinyl = S.Id(ID_CHOICE_TOVINYL)
         .AddChoice(_("to"), wxT(""), &rpm);
      m_pChoice_ToVinyl->SetName(_("To RPM"));
      m_pChoice_ToVinyl->SetSizeHints(100, -1);
   }
   S.EndMultiColumn();

   return;
}

bool ChangeSpeedDialog::TransferDataToWindow()
{
   m_bLoopDetect = true;

   // percent change controls
   this->Update_Text_PercentChange();
   this->Update_Slider_PercentChange();

   // from/to Vinyl controls
   if (m_pChoice_FromVinyl) 
      m_pChoice_FromVinyl->SetSelection(m_FromVinyl);

   if (m_pChoice_ToVinyl) 
      m_pChoice_ToVinyl->SetSelection(m_ToVinyl);

   m_bLoopDetect = false;

   return true;
}

bool ChangeSpeedDialog::TransferDataFromWindow()
{
   wxString str;

   // percent change controls
   if (m_pTextCtrl_PercentChange) {
      double newValue = 0;
      str = m_pTextCtrl_PercentChange->GetValue();
      str.ToDouble(&newValue);
      m_PercentChange = newValue;
   }

   // Ignore Slider_PercentChange because TextCtrl_PercentChange 
   // always tracks it & is more precise (decimal points).


   // from/to Vinyl controls
   if (m_pChoice_FromVinyl) 
      m_FromVinyl = m_pChoice_FromVinyl->GetSelection();

   if (m_pChoice_ToVinyl) 
      m_ToVinyl = m_pChoice_ToVinyl->GetSelection();

   return true;
}

// handler implementations for ChangeSpeedDialog

void ChangeSpeedDialog::OnText_PercentChange(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   if (m_pTextCtrl_PercentChange) {
      double newValue = 0;
      wxString str = m_pTextCtrl_PercentChange->GetValue();
      str.ToDouble(&newValue);
      m_PercentChange = newValue;

      m_bLoopDetect = true;
      this->Update_Slider_PercentChange();
      this->Update_Vinyl();
      m_bLoopDetect = false;

      FindWindow(wxID_OK)->Enable(m_PercentChange > -100.0);
   }
}

void ChangeSpeedDialog::OnSlider_PercentChange(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   if (m_pSlider_PercentChange) {
      m_PercentChange = (double)(m_pSlider_PercentChange->GetValue()); 
      // Warp positive values to actually go up faster & further than negatives.
      if (m_PercentChange > 0.0)
         m_PercentChange = pow(m_PercentChange, PERCENTCHANGE_SLIDER_WARP);

      m_bLoopDetect = true;
      this->Update_Text_PercentChange();
      this->Update_Vinyl();
      m_bLoopDetect = false;
   }
}

void ChangeSpeedDialog::OnChoice_FromVinyl(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   if (m_pChoice_FromVinyl) {
      m_FromVinyl = m_pChoice_FromVinyl->GetSelection();

      m_bLoopDetect = true;
      this->Update_PercentChange();
      m_bLoopDetect = false;
   }
}

void ChangeSpeedDialog::OnChoice_ToVinyl(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   if (m_pChoice_ToVinyl) {
      m_ToVinyl = m_pChoice_ToVinyl->GetSelection();

      m_bLoopDetect = true;
      this->Update_PercentChange();
      m_bLoopDetect = false;
   }
}

void ChangeSpeedDialog::OnPreview(wxCommandEvent &event)
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
   if (m_pTextCtrl_PercentChange) {
      wxString str;
      str.Printf(wxT("%.3f"), m_PercentChange);
      m_pTextCtrl_PercentChange->SetValue(str);
      FindWindow(wxID_OK)->Enable(m_PercentChange > -100.0);
   }
}

void ChangeSpeedDialog::Update_Slider_PercentChange()
{
   if (m_pSlider_PercentChange) {
      double unwarped = m_PercentChange;
      if (unwarped > 0.0)
         // Un-warp values above zero to actually go up to PERCENTCHANGE_MAX.
         unwarped = pow(m_PercentChange, (1.0 / PERCENTCHANGE_SLIDER_WARP));

      // Add 0.5 to unwarped so trunc -> round.
      m_pSlider_PercentChange->SetValue((int)(unwarped + 0.5)); 
   }
}

void ChangeSpeedDialog::Update_Vinyl() 
// Update Vinyl controls for new percent change.
{
   if (m_pChoice_ToVinyl) {
      // Chances are so low that the slider will exactly match a 
      // standard ratio, just turn it "n/a" unless it's 0.0.
      if ((m_PercentChange == 0.0) && m_pChoice_FromVinyl)
         m_pChoice_ToVinyl->SetSelection(m_pChoice_FromVinyl->GetSelection());
      else
         m_pChoice_ToVinyl->SetSelection(CHOICE_NA);
        }
}

void ChangeSpeedDialog::Update_PercentChange() 
// Update percent change controls for new Vinyl values.
{
   // If m_FromVinyl & m_ToVinyl are set, then there's a new percent change.
   if ((m_FromVinyl != CHOICE_NA) && (m_ToVinyl != CHOICE_NA)) {
      double fromRPM;
      double toRPM;
      switch (m_FromVinyl) {
      default:
      case CHOICE_33ANDATHIRD:   fromRPM = 33.0 + (1.0 / 3.0); break;
      case CHOICE_45:            fromRPM = 45.0; break;
      case CHOICE_78:            fromRPM = 78; break;
      }
      switch (m_ToVinyl) {
      default:
      case CHOICE_33ANDATHIRD:   toRPM = 33.0 + (1.0 / 3.0); break;
      case CHOICE_45:            toRPM = 45.0; break;
      case CHOICE_78:            toRPM = 78; break;
      }
      m_PercentChange = ((toRPM * 100.0) / fromRPM) - 100.0;

      this->Update_Text_PercentChange();
      this->Update_Slider_PercentChange();
   }
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 84775e31-325f-46fd-bcec-7336b0a574e1

