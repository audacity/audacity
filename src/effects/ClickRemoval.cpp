/**********************************************************************

  Audacity: A Digital Audio Editor

  ClickRemoval.cpp

  Craig DeForest

*******************************************************************//**

\class EffectClickRemoval
\brief An Effect for removing clicks.

  Clicks are identified as small regions of high amplitude compared
  to the surrounding chunk of sound.  Anything sufficiently tall compared
  to a large (2048 sample) window around it, and sufficiently narrow,
  is considered to be a click.

  The structure was largely stolen from Domonic Mazzoni's NoiseRemoval
  module, and reworked for the NEW effect.

  This file is intended to become part of Audacity.  You may modify
  and/or distribute it under the same terms as Audacity itself.

*//*******************************************************************/

#include "../Audacity.h"
#include "ClickRemoval.h"

#include <math.h>

#include <wx/intl.h>
#include <wx/msgdlg.h>
#include <wx/valgen.h>

#include "../Prefs.h"
#include "../ShuttleGui.h"
#include "../widgets/valnum.h"

#include "../WaveTrack.h"

enum
{
   ID_Thresh = 10000,
   ID_Width
};

// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name       Type     Key               Def      Min      Max      Scale
Param( Threshold, int,     XO("Threshold"),  200,     0,       900,     1  );
Param( Width,     int,     XO("Width"),      20,      0,       40,      1  );

BEGIN_EVENT_TABLE(EffectClickRemoval, wxEvtHandler)
    EVT_SLIDER(ID_Thresh, EffectClickRemoval::OnThreshSlider)
    EVT_SLIDER(ID_Width, EffectClickRemoval::OnWidthSlider)
    EVT_TEXT(ID_Thresh, EffectClickRemoval::OnThreshText)
    EVT_TEXT(ID_Width, EffectClickRemoval::OnWidthText)
END_EVENT_TABLE()

EffectClickRemoval::EffectClickRemoval()
{
   mThresholdLevel = DEF_Threshold;
   mClickWidth = DEF_Width;

   SetLinearEffectFlag(false);

   windowSize = 8192;
   sep = 2049;
}

EffectClickRemoval::~EffectClickRemoval()
{
}

// IdentInterface implementation

wxString EffectClickRemoval::GetSymbol()
{
   return CLICKREMOVAL_PLUGIN_SYMBOL;
}

wxString EffectClickRemoval::GetDescription()
{
   return XO("Click Removal is designed to remove clicks on audio tracks");
}

// EffectIdentInterface implementation

EffectType EffectClickRemoval::GetType()
{
   return EffectTypeProcess;
}

// EffectClientInterface implementation

bool EffectClickRemoval::GetAutomationParameters(EffectAutomationParameters & parms)
{
   parms.Write(KEY_Threshold, mThresholdLevel);
   parms.Write(KEY_Width, mClickWidth);

   return true;
}

bool EffectClickRemoval::SetAutomationParameters(EffectAutomationParameters & parms)
{
   ReadAndVerifyInt(Threshold);
   ReadAndVerifyInt(Width);

   mThresholdLevel = Threshold;
   mClickWidth = Width;

   return true;
}

// Effect implementation

bool EffectClickRemoval::CheckWhetherSkipEffect()
{
   return ((mClickWidth == 0) || (mThresholdLevel == 0));
}

bool EffectClickRemoval::Startup()
{
   wxString base = wxT("/Effects/ClickRemoval/");

   // Migrate settings from 2.1.0 or before

   // Already migrated, so bail
   if (gPrefs->Exists(base + wxT("Migrated")))
   {
      return true;
   }

   // Load the old "current" settings
   if (gPrefs->Exists(base))
   {
      mThresholdLevel = gPrefs->Read(base + wxT("ClickThresholdLevel"), 200);
      if ((mThresholdLevel < MIN_Threshold) || (mThresholdLevel > MAX_Threshold))
      {  // corrupted Prefs?
         mThresholdLevel = 0;  //Off-skip
      }
      mClickWidth = gPrefs->Read(base + wxT("ClickWidth"), 20);
      if ((mClickWidth < MIN_Width) || (mClickWidth > MAX_Width))
      {  // corrupted Prefs?
         mClickWidth = 0;  //Off-skip
      }

      SaveUserPreset(GetCurrentSettingsGroup());

      // Do not migrate again
      gPrefs->Write(base + wxT("Migrated"), true);
      gPrefs->Flush();
   }

   return true;
}

bool EffectClickRemoval::Process()
{
   this->CopyInputTracks(); // Set up mOutputTracks.
   bool bGoodResult = true;
   mbDidSomething = false;

   SelectedTrackListOfKindIterator iter(Track::Wave, mOutputTracks.get());
   WaveTrack *track = (WaveTrack *) iter.First();
   int count = 0;
   while (track) {
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();
      double t0 = mT0 < trackStart? trackStart: mT0;
      double t1 = mT1 > trackEnd? trackEnd: mT1;

      if (t1 > t0) {
         auto start = track->TimeToLongSamples(t0);
         auto end = track->TimeToLongSamples(t1);
         auto len = end - start;

         if (!ProcessOne(count, track, start, len))
         {
            bGoodResult = false;
            break;
         }
      }

      track = (WaveTrack *) iter.Next();
      count++;
   }
   if (bGoodResult && !mbDidSomething) // Processing successful, but ineffective.
      wxMessageBox(
         wxString::Format(_("Algorithm not effective on this audio. Nothing changed.")),
         GetName(),
         wxOK | wxICON_ERROR);

   this->ReplaceProcessedTracks(bGoodResult && mbDidSomething);
   return bGoodResult && mbDidSomething;
}

bool EffectClickRemoval::ProcessOne(int count, WaveTrack * track, sampleCount start, sampleCount len)
{
   if (len <= windowSize / 2)
   {
      wxMessageBox(
         wxString::Format(_("Selection must be larger than %d samples."),
                          windowSize / 2),
         GetName(),
         wxOK | wxICON_ERROR);
      return false;
   }

   auto idealBlockLen = track->GetMaxBlockSize() * 4;
   if (idealBlockLen % windowSize != 0)
      idealBlockLen += (windowSize - (idealBlockLen % windowSize));

   bool bResult = true;
   decltype(len) s = 0;
   float *buffer = new float[idealBlockLen];
   float *datawindow = new float[windowSize];
   while ((len - s) > windowSize / 2)
   {
      auto block = limitSampleBufferSize( idealBlockLen, len - s );

      track->Get((samplePtr) buffer, floatSample, start + s, block);

      for (decltype(block) i = 0; i + windowSize / 2 < block; i += windowSize / 2)
      {
         auto wcopy = std::min( windowSize, block - i );

         for(decltype(wcopy) j = 0; j < wcopy; j++)
            datawindow[j] = buffer[i+j];
         for(auto j = wcopy; j < windowSize; j++)
            datawindow[j] = 0;

         mbDidSomething |= RemoveClicks(windowSize, datawindow);

         for(decltype(wcopy) j = 0; j < wcopy; j++)
           buffer[i+j] = datawindow[j];
      }

      if (mbDidSomething) // RemoveClicks() actually did something.
         track->Set((samplePtr) buffer, floatSample, start + s, block);

      s += block;

      if (TrackProgress(count, s.as_double() /
                               len.as_double())) {
         bResult = false;
         break;
      }
   }

   delete[] buffer;
   delete[] datawindow;

   return bResult;
}

bool EffectClickRemoval::RemoveClicks(int len, float *buffer)
{
   bool bResult = false; // This effect usually does nothing.
   int i;
   int j;
   int left = 0;

   float msw;
   int ww;
   int s2 = sep/2;
   float *ms_seq = new float[len];
   float *b2 = new float[len];

   for( i=0; i<len; i++)
      b2[i] = buffer[i]*buffer[i];

   /* Shortcut for rms - multiple passes through b2, accumulating
    * as we go.
    */
   for(i=0;i<len;i++)
      ms_seq[i]=b2[i];

   for(i=1; i < sep; i *= 2) {
      for(j=0;j<len-i; j++)
         ms_seq[j] += ms_seq[j+i];
      }

      /* Cheat by truncating sep to next-lower power of two... */
      sep = i;

      for( i=0; i<len-sep; i++ ) {
         ms_seq[i] /= sep;
      }
      /* ww runs from about 4 to mClickWidth.  wrc is the reciprocal;
       * chosen so that integer roundoff doesn't clobber us.
       */
      int wrc;
      for(wrc=mClickWidth/4; wrc>=1; wrc /= 2) {
         ww = mClickWidth/wrc;

         for( i=0; i<len-sep; i++ ){
            msw = 0;
            for( j=0; j<ww; j++) {
               msw += b2[i+s2+j];
            }
            msw /= ww;

            if(msw >= mThresholdLevel * ms_seq[i]/10) {
               if( left == 0 ) {
                  left = i+s2;
               }
            } else {
               if(left != 0 && i-left+s2 <= ww*2) {
                  float lv = buffer[left];
                  float rv = buffer[i+ww+s2];
                  for(j=left; j<i+ww+s2; j++) {
                     bResult = true;
                     buffer[j]= (rv*(j-left) + lv*(i+ww+s2-j))/(float)(i+ww+s2-left);
                     b2[j] = buffer[j]*buffer[j];
                  }
                  left=0;
               } else if(left != 0) {
               left = 0;
            }
         }
      }
   }
   delete[] ms_seq;
   delete[] b2;
   return bResult;
}

void EffectClickRemoval::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);
   S.SetBorder(10);

   S.StartMultiColumn(3, wxEXPAND);
   S.SetStretchyCol(2);
   {
      // Threshold
      IntegerValidator<int> vldThresh(&mThresholdLevel);
      vldThresh.SetRange(MIN_Threshold, MAX_Threshold);
      mThreshT = S.Id(ID_Thresh).AddTextBox(_("Threshold (lower is more sensitive):"),
                                            wxT(""),
                                            10);
      mThreshT->SetValidator(vldThresh);

      S.SetStyle(wxSL_HORIZONTAL);
      mThreshS = S.Id(ID_Thresh).AddSlider(wxT(""), mThresholdLevel, MAX_Threshold, MIN_Threshold);
      mThreshS->SetName(_("Threshold"));
      mThreshS->SetValidator(wxGenericValidator(&mThresholdLevel));
      mThreshS->SetMinSize(wxSize(150, -1));

      // Click width
      IntegerValidator<int> vldWidth(&mClickWidth);
      vldWidth.SetRange(MIN_Width, MAX_Width);
      mWidthT = S.Id(ID_Width).AddTextBox(_("Max Spike Width (higher is more sensitive):"),
                                          wxT(""),
                                          10);
      mWidthT->SetValidator(vldWidth);

      S.SetStyle(wxSL_HORIZONTAL);
      mWidthS = S.Id(ID_Width).AddSlider(wxT(""), mClickWidth, MAX_Width, MIN_Width);
      mWidthS->SetName(_("Max Spike Width"));
      mWidthS->SetValidator(wxGenericValidator(&mClickWidth));
      mWidthS->SetMinSize(wxSize(150, -1));
   }
   S.EndMultiColumn();

   return;
}

bool EffectClickRemoval::TransferDataToWindow()
{
   if (!mUIParent->TransferDataToWindow())
   {
      return false;
   }

   return true;
}

bool EffectClickRemoval::TransferDataFromWindow()
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }

   return true;
}

void EffectClickRemoval::OnWidthText(wxCommandEvent & WXUNUSED(evt))
{
   mWidthT->GetValidator()->TransferFromWindow();
   mWidthS->GetValidator()->TransferToWindow();
}

void EffectClickRemoval::OnThreshText(wxCommandEvent & WXUNUSED(evt))
{
   mThreshT->GetValidator()->TransferFromWindow();
   mThreshS->GetValidator()->TransferToWindow();
}

void EffectClickRemoval::OnWidthSlider(wxCommandEvent & WXUNUSED(evt))
{
   mWidthS->GetValidator()->TransferFromWindow();
   mWidthT->GetValidator()->TransferToWindow();
}

void EffectClickRemoval::OnThreshSlider(wxCommandEvent & WXUNUSED(evt))
{
   mThreshS->GetValidator()->TransferFromWindow();
   mThreshT->GetValidator()->TransferToWindow();
}
