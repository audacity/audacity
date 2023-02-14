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
#include "ClickRemoval.h"
#include "EffectEditor.h"
#include "LoadEffects.h"

#include <math.h>

#include <wx/slider.h>
#include <wx/valgen.h>

#include "Prefs.h"
#include "ShuttleGui.h"
#include "AudacityMessageBox.h"
#include "valnum.h"

#include "WaveTrack.h"

enum
{
   ID_Thresh = 10000,
   ID_Width
};

const EffectParameterMethods& EffectClickRemoval::Parameters() const
{
   static CapturedParameters<EffectClickRemoval,
      Threshold, Width
   > parameters;
   return parameters;
}

const ComponentInterfaceSymbol EffectClickRemoval::Symbol
{ XO("Click Removal") };

namespace{ BuiltinEffectsModule::Registration< EffectClickRemoval > reg; }

BEGIN_EVENT_TABLE(EffectClickRemoval, wxEvtHandler)
    EVT_SLIDER(ID_Thresh, EffectClickRemoval::OnThreshSlider)
    EVT_SLIDER(ID_Width, EffectClickRemoval::OnWidthSlider)
    EVT_TEXT(ID_Thresh, EffectClickRemoval::OnThreshText)
    EVT_TEXT(ID_Width, EffectClickRemoval::OnWidthText)
END_EVENT_TABLE()

EffectClickRemoval::EffectClickRemoval()
{
   Parameters().Reset(*this);

   SetLinearEffectFlag(false);

   windowSize = 8192;
   sep = 2049;
}

EffectClickRemoval::~EffectClickRemoval()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectClickRemoval::GetSymbol() const
{
   return Symbol;
}

TranslatableString EffectClickRemoval::GetDescription() const
{
   return XO("Click Removal is designed to remove clicks on audio tracks");
}

ManualPageID EffectClickRemoval::ManualPage() const
{
   return L"Click_Removal";
}

// EffectDefinitionInterface implementation

EffectType EffectClickRemoval::GetType() const
{
   return EffectTypeProcess;
}

// Effect implementation

bool EffectClickRemoval::CheckWhetherSkipEffect(const EffectSettings &) const
{
   return ((mClickWidth == 0) || (mThresholdLevel == 0));
}

bool EffectClickRemoval::Process(EffectInstance &, EffectSettings &)
{
   this->CopyInputTracks(); // Set up mOutputTracks.
   bool bGoodResult = true;
   mbDidSomething = false;

   int count = 0;
   for( auto track : mOutputTracks->Selected< WaveTrack >() ) {
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

      count++;
   }
   if (bGoodResult && !mbDidSomething) // Processing successful, but ineffective.
      EffectUIServices::DoMessageBox(*this,
         XO("Algorithm not effective on this audio. Nothing changed."),
         wxOK | wxICON_ERROR );

   this->ReplaceProcessedTracks(bGoodResult && mbDidSomething);
   return bGoodResult && mbDidSomething;
}

bool EffectClickRemoval::ProcessOne(int count, WaveTrack * track, sampleCount start, sampleCount len)
{
   if (len <= windowSize / 2)
   {
      EffectUIServices::DoMessageBox(*this,
         XO("Selection must be larger than %d samples.")
            .Format(windowSize / 2),
         wxOK | wxICON_ERROR );
      return false;
   }

   auto idealBlockLen = track->GetMaxBlockSize() * 4;
   if (idealBlockLen % windowSize != 0)
      idealBlockLen += (windowSize - (idealBlockLen % windowSize));

   bool bResult = true;
   decltype(len) s = 0;
   Floats buffer{ idealBlockLen };
   Floats datawindow{ windowSize };
   while ((len - s) > windowSize / 2)
   {
      auto block = limitSampleBufferSize( idealBlockLen, len - s );

      track->GetFloats(buffer.get(), start + s, block);

      for (decltype(block) i = 0; i + windowSize / 2 < block; i += windowSize / 2)
      {
         auto wcopy = std::min( windowSize, block - i );

         for(decltype(wcopy) j = 0; j < wcopy; j++)
            datawindow[j] = buffer[i+j];
         for(auto j = wcopy; j < windowSize; j++)
            datawindow[j] = 0;

         mbDidSomething |= RemoveClicks(windowSize, datawindow.get());

         for(decltype(wcopy) j = 0; j < wcopy; j++)
           buffer[i+j] = datawindow[j];
      }

      if (mbDidSomething) // RemoveClicks() actually did something.
         track->Set((samplePtr) buffer.get(), floatSample, start + s, block);

      s += block;

      if (TrackProgress(count, s.as_double() /
                               len.as_double())) {
         bResult = false;
         break;
      }
   }

   return bResult;
}

bool EffectClickRemoval::RemoveClicks(size_t len, float *buffer)
{
   bool bResult = false; // This effect usually does nothing.
   size_t i;
   size_t j;
   int left = 0;

   float msw;
   int ww;
   int s2 = sep/2;
   Floats ms_seq{ len };
   Floats b2{ len };

   for( i=0; i<len; i++)
      b2[i] = buffer[i]*buffer[i];

   /* Shortcut for rms - multiple passes through b2, accumulating
    * as we go.
    */
   for(i=0;i<len;i++)
      ms_seq[i]=b2[i];

   for(i=1; (int)i < sep; i *= 2) {
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
         for( j=0; (int)j<ww; j++) {
            msw += b2[i+s2+j];
         }
         msw /= ww;

         if(msw >= mThresholdLevel * ms_seq[i]/10) {
            if( left == 0 ) {
               left = i+s2;
            }
         } else {
            if(left != 0 && ((int)i-left+s2) <= ww*2) {
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
   return bResult;
}

std::unique_ptr<EffectEditor> EffectClickRemoval::PopulateOrExchange(
   ShuttleGui & S, EffectInstance &, EffectSettingsAccess &,
   const EffectOutputs *)
{
   mUIParent = S.GetParent();
   S.AddSpace(0, 5);
   S.SetBorder(10);

   S.StartMultiColumn(3, wxEXPAND);
   S.SetStretchyCol(2);
   {
      // Threshold
      mThreshT = S.Id(ID_Thresh)
         .Validator<IntegerValidator<int>>(
            &mThresholdLevel, NumValidatorStyle::DEFAULT,
            Threshold.min, Threshold.max )
         .AddTextBox(XXO("&Threshold (lower is more sensitive):"),
                     wxT(""),
                     10);

      mThreshS = S.Id(ID_Thresh)
         .Name(XO("Threshold"))
         .Style(wxSL_HORIZONTAL)
         .Validator<wxGenericValidator>(&mThresholdLevel)
         .MinSize( { 150, -1 } )
         .AddSlider( {}, mThresholdLevel, Threshold.max, Threshold.min);

      // Click width
      mWidthT = S.Id(ID_Width)
         .Validator<IntegerValidator<int>>(
            &mClickWidth, NumValidatorStyle::DEFAULT, Width.min, Width.max)
         .AddTextBox(XXO("Max &Spike Width (higher is more sensitive):"),
                     wxT(""),
                     10);

      mWidthS = S.Id(ID_Width)
         .Name(XO("Max Spike Width"))
         .Style(wxSL_HORIZONTAL)
         .Validator<wxGenericValidator>(&mClickWidth)
         .MinSize( { 150, -1 } )
         .AddSlider( {}, mClickWidth, Width.max, Width.min);
   }
   S.EndMultiColumn();

   return nullptr;
}

bool EffectClickRemoval::TransferDataToWindow(const EffectSettings &)
{
   if (!mUIParent->TransferDataToWindow())
   {
      return false;
   }

   return true;
}

bool EffectClickRemoval::TransferDataFromWindow(EffectSettings &)
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
