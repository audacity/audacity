/**********************************************************************

  Audacity: A Digital Audio Editor

  Compressor.cpp

  Dominic Mazzoni
  Martyn Shaw
  Steve Jolly

*******************************************************************//**

\class EffectCompressor
\brief An Effect derived from EffectTwoPassSimpleMono

 - Martyn Shaw made it inherit from EffectTwoPassSimpleMono 10/2005.
 - Steve Jolly made it inherit from EffectSimpleMono.
 - GUI added and implementation improved by Dominic Mazzoni, 5/11/2003.

*//****************************************************************//**

\class CompressorDialog
\brief Dialog used with EffectCompressor.

*//****************************************************************//**

\class CompressorPanel
\brief Panel used within the CompressorDialog for EffectCompressor.

*//*******************************************************************/


#include "../Audacity.h" // for rint from configwin.h

#include <math.h>

#include <wx/msgdlg.h>
#include <wx/textdlg.h>
#include <wx/brush.h>
#include <wx/image.h>
#include <wx/dcmemory.h>

#include "Compressor.h"
#include "../ShuttleGui.h"
#include "../WaveTrack.h"
#include "../widgets/Ruler.h"
#include "../AColor.h"
#include "../Shuttle.h"
#include "../Prefs.h"

EffectCompressor::EffectCompressor()
{
   mNormalize = true;
   mUsePeak = false;
   mThreshold = 0.25;
   mAttackTime = 0.2;      // seconds
   mDecayTime = 1.0;       // seconds
   mRatio = 2.0;           // positive number > 1.0
   mCompression = 0.5;
   mThresholdDB = -12.0;
   mNoiseFloorDB = -40.0;
   mNoiseFloor = 0.01;
   mCircle = NULL;
   mFollow1 = NULL;
   mFollow2 = NULL;
   mFollowLen = 0;
}

bool EffectCompressor::Init()
{
   // Restore the saved preferences
   gPrefs->Read(wxT("/Effects/Compressor/ThresholdDB"), &mThresholdDB, -12.0f );
   gPrefs->Read(wxT("/Effects/Compressor/NoiseFloorDB"), &mNoiseFloorDB, -40.0f );
   gPrefs->Read(wxT("/Effects/Compressor/Ratio"), &mRatio, 2.0f );
   gPrefs->Read(wxT("/Effects/Compressor/AttackTime"), &mAttackTime, 0.2f );
   gPrefs->Read(wxT("/Effects/Compressor/DecayTime"), &mDecayTime, 1.0f );
   gPrefs->Read(wxT("/Effects/Compressor/Normalize"), &mNormalize, true );
   gPrefs->Read(wxT("/Effects/Compressor/UsePeak"), &mUsePeak, false );

   return true;
}

EffectCompressor::~EffectCompressor()
{
   if (mCircle) {
      delete[] mCircle;
      mCircle = NULL;
   }
   if(mFollow1!=NULL) {
      delete[] mFollow1;
      mFollow1 = NULL;
   }
   if(mFollow2!=NULL) {
      delete[] mFollow2;
      mFollow2 = NULL;
   }
}

bool EffectCompressor::TransferParameters( Shuttle & shuttle )
{
   shuttle.TransferDouble( wxT("Threshold"), mThresholdDB, -12.0f );
   shuttle.TransferDouble( wxT("NoiseFloor"), mNoiseFloorDB, -40.0f );
   shuttle.TransferDouble( wxT("Ratio"), mRatio, 2.0f );
   shuttle.TransferDouble( wxT("AttackTime"), mAttackTime, 0.2f );
   shuttle.TransferDouble( wxT("ReleaseTime"), mDecayTime, 1.0f );
   shuttle.TransferBool( wxT("Normalize"), mNormalize, true );
   shuttle.TransferBool( wxT("UsePeak"), mUsePeak, false );
   return true;
}

bool EffectCompressor::PromptUser()
{
   CompressorDialog dlog(this, mParent);
   dlog.threshold = mThresholdDB;
   dlog.noisefloor = mNoiseFloorDB;
   dlog.ratio = mRatio;
   dlog.attack = mAttackTime;
   dlog.decay = mDecayTime;
   dlog.useGain = mNormalize;
   dlog.usePeak = mUsePeak;
   dlog.TransferDataToWindow();

   dlog.CentreOnParent();
   dlog.ShowModal();

   if (dlog.GetReturnCode() == wxID_CANCEL)
      return false;

   mThresholdDB = dlog.threshold;
   mNoiseFloorDB = dlog.noisefloor;
   mRatio = dlog.ratio;
   mAttackTime = dlog.attack;
   mDecayTime = dlog.decay;
   mNormalize = dlog.useGain;
   mUsePeak = dlog.usePeak;

   // Retain the settings
   gPrefs->Write(wxT("/Effects/Compressor/ThresholdDB"), mThresholdDB);
   gPrefs->Write(wxT("/Effects/Compressor/NoiseFloorDB"), mNoiseFloorDB);
   gPrefs->Write(wxT("/Effects/Compressor/Ratio"), mRatio);
   gPrefs->Write(wxT("/Effects/Compressor/AttackTime"), mAttackTime);
   gPrefs->Write(wxT("/Effects/Compressor/DecayTime"), mDecayTime);
   gPrefs->Write(wxT("/Effects/Compressor/Normalize"), mNormalize);
   gPrefs->Write(wxT("/Effects/Compressor/UsePeak"), mUsePeak);

   return gPrefs->Flush();
}

bool EffectCompressor::NewTrackPass1()
{
   mThreshold = pow(10.0, mThresholdDB/20); // factor of 20 because it's power
   mNoiseFloor = pow(10.0, mNoiseFloorDB/20);
   mNoiseCounter = 100;

   mAttackInverseFactor = exp(log(mThreshold) / (mCurRate * mAttackTime + 0.5));
   mAttackFactor = 1.0 / mAttackInverseFactor;
   mDecayFactor = exp(log(mThreshold) / (mCurRate * mDecayTime + 0.5));

   if(mRatio > 1)
      mCompression = 1.0-1.0/mRatio;
   else
      mCompression = 0.0;

   mLastLevel = mThreshold;

   if (mCircle)
      delete[] mCircle;
   mCircleSize = 100;
   mCircle = new double[mCircleSize];
   for(int j=0; j<mCircleSize; j++) {
      mCircle[j] = 0.0;
   }
   mCirclePos = 0;
   mRMSSum = 0.0;

   return true;
}
bool EffectCompressor::InitPass1()
{
   mMax=0.0;
   if (!mNormalize)
      DisableSecondPass();

   // Find the maximum block length required for any track
   sampleCount maxlen=0;
   SelectedTrackListOfKindIterator iter(Track::Wave, mTracks);
   WaveTrack *track = (WaveTrack *) iter.First();
   while (track) {
      sampleCount len=track->GetMaxBlockSize();
      if(len > maxlen)
         maxlen = len;
      //Iterate to the next track
      track = (WaveTrack *) iter.Next();
   }
   if(mFollow1!=NULL) {
      delete[] mFollow1;
      mFollow1 = NULL;
   }
   if(mFollow2!=NULL) {
      delete[] mFollow2;
      mFollow2 = NULL;
   }
   // Allocate buffers for the envelope
   if(maxlen > 0) {
      mFollow1 = new float[maxlen];
      mFollow2 = new float[maxlen];
   }
   mFollowLen = maxlen;

   return true;
}
bool EffectCompressor::InitPass2()
{
    // Actually, this should not even be called, because we call
    // DisableSecondPass() before, if mNormalize is false.
    return mNormalize;
}

// Process the input with 2 buffers available at a time
// buffer1 will be written upon return
// buffer2 will be passed as buffer1 on the next call
bool EffectCompressor::TwoBufferProcessPass1(float *buffer1, sampleCount len1, float *buffer2, sampleCount len2)
{
   int i;

   // If buffers are bigger than allocated, then abort
   // (this should never happen, but if it does, we don't want to crash)
   if((len1 > mFollowLen) || (len2 > mFollowLen))
      return false;

   // This makes sure that the initial value is well-chosen
   // buffer1 == NULL on the first and only the first call
   if (buffer1 == NULL) {
      // Initialize the mLastLevel to the peak level in the first buffer
      // This avoids problems with large spike events near the beginning of the track
      mLastLevel = mThreshold;
      for(i=0; i<len2; i++) {
         if(mLastLevel < fabs(buffer2[i]))
            mLastLevel = fabs(buffer2[i]);
      }
   }

   // buffer2 is NULL on the last and only the last call
   if(buffer2 != NULL) {
      Follow(buffer2, mFollow2, len2, mFollow1, len1);
   }

   if(buffer1 != NULL) {
      for (i = 0; i < len1; i++) {
         buffer1[i] = DoCompression(buffer1[i], mFollow1[i]);
      }
   }


#if 0
   // Copy the envelope over the track data (for debug purposes)
   memcpy(buffer1, mFollow1, len1*sizeof(float));
#endif

   // Rotate the buffer pointers
   float *tmpfloat = mFollow1;
   mFollow1 = mFollow2;
   mFollow2 = tmpfloat;

   return true;
}

bool EffectCompressor::ProcessPass2(float *buffer, sampleCount len)
{
   if (mMax != 0)
   {
      for (int i = 0; i < len; i++)
         buffer[i] /= mMax;
   }

   return true;
}

void EffectCompressor::FreshenCircle()
{
   // Recompute the RMS sum periodically to prevent accumulation of rounding errors
   // during long waveforms
   mRMSSum = 0;
   for(int i=0; i<mCircleSize; i++)
      mRMSSum += mCircle[i];
}

float EffectCompressor::AvgCircle(float value)
{
   float level;

   // Calculate current level from root-mean-squared of
   // circular buffer ("RMS")
   mRMSSum -= mCircle[mCirclePos];
   mCircle[mCirclePos] = value*value;
   mRMSSum += mCircle[mCirclePos];
   level = sqrt(mRMSSum/mCircleSize);
   mCirclePos = (mCirclePos+1)%mCircleSize;

   return level;
}

void EffectCompressor::Follow(float *buffer, float *env, int len, float *previous, int previous_len)
{
   /*

   "Follow"ing algorithm by Roger B. Dannenberg, taken from
   Nyquist.  His description follows.  -DMM

   Description: this is a sophisticated envelope follower.
    The input is an envelope, e.g. something produced with
    the AVG function. The purpose of this function is to
    generate a smooth envelope that is generally not less
    than the input signal. In other words, we want to "ride"
    the peaks of the signal with a smooth function. The
    algorithm is as follows: keep a current output value
    (called the "value"). The value is allowed to increase
    by at most rise_factor and decrease by at most fall_factor.
    Therefore, the next value should be between
    value * rise_factor and value * fall_factor. If the input
    is in this range, then the next value is simply the input.
    If the input is less than value * fall_factor, then the
    next value is just value * fall_factor, which will be greater
    than the input signal. If the input is greater than value *
    rise_factor, then we compute a rising envelope that meets
    the input value by working bacwards in time, changing the
    previous values to input / rise_factor, input / rise_factor^2,
    input / rise_factor^3, etc. until this new envelope intersects
    the previously computed values. There is only a limited buffer
    in which we can work backwards, so if the new envelope does not
    intersect the old one, then make yet another pass, this time
    from the oldest buffered value forward, increasing on each
    sample by rise_factor to produce a maximal envelope. This will
    still be less than the input.

    The value has a lower limit of floor to make sure value has a
    reasonable positive value from which to begin an attack.
   */
   int i;
   double level,last;

   if(!mUsePeak) {
      // Update RMS sum directly from the circle buffer
      // to avoid accumulation of rounding errors
      FreshenCircle();
   }
   // First apply a peak detect with the requested decay rate
   last = mLastLevel;
   for(i=0; i<len; i++) {
      if(mUsePeak)
         level = fabs(buffer[i]);
      else // use RMS
         level = AvgCircle(buffer[i]);
      // Don't increase gain when signal is continuously below the noise floor
      if(level < mNoiseFloor) {
         mNoiseCounter++;
      } else {
         mNoiseCounter = 0;
      }
      if(mNoiseCounter < 100) {
         last *= mDecayFactor;
         if(last < mThreshold)
            last = mThreshold;
         if(level > last)
            last = level;
      }
      env[i] = last;
   }
   mLastLevel = last;

   // Next do the same process in reverse direction to get the requested attack rate
   last = mLastLevel;
   for(i=len-1; i>=0; i--) {
      last *= mAttackInverseFactor;
      if(last < mThreshold)
         last = mThreshold;
      if(env[i] < last)
         env[i] = last;
      else
         last = env[i];
   }

   if((previous != NULL) && (previous_len > 0)) {
      // If the previous envelope was passed, propagate the rise back until we intersect
      for(i=previous_len-1; i>0; i--) {
         last *= mAttackInverseFactor;
         if(last < mThreshold)
            last = mThreshold;
         if(previous[i] < last)
            previous[i] = last;
         else // Intersected the previous envelope buffer, so we are finished
            return;
      }
      // If we can't back up far enough, project the starting level forward
      // until we intersect the desired envelope
      last = previous[0];
      for(i=1; i<previous_len; i++) {
         last *= mAttackFactor;
         if(previous[i] > last)
            previous[i] = last;
         else // Intersected the desired envelope, so we are finished
            return;
      }
      // If we still didn't intersect, then continue ramp up into current buffer
      for(i=0; i<len; i++) {
         last *= mAttackFactor;
         if(buffer[i] > last)
            buffer[i] = last;
         else // Finally got an intersect
            return;
      }
      // If we still didn't intersect, then reset mLastLevel
      mLastLevel = last;
   }
}

float EffectCompressor::DoCompression(float value, double env)
{
   float out;
   if(mUsePeak) {
      // Peak values map 1.0 to 1.0 - 'upward' compression
      out = value * pow(1.0/env, mCompression);
   } else {
      // With RMS-based compression don't change values below mThreshold - 'downward' compression
      out = value * pow(mThreshold/env, mCompression);
   }

   // Retain the maximum value for use in the normalization pass
   if(mMax < fabs(out))
      mMax = fabs(out);

   return out;
}

//----------------------------------------------------------------------------
// CompressorPanel
//----------------------------------------------------------------------------

BEGIN_EVENT_TABLE(CompressorPanel, wxPanel)
    EVT_PAINT(CompressorPanel::OnPaint)
END_EVENT_TABLE()

CompressorPanel::CompressorPanel( wxWindow *parent, wxWindowID id,
                          const wxPoint& pos,
                          const wxSize& size):
   wxPanel(parent, id, pos, size)
{
   mBitmap = NULL;
   mWidth = 0;
   mHeight = 0;
}

void CompressorPanel::OnPaint(wxPaintEvent & WXUNUSED(event))
{
   wxPaintDC dc(this);

   int width, height;
   GetSize(&width, &height);

   if (!mBitmap || mWidth!=width || mHeight!=height) {
      if (mBitmap)
         delete mBitmap;

      mWidth = width;
      mHeight = height;
      mBitmap = new wxBitmap(mWidth, mHeight);
   }

   double rangeDB = 60;

   // Ruler
   int w = 0;
   int h = 0;

   Ruler vRuler;
   vRuler.SetBounds(0, 0, mWidth, mHeight);
   vRuler.SetOrientation(wxVERTICAL);
   vRuler.SetRange(0, -rangeDB);
   vRuler.SetFormat(Ruler::LinearDBFormat);
   vRuler.SetUnits(_("dB"));
   vRuler.GetMaxSize(&w, NULL);
   
   Ruler hRuler;
   hRuler.SetBounds(0, 0, mWidth, mHeight);
   hRuler.SetOrientation(wxHORIZONTAL);
   hRuler.SetRange(-rangeDB, 0);
   hRuler.SetFormat(Ruler::LinearDBFormat);
   hRuler.SetUnits(_("dB"));
   hRuler.SetFlip(true);
   hRuler.GetMaxSize(NULL, &h);

   vRuler.SetBounds(0, 0, w, mHeight - h);
   hRuler.SetBounds(w, mHeight - h, mWidth, mHeight);

   wxColour bkgnd = GetBackgroundColour();
   wxBrush bkgndBrush(bkgnd, wxSOLID);

   wxMemoryDC memDC;
   memDC.SelectObject(*mBitmap);

   wxRect bkgndRect;
   bkgndRect.x = 0;
   bkgndRect.y = 0;
   bkgndRect.width = w;
   bkgndRect.height = mHeight;
   memDC.SetBrush(bkgndBrush);
   memDC.SetPen(*wxTRANSPARENT_PEN);
   memDC.DrawRectangle(bkgndRect);

   bkgndRect.y = mHeight - h;
   bkgndRect.width = mWidth;
   bkgndRect.height = h;
   memDC.DrawRectangle(bkgndRect);

   wxRect border;
   border.x = w;
   border.y = 0;
   border.width = mWidth - w;
   border.height = mHeight - h + 1;

   memDC.SetBrush(*wxWHITE_BRUSH);
   memDC.SetPen(*wxBLACK_PEN);
   memDC.DrawRectangle(border);

   mEnvRect = border;
   mEnvRect.Deflate( 2, 2 );

   int kneeX = (int)rint((rangeDB+threshold)*mEnvRect.width/rangeDB);
   int kneeY = (int)rint((rangeDB+threshold/ratio)*mEnvRect.height/rangeDB);

   int finalY = mEnvRect.height;
   int startY = (int)rint((threshold*(1.0/ratio-1.0))*mEnvRect.height/rangeDB);

   // Yellow line for threshold
/*   memDC.SetPen(wxPen(wxColour(220, 220, 0), 1, wxSOLID));
   AColor::Line(memDC,
                mEnvRect.x,
                mEnvRect.y + mEnvRect.height - kneeY,
                mEnvRect.x + mEnvRect.width - 1,
                mEnvRect.y + mEnvRect.height - kneeY);*/

   // Was: Nice dark red line for the compression diagram
//   memDC.SetPen(wxPen(wxColour(180, 40, 40), 3, wxSOLID));

   // Nice blue line for compressor, same color as used in the waveform envelope.
   memDC.SetPen( AColor::WideEnvelopePen) ;

   AColor::Line(memDC,
                mEnvRect.x,
                mEnvRect.y + mEnvRect.height - startY,
                mEnvRect.x + kneeX - 1,
                mEnvRect.y + mEnvRect.height - kneeY);

   AColor::Line(memDC,
                mEnvRect.x + kneeX,
                mEnvRect.y + mEnvRect.height - kneeY,
                mEnvRect.x + mEnvRect.width - 1,
                mEnvRect.y + mEnvRect.height - finalY);

   // Paint border again
   memDC.SetBrush(*wxTRANSPARENT_BRUSH);
   memDC.SetPen(*wxBLACK_PEN);
   memDC.DrawRectangle(border);

   vRuler.Draw(memDC);
   hRuler.Draw(memDC);

   dc.Blit(0, 0, mWidth, mHeight,
           &memDC, 0, 0, wxCOPY, FALSE);
}

//----------------------------------------------------------------------------
// CompressorDialog
//----------------------------------------------------------------------------

enum {
   ThresholdID = 7100,
   NoiseFloorID,
   RatioID,
   AttackID,
   DecayID
};

BEGIN_EVENT_TABLE(CompressorDialog, EffectDialog)
   EVT_SIZE( CompressorDialog::OnSize )
   EVT_BUTTON( ID_EFFECT_PREVIEW, CompressorDialog::OnPreview )
   EVT_SLIDER( ThresholdID, CompressorDialog::OnSlider )
   EVT_SLIDER( NoiseFloorID, CompressorDialog::OnSlider )
   EVT_SLIDER( RatioID, CompressorDialog::OnSlider )
   EVT_SLIDER( AttackID, CompressorDialog::OnSlider )
   EVT_SLIDER( DecayID, CompressorDialog::OnSlider )
END_EVENT_TABLE()

CompressorDialog::CompressorDialog(EffectCompressor *effect, wxWindow *parent)
:  EffectDialog(parent, _("Dynamic Range Compressor"), PROCESS_EFFECT,
                wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER ),
   mEffect(effect)
{
   Init();

   SetSizeHints(500, 400);
   SetSize(500, 500);
}

void CompressorDialog::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(5);

   S.StartHorizontalLay(wxEXPAND, true);
   {
      S.SetBorder(10);
      mPanel = new CompressorPanel(S.GetParent(), wxID_ANY);
      mPanel->threshold = threshold;
      mPanel->noisefloor = noisefloor;
      mPanel->ratio = ratio;
      S.Prop(true).AddWindow(mPanel, wxEXPAND | wxALL);
      S.SetBorder(5);
   }
   S.EndHorizontalLay();

   S.StartStatic(wxT(""));
   {
      S.StartMultiColumn(3, wxEXPAND | wxALIGN_CENTER_VERTICAL);
      {
         S.SetStretchyCol(1);
         mThresholdLabel = S.AddVariableText(_("Threshold:"), true,
                                             wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         S.SetStyle(wxSL_HORIZONTAL);
         mThresholdSlider = S.Id(ThresholdID).AddSlider(wxT(""), -12, -1, -60);
         mThresholdSlider->SetName(_("Threshold"));
         mThresholdText = S.AddVariableText(wxT("XXX dB"), true,
                                            wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

         mNoiseFloorLabel = S.AddVariableText(_("Noise Floor:"), true,
                                             wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         S.SetStyle(wxSL_HORIZONTAL);
         mNoiseFloorSlider = S.Id(NoiseFloorID).AddSlider(wxT(""), -8, -4, -16);
         mNoiseFloorSlider->SetName(_("Noise Floor"));
         mNoiseFloorText = S.AddVariableText(wxT("XXX dB"), true,
                                            wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

         mRatioLabel = S.AddVariableText(_("Ratio:"), true,
                                         wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         S.SetStyle(wxSL_HORIZONTAL);
         mRatioSlider = S.Id(RatioID).AddSlider(wxT(""), 4, 20, 3);
         mRatioSlider->SetName(_("Ratio"));
         mRatioText = S.AddVariableText(wxT("XXXX:1"), true,
                                             wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

         /* i18n-hint: Particularly in percussion, sounds can be regarded as having 
          * an 'attack' phase where the sound builds up and a 'decay' where the 
          * sound dies away.  So this means 'onset duration'.  */
         mAttackLabel = S.AddVariableText(_("Attack Time:"), true,
                                         wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         S.SetStyle(wxSL_HORIZONTAL);
         mAttackSlider = S.Id(AttackID).AddSlider(wxT(""), 2, 50, 1);
         mAttackSlider->SetName(_("Attack Time"));
         mAttackText = S.AddVariableText(wxT("XXXX secs"), true,
                                         wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

         mDecayLabel = S.AddVariableText(_("Release Time:"), true,
                                         wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         S.SetStyle(wxSL_HORIZONTAL);
         mDecaySlider = S.Id(DecayID).AddSlider(wxT(""), 2, 30, 1);
         mDecaySlider->SetName(_("Release Time"));
         mDecayText = S.AddVariableText(wxT("XXXX secs"), true,
                                        wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   S.StartHorizontalLay(wxCENTER, false);
   {
      /* i18n-hint: Make-up, i.e. correct for any reduction, rather than fabricate it.*/
      mGainCheckBox = S.AddCheckBox(_("Make-up gain for 0dB after compressing"),
                                    wxT("true"));
      mPeakCheckBox = S.AddCheckBox(_("Compress based on Peaks"),
                                    wxT("false"));
   }
   S.EndHorizontalLay();
}

bool CompressorDialog::TransferDataToWindow()
{
   mPanel->threshold = threshold;
   mPanel->noisefloor = noisefloor;
   mPanel->ratio = ratio;

   mThresholdSlider->SetValue((int)rint(threshold));
   mNoiseFloorSlider->SetValue((int)rint(noisefloor/5));
   mRatioSlider->SetValue((int)rint(ratio*2));
   mAttackSlider->SetValue((int)rint(attack*10));
   mDecaySlider->SetValue((int)rint(decay));
   mGainCheckBox->SetValue(useGain);
   mPeakCheckBox->SetValue(usePeak);

   TransferDataFromWindow();

   return true;
}

bool CompressorDialog::TransferDataFromWindow()
{
   threshold = (double)mThresholdSlider->GetValue();
   noisefloor = (double)mNoiseFloorSlider->GetValue() * 5.0;
   ratio = (double)(mRatioSlider->GetValue() / 2.0);
   attack = (double)(mAttackSlider->GetValue() / 10.0);
   decay = (double)(mDecaySlider->GetValue());
   useGain = mGainCheckBox->GetValue();
   usePeak = mPeakCheckBox->GetValue();

   mPanel->threshold = threshold;
   mPanel->noisefloor = noisefloor;
   mPanel->ratio = ratio;

   mThresholdLabel->SetName(wxString::Format(_("Threshold %d dB"), (int)threshold));
   /* i18n-hint: usually leave this as is as dB doesn't get translated*/
   mThresholdText->SetLabel(wxString::Format(_("%3d dB"), (int)threshold));
   mThresholdText->SetName(mThresholdText->GetLabel()); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)

   mNoiseFloorLabel->SetName(wxString::Format(_("Noise Floor %d dB"), (int)noisefloor));
   mNoiseFloorText->SetLabel(wxString::Format(_("%3d dB"), (int)noisefloor));
   mNoiseFloorText->SetName(mNoiseFloorText->GetLabel()); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)

   if (mRatioSlider->GetValue()%2 == 0) {
      mRatioLabel->SetName(wxString::Format(_("Ratio %.0f to 1"), ratio));
      /* i18n-hint: Unless your language has a different convention for ratios, 
       * like 8:1, leave as is.*/
      mRatioText->SetLabel(wxString::Format(_("%.0f:1"), ratio));
   }
   else {
      mRatioLabel->SetName(wxString::Format(_("Ratio %.1f to 1"), ratio));
      /* i18n-hint: Unless your language has a different convention for ratios, 
       * like 8:1, leave as is.*/
      mRatioText->SetLabel(wxString::Format(_("%.1f:1"), ratio));
   }
   mRatioText->SetName(mRatioText->GetLabel()); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)

   mAttackLabel->SetName(wxString::Format(_("Attack Time %.1f secs"), attack));
   mAttackText->SetLabel(wxString::Format(_("%.1f secs"), attack));
   mAttackText->SetName(mAttackText->GetLabel()); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)

   mDecayLabel->SetName(wxString::Format(_("Release Time %.1f secs"), decay));
   mDecayText->SetLabel(wxString::Format(_("%.1f secs"), decay));
   mDecayText->SetName(mDecayText->GetLabel()); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)

   mPanel->Refresh(false);

   return true;
}

void CompressorDialog::OnSize(wxSizeEvent &event)
{
   mPanel->Refresh( false );
   event.Skip();
}

void CompressorDialog::OnPreview(wxCommandEvent & WXUNUSED(event))
{
   TransferDataFromWindow();

   // Save & restore parameters around Preview, because we didn't do OK.
   double    oldAttackTime = mEffect->mAttackTime;
   double    oldDecayTime = mEffect->mDecayTime;
   double    oldThresholdDB = mEffect->mThresholdDB;
   double    oldNoiseFloorDB = mEffect->mNoiseFloorDB;
   double    oldRatio = mEffect->mRatio;
   bool      oldUseGain = mEffect->mNormalize;
   bool      oldUsePeak = mEffect->mUsePeak;

   mEffect->mAttackTime = attack;
   mEffect->mDecayTime = decay;
   mEffect->mThresholdDB = threshold;
   mEffect->mNoiseFloorDB = noisefloor;
   mEffect->mRatio = ratio;
   mEffect->mNormalize = useGain;
   mEffect->mUsePeak = usePeak;

   mEffect->Preview();

   mEffect->mAttackTime = oldAttackTime;
   mEffect->mDecayTime = oldDecayTime;
   mEffect->mThresholdDB = oldThresholdDB;
   mEffect->mNoiseFloorDB = oldNoiseFloorDB;
   mEffect->mRatio = oldRatio;
   mEffect->mNormalize = oldUseGain;
   mEffect->mUsePeak = oldUsePeak;
}

void CompressorDialog::OnSlider(wxCommandEvent & WXUNUSED(event))
{
   TransferDataFromWindow();
}
