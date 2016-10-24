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

\class CompressorPanel
\brief Panel used within the EffectCompressor for EffectCompressor.

*//*******************************************************************/

#include "../Audacity.h"
#include "Compressor.h"

#include <math.h>

#include <wx/brush.h>
#include <wx/dcclient.h>
#include <wx/dcmemory.h>
#include <wx/intl.h>
#include <wx/msgdlg.h>

#include "../AColor.h"
#include "../Prefs.h"
#include "../ShuttleGui.h"
#include "../float_cast.h"
#include "../widgets/Ruler.h"

#include "../WaveTrack.h"

enum
{
   ID_Threshold = 10000,
   ID_NoiseFloor,
   ID_Ratio,
   ID_Attack,
   ID_Decay
};

// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name          Type     Key                  Def      Min      Max      Scale
Param( Threshold,    double,  XO("Threshold"),     -12.0,   -60.0,   -1.0,    1   );
Param( NoiseFloor,   double,  XO("NoiseFloor"),    -40.0,   -80.0,   -20.0,   5   );
Param( Ratio,        double,  XO("Ratio"),         2.0,     1.1,     10.0,    10  );
Param( AttackTime,   double,  XO("AttackTime"),    0.2,     0.1,     5.0,     100 );
Param( ReleaseTime,  double,  XO("ReleaseTime"),   1.0,     1.0,     30.0,    10  );
Param( Normalize,    bool,    XO("Normalize"),     true,    false,   true,    1   );
Param( UsePeak,      bool,    XO("UsePeak"),       false,   false,   true,    1   );

//----------------------------------------------------------------------------
// EffectCompressor
//----------------------------------------------------------------------------

BEGIN_EVENT_TABLE(EffectCompressor, wxEvtHandler)
   EVT_SLIDER(wxID_ANY, EffectCompressor::OnSlider)
END_EVENT_TABLE()

EffectCompressor::EffectCompressor()
{
   mThresholdDB = DEF_Threshold;
   mNoiseFloorDB = DEF_NoiseFloor;
   mAttackTime = DEF_AttackTime;          // seconds
   mDecayTime = DEF_ReleaseTime;          // seconds
   mRatio = DEF_Ratio;                    // positive number > 1.0
   mNormalize = DEF_Normalize;
   mUsePeak = DEF_UsePeak;

   mThreshold = 0.25;
   mNoiseFloor = 0.01;
   mCompression = 0.5;
   mCircle = NULL;
   mFollow1 = NULL;
   mFollow2 = NULL;
   mFollowLen = 0;

   SetLinearEffectFlag(false);
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

// IdentInterface implementation

wxString EffectCompressor::GetSymbol()
{
   return COMPRESSOR_PLUGIN_SYMBOL;
}

wxString EffectCompressor::GetDescription()
{
   return XO("Compresses the dynamic range of audio");
}

// EffectIdentInterface implementation

EffectType EffectCompressor::GetType()
{
   return EffectTypeProcess;
}

// EffectClientInterface implementation

bool EffectCompressor::GetAutomationParameters(EffectAutomationParameters & parms)
{
   parms.Write(KEY_Threshold, mThresholdDB);
   parms.Write(KEY_NoiseFloor, mNoiseFloorDB);
   parms.Write(KEY_Ratio, mRatio);
   parms.Write(KEY_AttackTime, mAttackTime);
   parms.Write(KEY_ReleaseTime, mDecayTime);
   parms.Write(KEY_Normalize, mNormalize);
   parms.Write(KEY_UsePeak, mUsePeak);

   return true;
}

bool EffectCompressor::SetAutomationParameters(EffectAutomationParameters & parms)
{
   ReadAndVerifyDouble(Threshold);
   ReadAndVerifyDouble(NoiseFloor);
   ReadAndVerifyDouble(Ratio);
   ReadAndVerifyDouble(AttackTime);
   ReadAndVerifyDouble(ReleaseTime);
   ReadAndVerifyBool(Normalize);
   ReadAndVerifyBool(UsePeak);

   mThresholdDB = Threshold;
   mNoiseFloorDB = NoiseFloor;
   mRatio = Ratio;
   mAttackTime = AttackTime;
   mDecayTime = ReleaseTime;
   mNormalize = Normalize;
   mUsePeak = UsePeak;

   return true;
}

// Effect Implemenration

bool EffectCompressor::Startup()
{
   wxString base = wxT("/Effects/Compressor/");

   // Migrate settings from 2.1.0 or before

   // Already migrated, so bail
   if (gPrefs->Exists(base + wxT("Migrated")))
   {
      return true;
   }

   // Load the old "current" settings
   if (gPrefs->Exists(base))
   {
      gPrefs->Read(base + wxT("ThresholdDB"), &mThresholdDB, -12.0f );
      gPrefs->Read(base + wxT("NoiseFloorDB"), &mNoiseFloorDB, -40.0f );
      gPrefs->Read(base + wxT("Ratio"), &mRatio, 2.0f );
      gPrefs->Read(base + wxT("AttackTime"), &mAttackTime, 0.2f );
      gPrefs->Read(base + wxT("DecayTime"), &mDecayTime, 1.0f );
      gPrefs->Read(base + wxT("Normalize"), &mNormalize, true );
      gPrefs->Read(base + wxT("UsePeak"), &mUsePeak, false );

      SaveUserPreset(GetCurrentSettingsGroup());

      // Do not migrate again
      gPrefs->Write(base + wxT("Migrated"), true);
      gPrefs->Flush();
   }

   return true;
}

void EffectCompressor::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(5);

   S.StartHorizontalLay(wxEXPAND, true);
   {
      S.SetBorder(10);
      mPanel = safenew EffectCompressorPanel(S.GetParent(),
                                         mThresholdDB,
                                         mNoiseFloorDB,
                                         mRatio);
      mPanel->SetMinSize(wxSize(400, 200));
      S.Prop(true).AddWindow(mPanel, wxEXPAND | wxALL);
      S.SetBorder(5);
   }
   S.EndHorizontalLay();

   S.StartStatic(wxT(""));
   {
      S.StartMultiColumn(3, wxEXPAND);
      {
         S.SetStretchyCol(1);
         mThresholdLabel = S.AddVariableText(_("Threshold:"), true,
                                             wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         S.SetStyle(wxSL_HORIZONTAL);
         mThresholdSlider = S.Id(ID_Threshold).AddSlider(wxT(""),
                                                         DEF_Threshold * SCL_Threshold,
                                                         MAX_Threshold * SCL_Threshold,
                                                         MIN_Threshold * SCL_Threshold);
         mThresholdSlider->SetName(_("Threshold"));
         mThresholdText = S.AddVariableText(wxT("XXX dB"), true,
                                            wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

         mNoiseFloorLabel = S.AddVariableText(_("Noise Floor:"), true,
                                             wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         S.SetStyle(wxSL_HORIZONTAL);
         mNoiseFloorSlider = S.Id(ID_NoiseFloor).AddSlider(wxT(""),
                                                           DEF_NoiseFloor / SCL_NoiseFloor,
                                                           MAX_NoiseFloor / SCL_NoiseFloor,
                                                           MIN_NoiseFloor / SCL_NoiseFloor);
         mNoiseFloorSlider->SetName(_("Noise Floor"));
         mNoiseFloorText = S.AddVariableText(wxT("XXX dB"), true,
                                            wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

         mRatioLabel = S.AddVariableText(_("Ratio:"), true,
                                         wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         S.SetStyle(wxSL_HORIZONTAL);
         mRatioSlider = S.Id(ID_Ratio).AddSlider(wxT(""),
                                                 DEF_Ratio * SCL_Ratio,
                                                 MAX_Ratio * SCL_Ratio,
                                                 MIN_Ratio * SCL_Ratio);
         mRatioSlider->SetName(_("Ratio"));
         mRatioSlider->SetPageSize(5);
         mRatioText = S.AddVariableText(wxT("XXXX:1"), true,
                                             wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

         /* i18n-hint: Particularly in percussion, sounds can be regarded as having
          * an 'attack' phase where the sound builds up and a 'decay' where the
          * sound dies away.  So this means 'onset duration'.  */
         mAttackLabel = S.AddVariableText(_("Attack Time:"), true,
                                         wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         S.SetStyle(wxSL_HORIZONTAL);
         mAttackSlider = S.Id(ID_Attack).AddSlider(wxT(""),
                                                   DEF_AttackTime * SCL_AttackTime,
                                                   MAX_AttackTime * SCL_AttackTime,
                                                   MIN_AttackTime * SCL_AttackTime);
         mAttackSlider->SetName(_("Attack Time"));
         mAttackText = S.AddVariableText(wxT("XXXX secs"), true,
                                         wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

         mDecayLabel = S.AddVariableText(_("Release Time:"), true,
                                         wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         S.SetStyle(wxSL_HORIZONTAL);
         mDecaySlider = S.Id(ID_Decay).AddSlider(wxT(""),
                                                 DEF_ReleaseTime * SCL_ReleaseTime,
                                                 MAX_ReleaseTime * SCL_ReleaseTime,
                                                 MIN_ReleaseTime * SCL_ReleaseTime);
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
      mGainCheckBox = S.AddCheckBox(_("Make-up gain for 0 dB after compressing"),
                                    DEF_Normalize ? wxT("true") : wxT("false"));
      mPeakCheckBox = S.AddCheckBox(_("Compress based on Peaks"),
                                    DEF_UsePeak ? wxT("true") : wxT("false"));
   }
   S.EndHorizontalLay();
}

bool EffectCompressor::TransferDataToWindow()
{
   mThresholdSlider->SetValue(lrint(mThresholdDB));
   mNoiseFloorSlider->SetValue(lrint(mNoiseFloorDB / SCL_NoiseFloor));
   mRatioSlider->SetValue(lrint(mRatio * SCL_Ratio));
   mAttackSlider->SetValue(lrint(mAttackTime * SCL_AttackTime));
   mDecaySlider->SetValue(lrint(mDecayTime * SCL_ReleaseTime));
   mGainCheckBox->SetValue(mNormalize);
   mPeakCheckBox->SetValue(mUsePeak);

   UpdateUI();

   return true;
}

bool EffectCompressor::TransferDataFromWindow()
{
   if (!mUIParent->Validate())
   {
      return false;
   }

   mThresholdDB = (double) mThresholdSlider->GetValue();
   mNoiseFloorDB = (double) mNoiseFloorSlider->GetValue() * SCL_NoiseFloor;
   mRatio = (double) mRatioSlider->GetValue() / SCL_Ratio;
   mAttackTime = (double) mAttackSlider->GetValue() / 100.0; //SCL_AttackTime;
   mDecayTime = (double) mDecaySlider->GetValue() / SCL_ReleaseTime;
   mNormalize = mGainCheckBox->GetValue();
   mUsePeak = mPeakCheckBox->GetValue();

   return true;
}

// EffectTwoPassSimpleMono implementation

bool EffectCompressor::NewTrackPass1()
{
   mThreshold = DB_TO_LINEAR(mThresholdDB);
   mNoiseFloor = DB_TO_LINEAR(mNoiseFloorDB);
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
   size_t maxlen = 0;
   SelectedTrackListOfKindIterator iter(Track::Wave, mTracks);
   WaveTrack *track = (WaveTrack *) iter.First();
   while (track) {
      maxlen = std::max(maxlen, track->GetMaxBlockSize());
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
bool EffectCompressor::TwoBufferProcessPass1
   (float *buffer1, size_t len1, float *buffer2, size_t len2)
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

bool EffectCompressor::ProcessPass2(float *buffer, size_t len)
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

void EffectCompressor::Follow(float *buffer, float *env, size_t len, float *previous, size_t previous_len)
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
    input / rise_factor^3, etc. until this NEW envelope intersects
    the previously computed values. There is only a limited buffer
    in which we can work backwards, so if the NEW envelope does not
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
   for(i = len; i--;) {
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
      for(i = previous_len; i--;) {
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

void EffectCompressor::OnSlider(wxCommandEvent & WXUNUSED(evt))
{
   TransferDataFromWindow();
   UpdateUI();
}

void EffectCompressor::UpdateUI()
{
   mThresholdLabel->SetName(wxString::Format(_("Threshold %d dB"), (int) mThresholdDB));
   /* i18n-hint: usually leave this as is as dB doesn't get translated*/
   mThresholdText->SetLabel(wxString::Format(_("%3d dB"), (int) mThresholdDB));
   mThresholdText->SetName(mThresholdText->GetLabel()); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)

   mNoiseFloorLabel->SetName(wxString::Format(_("Noise Floor %d dB"), (int) mNoiseFloorDB));
   mNoiseFloorText->SetLabel(wxString::Format(_("%3d dB"), (int) mNoiseFloorDB));
   mNoiseFloorText->SetName(mNoiseFloorText->GetLabel()); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)

   if (mRatioSlider->GetValue() % 10 == 0) {
      mRatioLabel->SetName(wxString::Format(_("Ratio %.0f to 1"), mRatio));
      /* i18n-hint: Unless your language has a different convention for ratios,
       * like 8:1, leave as is.*/
      mRatioText->SetLabel(wxString::Format(_("%.0f:1"), mRatio));
   }
   else {
      mRatioLabel->SetName(wxString::Format(_("Ratio %.1f to 1"), mRatio));
      /* i18n-hint: Unless your language has a different convention for ratios,
       * like 8:1, leave as is.*/
      mRatioText->SetLabel(wxString::Format(_("%.1f:1"), mRatio));
   }
   mRatioText->SetName(mRatioText->GetLabel()); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)

   mAttackLabel->SetName(wxString::Format(_("Attack Time %.2f secs"), mAttackTime));
   mAttackText->SetLabel(wxString::Format(_("%.2f secs"), mAttackTime));
   mAttackText->SetName(mAttackText->GetLabel()); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)

   mDecayLabel->SetName(wxString::Format(_("Release Time %.1f secs"), mDecayTime));
   mDecayText->SetLabel(wxString::Format(_("%.1f secs"), mDecayTime));
   mDecayText->SetName(mDecayText->GetLabel()); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)

   mPanel->Refresh(false);

   return;
}

//----------------------------------------------------------------------------
// EffectCompressorPanel
//----------------------------------------------------------------------------

BEGIN_EVENT_TABLE(EffectCompressorPanel, wxPanelWrapper)
   EVT_PAINT(EffectCompressorPanel::OnPaint)
   EVT_SIZE(EffectCompressorPanel::OnSize)
END_EVENT_TABLE()

EffectCompressorPanel::EffectCompressorPanel(wxWindow *parent,
                                             double & threshold,
                                             double & noiseFloor,
                                             double & ratio)
:  wxPanelWrapper(parent),
   threshold(threshold),
   noiseFloor(noiseFloor),
   ratio(ratio)
{
}

void EffectCompressorPanel::OnPaint(wxPaintEvent & WXUNUSED(evt))
{
   wxPaintDC dc(this);

   int width, height;
   GetSize(&width, &height);

   double rangeDB = 60;

   // Ruler
   int w = 0;
   int h = 0;

   Ruler vRuler;
   vRuler.SetBounds(0, 0, width, height);
   vRuler.SetOrientation(wxVERTICAL);
   vRuler.SetRange(0, -rangeDB);
   vRuler.SetFormat(Ruler::LinearDBFormat);
   vRuler.SetUnits(_("dB"));
   vRuler.GetMaxSize(&w, NULL);

   Ruler hRuler;
   hRuler.SetBounds(0, 0, width, height);
   hRuler.SetOrientation(wxHORIZONTAL);
   hRuler.SetRange(-rangeDB, 0);
   hRuler.SetFormat(Ruler::LinearDBFormat);
   hRuler.SetUnits(_("dB"));
   hRuler.SetFlip(true);
   hRuler.GetMaxSize(NULL, &h);

   vRuler.SetBounds(0, 0, w, height - h);
   hRuler.SetBounds(w, height - h, width, height);

#if defined(__WXMSW__)
   dc.Clear();
#endif

   wxRect border;
   border.x = w;
   border.y = 0;
   border.width = width - w;
   border.height = height - h + 1;

   dc.SetBrush(*wxWHITE_BRUSH);
   dc.SetPen(*wxBLACK_PEN);
   dc.DrawRectangle(border);

   wxRect envRect = border;
   envRect.Deflate( 2, 2 );

   int kneeX = lrint((rangeDB+threshold)*envRect.width/rangeDB);
   int kneeY = lrint((rangeDB+threshold/ratio)*envRect.height/rangeDB);

   int finalY = envRect.height;
   int startY = lrint((threshold*(1.0/ratio-1.0))*envRect.height/rangeDB);

   // Yellow line for threshold
/*   dc.SetPen(wxPen(wxColour(220, 220, 0), 1, wxSOLID));
   AColor::Line(dc,
                envRect.x,
                envRect.y + envRect.height - kneeY,
                envRect.x + envRect.width - 1,
                envRect.y + envRect.height - kneeY);*/

   // Was: Nice dark red line for the compression diagram
//   dc.SetPen(wxPen(wxColour(180, 40, 40), 3, wxSOLID));

   // Nice blue line for compressor, same color as used in the waveform envelope.
   dc.SetPen( AColor::WideEnvelopePen) ;

   AColor::Line(dc,
                envRect.x,
                envRect.y + envRect.height - startY,
                envRect.x + kneeX - 1,
                envRect.y + envRect.height - kneeY);

   AColor::Line(dc,
                envRect.x + kneeX,
                envRect.y + envRect.height - kneeY,
                envRect.x + envRect.width - 1,
                envRect.y + envRect.height - finalY);

   // Paint border again
   dc.SetBrush(*wxTRANSPARENT_BRUSH);
   dc.SetPen(*wxBLACK_PEN);
   dc.DrawRectangle(border);

   vRuler.Draw(dc);
   hRuler.Draw(dc);
}

void EffectCompressorPanel::OnSize(wxSizeEvent & WXUNUSED(evt))
{
   Refresh(false);
}
