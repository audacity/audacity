/**********************************************************************

  Audacity: A Digital Audio Editor

  AutoDuck.cpp

  Markus Meyer

*******************************************************************//**

\class EffectAutoDuck
\brief Implements the Auto Ducking effect

*******************************************************************/

#include "../Audacity.h"
#include "AutoDuck.h"

#include <math.h>
#include <float.h>

#include <wx/dcclient.h>
#include <wx/dcmemory.h>
#include <wx/dynarray.h>
#include <wx/intl.h>
#include <wx/msgdlg.h>

#include "../AColor.h"
#include "../AllThemeResources.h"
#include "../Internat.h"
#include "../Prefs.h"
#include "../ShuttleGui.h"
#include "../Theme.h"
#include "../widgets/valnum.h"

#include "../WaveTrack.h"

// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name                Type     Key                     Def      Min      Max      Scale
Param( DuckAmountDb,       double,  XO("DuckAmountDb"),     -12.0,   -24.0,   0.0,     1  );
Param( InnerFadeDownLen,   double,  XO("InnerFadeDownLen"), 0.0,     0.0,     3.0,     1  );
Param( InnerFadeUpLen,     double,  XO("InnerFadeUpLen"),   0.0,     0.0,     3.0,     1  );
Param( OuterFadeDownLen,   double,  XO("OuterFadeDownLen"), 0.5,     0.0,     3.0,     1  );
Param( OuterFadeUpLen,     double,  XO("OuterFadeUpLen"),   0.5,     0.0,     3.0,     1  );
Param( ThresholdDb,        double,  XO("ThresholdDb"),      -30.0,   -100.0,  0.0,     1  );
Param( MaximumPause,       double,  XO("MaximumPause"),     1.0,     0.0,     DBL_MAX, 1  );

/*
 * Common constants
 */

enum : size_t { kBufSize = 131072 };     // number of samples to process at once
enum : size_t { kRMSWindowSize = 100 };  // samples in circular RMS window buffer

/*
 * A auto duck region and an array of auto duck regions
 */

struct AutoDuckRegion
{
   AutoDuckRegion(double t0, double t1)
   {
      this->t0 = t0;
      this->t1 = t1;
   }

   double t0;
   double t1;
};

#include <wx/arrimpl.cpp>

WX_DECLARE_OBJARRAY(AutoDuckRegion, AutoDuckRegionArray);
WX_DEFINE_OBJARRAY(AutoDuckRegionArray);

/*
 * Effect implementation
 */

BEGIN_EVENT_TABLE(EffectAutoDuck, wxEvtHandler)
   EVT_TEXT(wxID_ANY, EffectAutoDuck::OnValueChanged)
END_EVENT_TABLE()

EffectAutoDuck::EffectAutoDuck()
{
   mDuckAmountDb = DEF_DuckAmountDb;
   mInnerFadeDownLen = DEF_InnerFadeDownLen;
   mInnerFadeUpLen = DEF_InnerFadeUpLen;
   mOuterFadeDownLen = DEF_OuterFadeDownLen;
   mOuterFadeUpLen = DEF_OuterFadeUpLen;
   mThresholdDb = DEF_ThresholdDb;
   mMaximumPause = DEF_MaximumPause;

   SetLinearEffectFlag(true);

   mControlTrack = NULL;

   mPanel = NULL;
}

EffectAutoDuck::~EffectAutoDuck()
{
}

// IdentInterface implementation

wxString EffectAutoDuck::GetSymbol()
{
   return AUTODUCK_PLUGIN_SYMBOL;
}

wxString EffectAutoDuck::GetDescription()
{
   return XO("Reduces (ducks) the volume of one or more tracks whenever the volume of a specified \"control\" track reaches a particular level");
}

// EffectIdentInterface implementation

EffectType EffectAutoDuck::GetType()
{
   return EffectTypeProcess;
}

// EffectClientInterface implementation

bool EffectAutoDuck::GetAutomationParameters(EffectAutomationParameters & parms)
{
   parms.Write(KEY_DuckAmountDb, mDuckAmountDb);
   parms.Write(KEY_InnerFadeDownLen, mInnerFadeDownLen);
   parms.Write(KEY_InnerFadeUpLen, mInnerFadeUpLen);
   parms.Write(KEY_OuterFadeDownLen, mOuterFadeDownLen);
   parms.Write(KEY_OuterFadeUpLen, mOuterFadeUpLen);
   parms.Write(KEY_ThresholdDb, mThresholdDb);
   parms.Write(KEY_MaximumPause, mMaximumPause);

   return true;
}

bool EffectAutoDuck::SetAutomationParameters(EffectAutomationParameters & parms)
{
   ReadAndVerifyDouble(DuckAmountDb);
   ReadAndVerifyDouble(InnerFadeDownLen);
   ReadAndVerifyDouble(InnerFadeUpLen);
   ReadAndVerifyDouble(OuterFadeDownLen);
   ReadAndVerifyDouble(OuterFadeUpLen);
   ReadAndVerifyDouble(ThresholdDb);
   ReadAndVerifyDouble(MaximumPause);

   mDuckAmountDb = DuckAmountDb;
   mInnerFadeDownLen = InnerFadeDownLen;
   mInnerFadeUpLen = InnerFadeUpLen;
   mOuterFadeDownLen = OuterFadeDownLen;
   mOuterFadeUpLen = OuterFadeUpLen;
   mThresholdDb = ThresholdDb;
   mMaximumPause = MaximumPause;

   return true;
}

// Effect implementation

bool EffectAutoDuck::Startup()
{
   wxString base = wxT("/Effects/AutoDuck/");

   // Migrate settings from 2.1.0 or before

   // Already migrated, so bail
   if (gPrefs->Exists(base + wxT("Migrated")))
   {
      return true;
   }

   // Load the old "current" settings
   if (gPrefs->Exists(base))
   {
      gPrefs->Read(base + wxT("DuckAmountDb"), &mDuckAmountDb, DEF_DuckAmountDb);
      gPrefs->Read(base + wxT("InnerFadeDownLen"), &mInnerFadeDownLen, DEF_InnerFadeDownLen);
      gPrefs->Read(base + wxT("InnerFadeUpLen"), &mInnerFadeUpLen, DEF_InnerFadeUpLen);
      gPrefs->Read(base + wxT("OuterFadeDownLen"), &mOuterFadeDownLen, DEF_OuterFadeDownLen);
      gPrefs->Read(base + wxT("OuterFadeUpLen"), &mOuterFadeUpLen, DEF_OuterFadeUpLen);
      gPrefs->Read(base + wxT("ThresholdDb"), &mThresholdDb, DEF_ThresholdDb);
      gPrefs->Read(base + wxT("MaximumPause"), &mMaximumPause, DEF_MaximumPause);

      SaveUserPreset(GetCurrentSettingsGroup());
      
      // Do not migrate again
      gPrefs->Write(base + wxT("Migrated"), true);
      gPrefs->Flush();
   }

   return true;
}

bool EffectAutoDuck::Init()
{
   mControlTrack = NULL;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   bool lastWasSelectedWaveTrack = false;
   const WaveTrack *controlTrackCandidate = NULL;

   while(t)
   {
      if (lastWasSelectedWaveTrack && !t->GetSelected() &&
          t->GetKind() == Track::Wave)
      {
         // This could be the control track, so remember it
         controlTrackCandidate = (WaveTrack*)t;
      }

      lastWasSelectedWaveTrack = false;

      if (t->GetSelected())
      {
         if (t->GetKind() == Track::Wave)
         {
            lastWasSelectedWaveTrack = true;
         }
         else
         {
            wxMessageBox(
               _("You selected a track which does not contain audio. AutoDuck can only process audio tracks."),
               /* i18n-hint: Auto duck is the name of an effect that 'ducks' (reduces the volume)
                * of the audio automatically when there is sound on another track.  Not as
                * in 'Donald-Duck'!*/
               _("Auto Duck"), wxICON_ERROR, mUIParent);
            return false;
         }
      }

      t = iter.Next();
   }

   if (!controlTrackCandidate)
   {
      wxMessageBox(
         _("Auto Duck needs a control track which must be placed below the selected track(s)."),
         _("Auto Duck"), wxICON_ERROR, mUIParent);
      return false;
   }

   mControlTrack = controlTrackCandidate;

   return true;
}

void EffectAutoDuck::End()
{
   mControlTrack = NULL;
}

bool EffectAutoDuck::Process()
{
   if (GetNumWaveTracks() == 0 || !mControlTrack)
      return false;

   bool cancel = false;

   auto start =
      mControlTrack->TimeToLongSamples(mT0 + mOuterFadeDownLen);
   auto end =
      mControlTrack->TimeToLongSamples(mT1 - mOuterFadeUpLen);

   if (end <= start)
      return false;

   // the minimum number of samples we have to wait until the maximum
   // pause has been exceeded
   double maxPause = mMaximumPause;

   // We don't fade in until we have time enough to actually fade out again
   if (maxPause < mOuterFadeDownLen + mOuterFadeUpLen)
      maxPause = mOuterFadeDownLen + mOuterFadeUpLen;

   auto minSamplesPause =
      mControlTrack->TimeToLongSamples(maxPause);

   double threshold = DB_TO_LINEAR(mThresholdDb);

   // adjust the threshold so we can compare it to the rmsSum value
   threshold = threshold * threshold * kRMSWindowSize;

   int rmsPos = 0;
   float rmsSum = 0;
   float *rmsWindow = new float[kRMSWindowSize];
   for (size_t i = 0; i < kRMSWindowSize; i++)
      rmsWindow[i] = 0;

   float *buf = new float[kBufSize];

   bool inDuckRegion = false;

   // initialize the following two variables to prevent compiler warning
   double duckRegionStart = 0;
   sampleCount curSamplesPause = 0;

   // to make the progress bar appear more natural, we first look for all
   // duck regions and apply them all at once afterwards
   AutoDuckRegionArray regions;
   auto pos = start;

   while (pos < end)
   {
      const auto len = limitSampleBufferSize( kBufSize, end - pos );

      mControlTrack->Get((samplePtr)buf, floatSample, pos, len);

      for (auto i = pos; i < pos + len; i++)
      {
         rmsSum -= rmsWindow[rmsPos];
         // i - pos is bounded by len:
         auto index = ( i - pos ).as_size_t();
         rmsWindow[rmsPos] = buf[ index ] * buf[ index ];
         rmsSum += rmsWindow[rmsPos];
         rmsPos = (rmsPos + 1) % kRMSWindowSize;

         bool thresholdExceeded = rmsSum > threshold;

         if (thresholdExceeded)
         {
            // everytime the threshold is exceeded, reset our count for
            // the number of pause samples
            curSamplesPause = 0;

            if (!inDuckRegion)
            {
               // the threshold has been exceeded for the first time, so
               // let the duck region begin here
               inDuckRegion = true;
               duckRegionStart = mControlTrack->LongSamplesToTime(i);
            }
         }

         if (!thresholdExceeded && inDuckRegion)
         {
            // the threshold has not been exceeded and we are in a duck
            // region, but only fade in if the maximum pause has been
            // exceeded
            curSamplesPause += 1;

            if (curSamplesPause >= minSamplesPause)
            {
               // do the actual duck fade and reset all values
               double duckRegionEnd =
                  mControlTrack->LongSamplesToTime(i - curSamplesPause);

               regions.Add(AutoDuckRegion(
                              duckRegionStart - mOuterFadeDownLen,
                              duckRegionEnd + mOuterFadeUpLen));

               inDuckRegion = false;
            }
         }
      }

      pos += len;

      if (TotalProgress(
            (pos - start).as_double() /
            (end - start).as_double() /
            (GetNumWaveTracks() + 1)
      ))
      {
         cancel = true;
         break;
      }
   }

   // apply last duck fade, if any
   if (inDuckRegion)
   {
      double duckRegionEnd =
         mControlTrack->LongSamplesToTime(end - curSamplesPause);
      regions.Add(AutoDuckRegion(
                     duckRegionStart - mOuterFadeDownLen,
                     duckRegionEnd + mOuterFadeUpLen));
   }

   delete[] buf;
   delete[] rmsWindow;

   if (!cancel)
   {
      CopyInputTracks(); // Set up mOutputTracks.
      SelectedTrackListOfKindIterator iter(Track::Wave, mOutputTracks.get());
      Track *iterTrack = iter.First();

      int trackNumber = 0;

      while (iterTrack)
      {
         WaveTrack* t = (WaveTrack*)iterTrack;

         for (size_t i = 0; i < regions.GetCount(); i++)
         {
            const AutoDuckRegion& region = regions[i];
            if (ApplyDuckFade(trackNumber, t, region.t0, region.t1))
            {
               cancel = true;
               break;
            }
         }

         if (cancel)
            break;

         iterTrack = iter.Next();
         trackNumber++;
      }
   }

   ReplaceProcessedTracks(!cancel);
   return !cancel;
}

void EffectAutoDuck::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(5);
   S.StartVerticalLay(true);
   {
      S.AddSpace(0, 5);

      mPanel = safenew EffectAutoDuckPanel(S.GetParent(), this);
      S.AddWindow(mPanel);

      S.AddSpace(0, 5);

      S.StartMultiColumn(6, wxCENTER);
      {
         FloatingPointValidator<double> vldDuckAmountDb(1, &mDuckAmountDb, NUM_VAL_NO_TRAILING_ZEROES);
         vldDuckAmountDb.SetRange(MIN_DuckAmountDb, MAX_DuckAmountDb);
         mDuckAmountDbBox = S.AddTextBox(_("Duck amount:"), wxT(""), 10);
         mDuckAmountDbBox->SetValidator(vldDuckAmountDb);
         S.AddUnits(_("dB"));

         FloatingPointValidator<double> vldMaximumPause(2, &mMaximumPause, NUM_VAL_NO_TRAILING_ZEROES);
         vldMaximumPause.SetRange(MIN_MaximumPause, MAX_MaximumPause);
         mMaximumPauseBox = S.AddTextBox(_("Maximum pause:"), wxT(""), 10);
         mMaximumPauseBox->SetValidator(vldMaximumPause);
         S.AddUnits(_("seconds"));

         FloatingPointValidator<double> vldOuterFadeDownLen(2, &mOuterFadeDownLen, NUM_VAL_NO_TRAILING_ZEROES);
         vldOuterFadeDownLen.SetRange(MIN_OuterFadeDownLen, MAX_OuterFadeDownLen);
         mOuterFadeDownLenBox = S.AddTextBox(_("Outer fade down length:"), wxT(""), 10);
         mOuterFadeDownLenBox->SetValidator(vldOuterFadeDownLen);
         S.AddUnits(_("seconds"));

         FloatingPointValidator<double> vldOuterFadeUpLen(2, &mOuterFadeUpLen, NUM_VAL_NO_TRAILING_ZEROES);
         vldOuterFadeUpLen.SetRange(MIN_OuterFadeUpLen, MAX_OuterFadeUpLen);
         mOuterFadeUpLenBox = S.AddTextBox(_("Outer fade up length:"), wxT(""), 10);
         mOuterFadeUpLenBox->SetValidator(vldOuterFadeUpLen);
         S.AddUnits(_("seconds"));

         FloatingPointValidator<double> vldInnerFadeDownLen(2, &mInnerFadeDownLen, NUM_VAL_NO_TRAILING_ZEROES);
         vldInnerFadeDownLen.SetRange(MIN_InnerFadeDownLen, MAX_InnerFadeDownLen);
         mInnerFadeDownLenBox = S.AddTextBox(_("Inner fade down length:"), wxT(""), 10);
         mInnerFadeDownLenBox->SetValidator(vldInnerFadeDownLen);
         S.AddUnits(_("seconds"));

         FloatingPointValidator<double> vldInnerFadeUpLen(2, &mInnerFadeUpLen, NUM_VAL_NO_TRAILING_ZEROES);
         vldInnerFadeUpLen.SetRange(MIN_InnerFadeUpLen, MAX_InnerFadeUpLen);
         mInnerFadeUpLenBox = S.AddTextBox(_("Inner fade up length:"), wxT(""), 10);
         mInnerFadeUpLenBox->SetValidator(vldInnerFadeUpLen);
         S.AddUnits(_("seconds"));
      }
      S.EndMultiColumn();

      S.StartMultiColumn(3, wxCENTER);
      {
         FloatingPointValidator<double> vldThresholdDb(2, &mThresholdDb, NUM_VAL_NO_TRAILING_ZEROES);
         vldThresholdDb.SetRange(MIN_ThresholdDb, MAX_ThresholdDb);
         mThresholdDbBox = S.AddTextBox(_("Threshold:"), wxT(""), 10);
         mThresholdDbBox->SetValidator(vldThresholdDb);
         S.AddUnits(_("dB"));
      }
      S.EndMultiColumn();

   }
   S.EndVerticalLay();

   return;
}

bool EffectAutoDuck::TransferDataToWindow()
{
   if (!mUIParent->TransferDataToWindow())
   {
      return false;
   }

   mPanel->Refresh(false);

   return true;
}

bool EffectAutoDuck::TransferDataFromWindow()
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }

   return true;
}

// EffectAutoDuck implementation

// this currently does an exponential fade
bool EffectAutoDuck::ApplyDuckFade(int trackNumber, WaveTrack* t,
                                   double t0, double t1)
{
   bool cancel = false;

   auto start = t->TimeToLongSamples(t0);
   auto end = t->TimeToLongSamples(t1);

   float *buf = new float[kBufSize];
   auto pos = start;

   auto fadeDownSamples = t->TimeToLongSamples(
      mOuterFadeDownLen + mInnerFadeDownLen);
   if (fadeDownSamples < 1)
      fadeDownSamples = 1;

   auto fadeUpSamples = t->TimeToLongSamples(
      mOuterFadeUpLen + mInnerFadeUpLen);
   if (fadeUpSamples < 1)
      fadeUpSamples = 1;

   float fadeDownStep = mDuckAmountDb / fadeDownSamples.as_double();
   float fadeUpStep = mDuckAmountDb / fadeUpSamples.as_double();

   while (pos < end)
   {
      const auto len = limitSampleBufferSize( kBufSize, end - pos );

      t->Get((samplePtr)buf, floatSample, pos, len);

      for (auto i = pos; i < pos + len; i++)
      {
         float gainDown = fadeDownStep * (i - start).as_float();
         float gainUp = fadeUpStep * (end - i).as_float();

         float gain;
         if (gainDown > gainUp)
            gain = gainDown;
         else
            gain = gainUp;
         if (gain < mDuckAmountDb)
            gain = mDuckAmountDb;

         // i - pos is bounded by len:
         buf[ ( i - pos ).as_size_t() ] *= DB_TO_LINEAR(gain);
      }

      t->Set((samplePtr)buf, floatSample, pos, len);

      pos += len;

      float curTime = t->LongSamplesToTime(pos);
      float fractionFinished = (curTime - mT0) / (mT1 - mT0);
      if (TotalProgress( (trackNumber + 1 + fractionFinished) /
                         (GetNumWaveTracks() + 1) ))
      {
         cancel = true;
         break;
      }
   }

   delete[] buf;
   return cancel;
}

void EffectAutoDuck::OnValueChanged(wxCommandEvent & WXUNUSED(evt))
{
   mPanel->Refresh(false);
}

/*
 * EffectAutoDuckPanel implementation
 */

#define CONTROL_POINT_REGION 10 // pixel distance to click on a control point
#define CONTROL_POINT_MIN_MOVE 5 // min mouse move until value is changed

#define TEXT_DISTANCE 15 // pixel distance text <-> center of control point

#define FADE_DOWN_START 150 // x coordinate
#define FADE_UP_START 450 // x coordinate
#define DUCK_AMOUNT_START 50 // y coordinate

#define FADE_SCALE 40 // scale factor for second -> pixel conversion
#define DUCK_AMOUNT_SCALE 8 // scale factor for db -> pixel conversion

static int GetDistance(const wxPoint& first, const wxPoint& second)
{
   int distanceX = abs(first.x - second.x);
   int distanceY = abs(first.y - second.y);
   if (distanceX > distanceY)
      return distanceX;
   else
      return distanceY;
}

BEGIN_EVENT_TABLE(EffectAutoDuckPanel, wxPanelWrapper)
   EVT_PAINT(EffectAutoDuckPanel::OnPaint)
   EVT_MOUSE_CAPTURE_CHANGED(EffectAutoDuckPanel::OnMouseCaptureChanged)
   EVT_MOUSE_CAPTURE_LOST(EffectAutoDuckPanel::OnMouseCaptureLost)
   EVT_LEFT_DOWN(EffectAutoDuckPanel::OnLeftDown)
   EVT_LEFT_UP(EffectAutoDuckPanel::OnLeftUp)
   EVT_MOTION(EffectAutoDuckPanel::OnMotion)
END_EVENT_TABLE()

EffectAutoDuckPanel::EffectAutoDuckPanel(wxWindow *parent, EffectAutoDuck *effect)
:  wxPanelWrapper(parent, wxID_ANY, wxDefaultPosition, wxSize(600, 300))
{
   mParent = parent;
   mEffect = effect;
   mCurrentControlPoint = none;
   mBackgroundBitmap = NULL;

   ResetControlPoints();
}

EffectAutoDuckPanel::~EffectAutoDuckPanel()
{
   if(HasCapture())
      ReleaseMouse();
}

void EffectAutoDuckPanel::ResetControlPoints()
{
   mControlPoints[innerFadeDown] = wxPoint(-100,-100);
   mControlPoints[innerFadeUp] = wxPoint(-100,-100);
   mControlPoints[outerFadeDown] = wxPoint(-100,-100);
   mControlPoints[outerFadeUp] = wxPoint(-100,-100);
   mControlPoints[duckAmount] = wxPoint(-100,-100);
}

void EffectAutoDuckPanel::OnPaint(wxPaintEvent & WXUNUSED(evt))
{
   int clientWidth, clientHeight;
   GetSize(&clientWidth, &clientHeight);

   if (!mBackgroundBitmap || mBackgroundBitmap->GetWidth() != clientWidth ||
       mBackgroundBitmap->GetHeight() != clientHeight)
   {
      mBackgroundBitmap = std::make_unique<wxBitmap>(clientWidth, clientHeight);
   }

   wxMemoryDC dc;
   dc.SelectObject(*mBackgroundBitmap);

   dc.SetBrush(*wxWHITE_BRUSH);
   dc.SetPen(*wxBLACK_PEN);
   dc.DrawRectangle(0, 0, clientWidth, clientHeight);

   dc.SetFont(wxFont(10, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL,
                     wxFONTWEIGHT_NORMAL));
   dc.SetTextForeground(*wxBLACK);
   dc.SetTextBackground(*wxWHITE);

   double duckAmountDb = 0;
   double innerFadeDownLen = 0;
   double innerFadeUpLen = 0;
   double outerFadeDownLen = 0;
   double outerFadeUpLen = 0;
   mEffect->mDuckAmountDbBox->GetValue().ToDouble(&duckAmountDb);
   mEffect->mInnerFadeDownLenBox->GetValue().ToDouble(&innerFadeDownLen);
   mEffect->mInnerFadeUpLenBox->GetValue().ToDouble(&innerFadeUpLen);
   mEffect->mOuterFadeDownLenBox->GetValue().ToDouble(&outerFadeDownLen);
   mEffect->mOuterFadeUpLenBox->GetValue().ToDouble(&outerFadeUpLen);

   if (innerFadeDownLen < MIN_InnerFadeDownLen || innerFadeDownLen > MAX_InnerFadeDownLen ||
       innerFadeUpLen < MIN_InnerFadeUpLen     || innerFadeUpLen > MAX_InnerFadeUpLen     ||
       outerFadeDownLen < MIN_OuterFadeDownLen || outerFadeDownLen > MAX_OuterFadeDownLen ||
       outerFadeUpLen < MIN_OuterFadeUpLen     || outerFadeUpLen > MAX_OuterFadeUpLen     ||
       duckAmountDb < MIN_DuckAmountDb         || duckAmountDb > MAX_DuckAmountDb)
   {
      // values are out of range, no preview available
      wxString message = wxString::Format(_("Preview not available"));
      int textWidth = 0, textHeight = 0;
      dc.GetTextExtent(message, &textWidth, &textHeight);
      dc.DrawText(message, (clientWidth - textWidth) / 2,
                           (clientHeight - textHeight) / 2);

      ResetControlPoints();
   } else
   {
      // draw preview
      dc.SetBrush(*wxTRANSPARENT_BRUSH);
      dc.SetPen(wxPen(theTheme.Colour(clrGraphLines), 3, wxSOLID));

      wxPoint points[6];

      points[0].x = 10;
      points[0].y = DUCK_AMOUNT_START;

      points[1].x = FADE_DOWN_START - (int)(outerFadeDownLen * FADE_SCALE);
      points[1].y = DUCK_AMOUNT_START;

      points[2].x = FADE_DOWN_START + (int)(innerFadeDownLen * FADE_SCALE);
      points[2].y = DUCK_AMOUNT_START -
         (int)(duckAmountDb * DUCK_AMOUNT_SCALE);

      points[3].x = FADE_UP_START - (int)(innerFadeUpLen * FADE_SCALE);
      points[3].y = DUCK_AMOUNT_START -
         (int)(duckAmountDb * DUCK_AMOUNT_SCALE);

      points[4].x = FADE_UP_START + (int)(outerFadeUpLen * FADE_SCALE);
      points[4].y = DUCK_AMOUNT_START;

      points[5].x = clientWidth - 10;
      points[5].y = DUCK_AMOUNT_START;

      dc.DrawLines(6, points);

      dc.SetPen(wxPen(*wxBLACK, 1, wxDOT));

      AColor::Line(dc, FADE_DOWN_START, 10, FADE_DOWN_START, clientHeight - 10);
      AColor::Line(dc, FADE_UP_START, 10, FADE_UP_START, clientHeight - 10);

      dc.SetPen(AColor::envelopePen);
      dc.SetBrush(*wxWHITE_BRUSH);

      mControlPoints[outerFadeDown] = points[1];
      mControlPoints[innerFadeDown] = points[2];
      mControlPoints[innerFadeUp] = points[3];
      mControlPoints[outerFadeUp] = points[4];
      mControlPoints[duckAmount] = wxPoint(
         (points[2].x + points[3].x) / 2, points[2].y);

      for (int i = 0; i < AUTO_DUCK_PANEL_NUM_CONTROL_POINTS; i++)
      {
         EControlPoint cp = (EControlPoint)i;
         int digits;
         float value;

         if (cp == innerFadeDown)
         {
            value = innerFadeDownLen;
            digits = 2;
         }
         else if (cp == innerFadeUp)
         {
            value = innerFadeUpLen;
            digits = 2;
         }
         else if (cp == outerFadeDown)
         {
            value = outerFadeDownLen;
            digits = 2;
         } else if (cp == outerFadeUp)
         {
            value = outerFadeUpLen;
            digits = 2;
         }
         else
         {
            value = duckAmountDb;
            digits = 1;
         }

         wxString valueStr = Internat::ToDisplayString(value, digits);
         valueStr += wxT(" ");

         if (cp == duckAmount)
            /* i18n-hint: short form of 'decibels'.*/
            valueStr += _("dB");
         else
            /* i18n-hint: short form of 'seconds'.*/
            valueStr += _("s");

         int textWidth = 0, textHeight = 0;
         GetTextExtent(valueStr, &textWidth, &textHeight);

         int textPosX = mControlPoints[i].x - textWidth / 2;
         int textPosY = mControlPoints[i].y;

         if (cp == duckAmount || cp == outerFadeDown || cp == outerFadeUp)
            textPosY -= TEXT_DISTANCE + textHeight;
         else
            textPosY += TEXT_DISTANCE;

         dc.DrawText(valueStr, textPosX, textPosY);

         dc.DrawEllipse(mControlPoints[i].x - 3,
                        mControlPoints[i].y - 3, 6, 6);
      }
   }

   // copy background buffer to paint dc
   wxPaintDC paintDC(this);
   paintDC.Blit(0, 0, clientWidth, clientHeight, &dc, 0, 0);

   // clean up: necessary to free resources on Windows
   dc.SetPen(wxNullPen);
   dc.SetBrush(wxNullBrush);
   dc.SetFont(wxNullFont);
   dc.SelectObject(wxNullBitmap);
}

void EffectAutoDuckPanel::OnMouseCaptureChanged(
   wxMouseCaptureChangedEvent & WXUNUSED(evt))
{
   SetCursor(wxNullCursor);
   mCurrentControlPoint = none;
}

void EffectAutoDuckPanel::OnMouseCaptureLost(
   wxMouseCaptureLostEvent & WXUNUSED(evt))
{
   mCurrentControlPoint = none;

   if (HasCapture())
   {
      ReleaseMouse();
   }
}

EffectAutoDuckPanel::EControlPoint
   EffectAutoDuckPanel::GetNearestControlPoint(const wxPoint & pt)
{
   int dist[AUTO_DUCK_PANEL_NUM_CONTROL_POINTS];
   int i;

   for (i = 0; i < AUTO_DUCK_PANEL_NUM_CONTROL_POINTS; i++)
      dist[i] = GetDistance(pt, mControlPoints[i]);

   int curMinimum = 0;
   for (i = 0; i < AUTO_DUCK_PANEL_NUM_CONTROL_POINTS; i++)
      if (dist[i] < dist[curMinimum])
         curMinimum = i;

   if (dist[curMinimum] <= CONTROL_POINT_REGION)
      return (EControlPoint)curMinimum;
   else
      return none;
}

void EffectAutoDuckPanel::OnLeftDown(wxMouseEvent & evt)
{
   EControlPoint nearest = GetNearestControlPoint(evt.GetPosition());

   if (nearest != none)
   {
      // this control point has been clicked
      mMouseDownPoint = evt.GetPosition();

      mCurrentControlPoint = nearest;
      mControlPointMoveActivated = false;

      for (int i = 0; i < AUTO_DUCK_PANEL_NUM_CONTROL_POINTS; i++)
         mMoveStartControlPoints[i] = mControlPoints[i];

      if( !HasCapture() )
         CaptureMouse();
   }
}

void EffectAutoDuckPanel::OnLeftUp(wxMouseEvent & WXUNUSED(evt))
{
   if (mCurrentControlPoint != none)
   {
      mCurrentControlPoint = none;
      ReleaseMouse();
   }
}

void EffectAutoDuckPanel::OnMotion(wxMouseEvent & evt)
{
   switch (GetNearestControlPoint(evt.GetPosition()))
   {
   case none:
      SetCursor(wxNullCursor);
      break;
   case innerFadeDown:
   case innerFadeUp:
   case outerFadeDown:
   case outerFadeUp:
      SetCursor(wxCursor(wxCURSOR_SIZEWE));
      break;
   case duckAmount:
      SetCursor(wxCursor(wxCURSOR_SIZENS));
      break;
   }

   if (mCurrentControlPoint != none)
   {
      if (!mControlPointMoveActivated)
      {
         int dist;

         if (mCurrentControlPoint == duckAmount)
            dist = abs(evt.GetY() - mMouseDownPoint.y);
         else
            dist = abs(evt.GetX() - mMouseDownPoint.x);

         if (dist >= CONTROL_POINT_MIN_MOVE)
            mControlPointMoveActivated = true;
      }

      if (mControlPointMoveActivated)
      {
         float newValue;

         switch (mCurrentControlPoint)
         {
         case outerFadeDown:
            newValue = ((double)(FADE_DOWN_START - evt.GetX())) / FADE_SCALE;
            mEffect->mOuterFadeDownLen = TrapDouble(newValue, MIN_OuterFadeDownLen, MAX_OuterFadeDownLen);
            break;
         case outerFadeUp:
            newValue = ((double)(evt.GetX() - FADE_UP_START)) / FADE_SCALE;
            mEffect->mOuterFadeUpLen = TrapDouble(newValue, MIN_OuterFadeUpLen, MAX_OuterFadeUpLen);
            break;
         case innerFadeDown:
            newValue = ((double)(evt.GetX() - FADE_DOWN_START)) / FADE_SCALE;
            mEffect->mInnerFadeDownLen = TrapDouble(newValue, MIN_InnerFadeDownLen, MAX_InnerFadeDownLen);
            break;
         case innerFadeUp:
            newValue = ((double)(FADE_UP_START - evt.GetX())) / FADE_SCALE;
            mEffect->mInnerFadeUpLen = TrapDouble(newValue, MIN_InnerFadeUpLen, MAX_InnerFadeUpLen);
            break;
         case duckAmount:
            newValue = ((double)(DUCK_AMOUNT_START - evt.GetY())) / DUCK_AMOUNT_SCALE;
            mEffect->mDuckAmountDb = TrapDouble(newValue, MIN_DuckAmountDb, MAX_DuckAmountDb);
            break;
         case none:
            wxASSERT(false); // should not happen
         }
         mEffect->TransferDataToWindow();
         Refresh(false);
      }
   }
}
