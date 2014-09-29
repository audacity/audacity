/**********************************************************************

  Audacity: A Digital Audio Editor

  AutoDuck.cpp

  Markus Meyer

*******************************************************************//**

\class EffectAutoDuck
\brief Implements the Auto Ducking effect

*******************************************************************/

#include <math.h>
#include <wx/sizer.h>
#include <wx/dynarray.h>
#include <wx/dcclient.h>
#include <wx/dcmemory.h>
#include <wx/valtext.h>

#include "../Audacity.h"
#include "../Prefs.h"
#include "../Internat.h"
#include "../Theme.h"
#include "../AllThemeResources.h"
#include "../AColor.h"

#include "AutoDuck.h"

#include <wx/arrimpl.cpp>

/*
 * Default values for effect params
 */
#define PARAM_DEFAULT_DUCK_AMOUNT_DB -12.0
#define PARAM_DEFAULT_OUTER_FADE_DOWN_LEN 0.5
#define PARAM_DEFAULT_INNER_FADE_DOWN_LEN 0
#define PARAM_DEFAULT_OUTER_FADE_UP_LEN 0.5
#define PARAM_DEFAULT_INNER_FADE_UP_LEN 0
#define PARAM_DEFAULT_THRESHOLD_DB -30.0
#define PARAM_DEFAULT_MAXIMUM_PAUSE 1.0

/*
 * Common constants
 */

#define BUF_SIZE 131072 // number of samples to process at once
#define RMS_WINDOW_SIZE 100 // samples in circular RMS window buffer

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

WX_DECLARE_OBJARRAY(AutoDuckRegion, AutoDuckRegionArray);
WX_DEFINE_OBJARRAY(AutoDuckRegionArray);

/*
 * Effect implementation
 */

EffectAutoDuck::EffectAutoDuck()
{
   SetEffectFlags(BUILTIN_EFFECT | PROCESS_EFFECT | ADVANCED_EFFECT);

   gPrefs->Read(wxT("/Effects/AutoDuck/DuckAmountDb"),
      &mDuckAmountDb, PARAM_DEFAULT_DUCK_AMOUNT_DB);
   gPrefs->Read(wxT("/Effects/AutoDuck/InnerFadeDownLen"),
      &mInnerFadeDownLen, PARAM_DEFAULT_INNER_FADE_DOWN_LEN);
   gPrefs->Read(wxT("/Effects/AutoDuck/InnerFadeUpLen"),
      &mInnerFadeUpLen, PARAM_DEFAULT_INNER_FADE_UP_LEN);
   gPrefs->Read(wxT("/Effects/AutoDuck/OuterFadeDownLen"),
      &mOuterFadeDownLen, PARAM_DEFAULT_OUTER_FADE_DOWN_LEN);
   gPrefs->Read(wxT("/Effects/AutoDuck/OuterFadeUpLen"),
      &mOuterFadeUpLen, PARAM_DEFAULT_OUTER_FADE_UP_LEN);
   gPrefs->Read(wxT("/Effects/AutoDuck/ThresholdDb"),
      &mThresholdDb, PARAM_DEFAULT_THRESHOLD_DB);
   gPrefs->Read(wxT("/Effects/AutoDuck/MaximumPause"),
      &mMaximumPause, PARAM_DEFAULT_MAXIMUM_PAUSE);

   mControlTrack = NULL;
}

bool EffectAutoDuck::Init()
{
   gPrefs->Read(wxT("/Effects/AutoDuck/DuckAmountDb"),
      &mDuckAmountDb, PARAM_DEFAULT_DUCK_AMOUNT_DB);
   gPrefs->Read(wxT("/Effects/AutoDuck/InnerFadeDownLen"),
      &mInnerFadeDownLen, PARAM_DEFAULT_INNER_FADE_DOWN_LEN);
   gPrefs->Read(wxT("/Effects/AutoDuck/InnerFadeUpLen"),
      &mInnerFadeUpLen, PARAM_DEFAULT_INNER_FADE_UP_LEN);
   gPrefs->Read(wxT("/Effects/AutoDuck/OuterFadeDownLen"),
      &mOuterFadeDownLen, PARAM_DEFAULT_OUTER_FADE_DOWN_LEN);
   gPrefs->Read(wxT("/Effects/AutoDuck/OuterFadeUpLen"),
      &mOuterFadeUpLen, PARAM_DEFAULT_OUTER_FADE_UP_LEN);
   gPrefs->Read(wxT("/Effects/AutoDuck/ThresholdDb"),
      &mThresholdDb, PARAM_DEFAULT_THRESHOLD_DB);
   gPrefs->Read(wxT("/Effects/AutoDuck/MaximumPause"),
      &mMaximumPause, PARAM_DEFAULT_MAXIMUM_PAUSE);

   mControlTrack = NULL;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   bool lastWasSelectedWaveTrack = false;
   WaveTrack *controlTrackCandidate = NULL;

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
         } else
         {
            wxMessageBox(
               _("You selected a track which does not contain audio. AutoDuck can only process audio tracks."),
               /* i18n-hint: Auto duck is the name of an effect that 'ducks' (reduces the volume)
                * of the audio automatically when there is sound on another track.  Not as
                * in 'Donald-Duck'!*/
               _("Auto Duck"), wxICON_ERROR, mParent);
            return false;
         }
      }

      t = iter.Next();
   }

   if (!controlTrackCandidate)
   {
      wxMessageBox(
         _("Auto Duck needs a control track which must be placed below the selected track(s)."),
         _("Auto Duck"), wxICON_ERROR, mParent);
      return false;
   }

   mControlTrack = controlTrackCandidate;

   return true;
}

bool EffectAutoDuck::TransferParameters( Shuttle & shuttle )
{
   shuttle.TransferDouble(wxT("DuckAmountDb"), mDuckAmountDb,
      PARAM_DEFAULT_DUCK_AMOUNT_DB);
   shuttle.TransferDouble(wxT("InnerFadeDownLen"), mInnerFadeDownLen,
      PARAM_DEFAULT_INNER_FADE_DOWN_LEN);
   shuttle.TransferDouble(wxT("InnerFadeUpLen"), mInnerFadeUpLen,
      PARAM_DEFAULT_INNER_FADE_UP_LEN);
   shuttle.TransferDouble(wxT("OuterFadeDownLen"), mOuterFadeDownLen,
      PARAM_DEFAULT_OUTER_FADE_DOWN_LEN);
   shuttle.TransferDouble(wxT("OuterFadeUpLen"), mOuterFadeUpLen,
      PARAM_DEFAULT_OUTER_FADE_UP_LEN);
   shuttle.TransferDouble(wxT("ThresholdDb"), mThresholdDb,
      PARAM_DEFAULT_THRESHOLD_DB);
   shuttle.TransferDouble(wxT("MaximumPause"), mMaximumPause,
      PARAM_DEFAULT_MAXIMUM_PAUSE);

   return true;
}

bool EffectAutoDuck::CheckWhetherSkipEffect()
{
   return false;
}

void EffectAutoDuck::End()
{
   mControlTrack = NULL;
}

bool EffectAutoDuck::PromptUser()
{
   EffectAutoDuckDialog dlog(this, mParent);

   if (dlog.ShowModal() != wxID_OK)
      return false; // user cancelled dialog

   gPrefs->Write(wxT("/Effects/AutoDuck/DuckAmountDb"), mDuckAmountDb);
   gPrefs->Write(wxT("/Effects/AutoDuck/OuterFadeDownLen"), mOuterFadeDownLen);
   gPrefs->Write(wxT("/Effects/AutoDuck/OuterFadeUpLen"), mOuterFadeUpLen);
   gPrefs->Write(wxT("/Effects/AutoDuck/InnerFadeDownLen"), mInnerFadeDownLen);
   gPrefs->Write(wxT("/Effects/AutoDuck/InnerFadeUpLen"), mInnerFadeUpLen);
   gPrefs->Write(wxT("/Effects/AutoDuck/ThresholdDb"), mThresholdDb);
   gPrefs->Write(wxT("/Effects/AutoDuck/MaximumPause"), mMaximumPause);
   gPrefs->Flush();

   return true;
}

bool EffectAutoDuck::Process()
{
   sampleCount i;

   if (GetNumWaveTracks() == 0 || !mControlTrack)
      return false;

   bool cancel = false;

   sampleCount start =
      mControlTrack->TimeToLongSamples(mT0 + mOuterFadeDownLen);
   sampleCount end =
      mControlTrack->TimeToLongSamples(mT1 - mOuterFadeUpLen);

   if (end <= start)
      return false;

   // the minimum number of samples we have to wait until the maximum
   // pause has been exceeded
   double maxPause = mMaximumPause;

   // We don't fade in until we have time enough to actually fade out again
   if (maxPause < mOuterFadeDownLen + mOuterFadeUpLen)
      maxPause = mOuterFadeDownLen + mOuterFadeUpLen;

   sampleCount minSamplesPause =
      mControlTrack->TimeToLongSamples(maxPause);

   double threshold = pow(10.0, mThresholdDb/20);

   // adjust the threshold so we can compare it to the rmsSum value
   threshold = threshold * threshold * RMS_WINDOW_SIZE;

   int rmsPos = 0;
   float rmsSum = 0;
   float *rmsWindow = new float[RMS_WINDOW_SIZE];
   for (i = 0; i < RMS_WINDOW_SIZE; i++)
      rmsWindow[i] = 0;

   float *buf = new float[BUF_SIZE];

   bool inDuckRegion = false;

   // initialize the following two variables to prevent compiler warning
   double duckRegionStart = 0;
   sampleCount curSamplesPause = 0;

   // to make the progress bar appear more natural, we first look for all
   // duck regions and apply them all at once afterwards
   AutoDuckRegionArray regions;
   sampleCount pos = start;

   while (pos < end)
   {
      sampleCount len = end - pos;
      if (len > BUF_SIZE)
         len = BUF_SIZE;

      mControlTrack->Get((samplePtr)buf, floatSample, pos, (sampleCount)len);

      for (i = pos; i < pos + len; i++)
      {
         rmsSum -= rmsWindow[rmsPos];
         rmsWindow[rmsPos] = buf[i - pos] * buf[i - pos];
         rmsSum += rmsWindow[rmsPos];
         rmsPos = (rmsPos + 1) % RMS_WINDOW_SIZE;

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

      if (TotalProgress( ((double)(pos-start)) / (end-start) /
                         (GetNumWaveTracks() + 1) ))
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
      this->CopyInputTracks(); // Set up mOutputTracks.
      SelectedTrackListOfKindIterator iter(Track::Wave, mOutputTracks);
      Track *iterTrack = iter.First();

      int trackNumber = 0;

      while (iterTrack)
      {
         wxASSERT(iterTrack->GetKind() == Track::Wave);

         WaveTrack* t = (WaveTrack*)iterTrack;

         for (i = 0; i < (int)regions.GetCount(); i++)
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

   this->ReplaceProcessedTracks(!cancel);
   return !cancel;
}

// this currently does an exponential fade
bool EffectAutoDuck::ApplyDuckFade(int trackNumber, WaveTrack* t,
                                   double t0, double t1)
{
   bool cancel = false;

   sampleCount start = t->TimeToLongSamples(t0);
   sampleCount end = t->TimeToLongSamples(t1);

   float *buf = new float[BUF_SIZE];
   sampleCount pos = start;

   int fadeDownSamples = t->TimeToLongSamples(
      mOuterFadeDownLen + mInnerFadeDownLen);
   if (fadeDownSamples < 1)
      fadeDownSamples = 1;

   int fadeUpSamples = t->TimeToLongSamples(
      mOuterFadeUpLen + mInnerFadeUpLen);
   if (fadeUpSamples < 1)
      fadeUpSamples = 1;

   float fadeDownStep = mDuckAmountDb / fadeDownSamples;
   float fadeUpStep = mDuckAmountDb / fadeUpSamples;

   while (pos < end)
   {
      sampleCount len = end - pos;
      if (len > BUF_SIZE)
         len = BUF_SIZE;

      t->Get((samplePtr)buf, floatSample, pos, len);

      for (sampleCount i = pos; i < pos + len; i++)
      {
         float gainDown = fadeDownStep * (i - start);
         float gainUp = fadeUpStep * (end - i);;

         float gain;
         if (gainDown > gainUp)
            gain = gainDown;
         else
            gain = gainUp;
         if (gain < mDuckAmountDb)
            gain = mDuckAmountDb;

         buf[i - pos] *= pow(10.0, gain / 20.0);
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

/*
 * Effect dialog implementation
 */

#define ID_DUCK_AMOUNT_DB        10001
#define ID_THRESHOLD_DB          10002
#define ID_INNER_FADE_DOWN_LEN   10003
#define ID_INNER_FADE_UP_LEN     10004
#define ID_OUTER_FADE_DOWN_LEN   10005
#define ID_OUTER_FADE_UP_LEN     10006
#define ID_MAXIMUM_PAUSE         10007
#define ID_PANEL                 10008

BEGIN_EVENT_TABLE(EffectAutoDuckDialog, wxDialog)
   EVT_BUTTON(wxID_OK, EffectAutoDuckDialog::OnOk)
   EVT_BUTTON(wxID_CANCEL, EffectAutoDuckDialog::OnCancel)
   EVT_TEXT(ID_DUCK_AMOUNT_DB, EffectAutoDuckDialog::OnValueChanged)
   EVT_TEXT(ID_THRESHOLD_DB, EffectAutoDuckDialog::OnValueChanged)
   EVT_TEXT(ID_INNER_FADE_DOWN_LEN, EffectAutoDuckDialog::OnValueChanged)
   EVT_TEXT(ID_INNER_FADE_UP_LEN, EffectAutoDuckDialog::OnValueChanged)
   EVT_TEXT(ID_OUTER_FADE_DOWN_LEN, EffectAutoDuckDialog::OnValueChanged)
   EVT_TEXT(ID_OUTER_FADE_UP_LEN, EffectAutoDuckDialog::OnValueChanged)
   EVT_TEXT(ID_MAXIMUM_PAUSE, EffectAutoDuckDialog::OnValueChanged)
END_EVENT_TABLE()

EffectAutoDuckDialog::EffectAutoDuckDialog(EffectAutoDuck* effect,
   wxWindow *parent) : wxDialog(parent, -1, _("Auto Duck"),
                                wxDefaultPosition, wxDefaultSize)
{
   mEffect = effect;
   wxTextValidator vld(wxFILTER_NUMERIC);

   ShuttleGui S(this, eIsCreating);

   S.SetBorder(5);
   S.StartVerticalLay(true);
   {
      S.AddSpace(0, 5);

      mPanel = (EffectAutoDuckPanel*)
         S.AddWindow(new EffectAutoDuckPanel(this, ID_PANEL));

      S.AddSpace(0, 5);

      S.StartMultiColumn(6, wxCENTER);
      {
         mDuckAmountDbBox = S.Id(ID_DUCK_AMOUNT_DB).AddTextBox(
            _("Duck amount:"),
            Internat::ToDisplayString(mEffect->mDuckAmountDb), 10);
         S.AddUnits(_("dB"));
         mDuckAmountDbBox->SetValidator(vld);

         mMaximumPauseBox = S.Id(ID_MAXIMUM_PAUSE).AddTextBox(
            _("Maximum pause:"),
            Internat::ToDisplayString(mEffect->mMaximumPause), 10);
         S.AddUnits(_("seconds"));
         mMaximumPauseBox->SetValidator(vld);

         mOuterFadeDownLenBox = S.Id(ID_OUTER_FADE_DOWN_LEN).AddTextBox(
            _("Outer fade down length:"),
            Internat::ToDisplayString(mEffect->mOuterFadeDownLen), 10);
         S.AddUnits(_("seconds"));
         mOuterFadeDownLenBox->SetValidator(vld);

         mOuterFadeUpLenBox = S.Id(ID_OUTER_FADE_UP_LEN).AddTextBox(
            _("Outer fade up length:"),
            Internat::ToDisplayString(mEffect->mOuterFadeUpLen), 10);
         S.AddUnits(_("seconds"));
         mOuterFadeUpLenBox->SetValidator(vld);

         mInnerFadeDownLenBox = S.Id(ID_INNER_FADE_DOWN_LEN).AddTextBox(
            _("Inner fade down length:"),
            Internat::ToDisplayString(mEffect->mInnerFadeDownLen), 10);
         S.AddUnits(_("seconds"));
         mInnerFadeDownLenBox->SetValidator(vld);

         mInnerFadeUpLenBox = S.Id(ID_INNER_FADE_UP_LEN).AddTextBox(
            _("Inner fade up length:"),
            Internat::ToDisplayString(mEffect->mInnerFadeUpLen), 10);
         S.AddUnits(_("seconds"));
         mInnerFadeUpLenBox->SetValidator(vld);
      }
      S.EndMultiColumn();

      S.StartMultiColumn(3, wxCENTER);
      {
         mThresholdDbBox = S.Id(ID_THRESHOLD_DB).AddTextBox(
            _("Threshold:"),
            Internat::ToDisplayString(mEffect->mThresholdDb), 10);
         S.AddUnits(_("dB"));
         mThresholdDbBox->SetValidator(vld);
      }
      S.EndMultiColumn();

   }
   S.EndVerticalLay();

   S.AddStandardButtons();

   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();
}

void EffectAutoDuckDialog::OnOk(wxCommandEvent& WXUNUSED(event))
{
   double duckAmountDb = 0, thresholdDb = 0;
   double innerFadeDownLen = 0, innerFadeUpLen = 0;
   double outerFadeDownLen = 0, outerFadeUpLen = 0;
   double maximumPause = 0;

   bool success =
      mDuckAmountDbBox->GetValue().ToDouble(&duckAmountDb) &&
      duckAmountDb > -100 &&
      duckAmountDb < 0 &&
      mThresholdDbBox->GetValue().ToDouble(&thresholdDb) &&
      thresholdDb > -100 &&
      thresholdDb < 0 &&
      mInnerFadeDownLenBox->GetValue().ToDouble(&innerFadeDownLen) &&
      innerFadeDownLen >= 0 &&
      innerFadeDownLen < 1000 &&
      mInnerFadeUpLenBox->GetValue().ToDouble(&innerFadeUpLen) &&
      innerFadeUpLen >= 0 &&
      innerFadeUpLen < 1000 &&
      mOuterFadeDownLenBox->GetValue().ToDouble(&outerFadeDownLen) &&
      outerFadeDownLen >= 0 &&
      outerFadeDownLen < 1000 &&
      mOuterFadeUpLenBox->GetValue().ToDouble(&outerFadeUpLen) &&
      outerFadeUpLen >= 0 &&
      outerFadeUpLen < 1000 &&
      mMaximumPauseBox->GetValue().ToDouble(&maximumPause) &&
      maximumPause >= 0 &&
      maximumPause < 1000;

   if (!success)
   {
      wxMessageBox(_("Please enter valid values."), _("Auto Duck"),
         wxICON_ERROR, this);
      return;
   }

   mEffect->mDuckAmountDb = duckAmountDb;
   mEffect->mThresholdDb = thresholdDb;
   mEffect->mInnerFadeDownLen = innerFadeDownLen;
   mEffect->mInnerFadeUpLen = innerFadeUpLen;
   mEffect->mOuterFadeDownLen = outerFadeDownLen;
   mEffect->mOuterFadeUpLen = outerFadeUpLen;
   mEffect->mMaximumPause = maximumPause;

   EndModal(wxID_OK);
}

void EffectAutoDuckDialog::OnCancel(wxCommandEvent& WXUNUSED(event))
{
   EndModal(wxID_CANCEL);
}

void EffectAutoDuckDialog::OnValueChanged(wxCommandEvent& WXUNUSED(event))
{
   mPanel->Refresh(false);
}

/*
 * Effect dialog panel implementation
 */

#define CONTROL_POINT_REGION 10 // pixel distance to click on a control point
#define CONTROL_POINT_MIN_MOVE 5 // min mouse move until value is changed

#define TEXT_DISTANCE 15 // pixel distance text <-> center of control point

#define FADE_DOWN_START 150 // x coordinate
#define FADE_UP_START 450 // x coordinate
#define DUCK_AMOUNT_START 50 // y coordinate

#define MAX_DUCK_AMOUNT 0 // db
#define MIN_DUCK_AMOUNT -24 // db

#define MIN_FADE 0 // seconds
#define MAX_FADE 3 // seconds

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

BEGIN_EVENT_TABLE(EffectAutoDuckPanel, wxPanel)
   EVT_PAINT(EffectAutoDuckPanel::OnPaint)
   EVT_MOUSE_CAPTURE_CHANGED(EffectAutoDuckPanel::OnMouseCaptureChanged)
   EVT_MOUSE_CAPTURE_LOST(EffectAutoDuckPanel::OnMouseCaptureLost)
   EVT_LEFT_DOWN(EffectAutoDuckPanel::OnLeftDown)
   EVT_LEFT_UP(EffectAutoDuckPanel::OnLeftUp)
   EVT_MOTION(EffectAutoDuckPanel::OnMotion)
END_EVENT_TABLE()

EffectAutoDuckPanel::EffectAutoDuckPanel(EffectAutoDuckDialog* parent,
   wxWindowID id) : wxPanel(parent, id, wxDefaultPosition, wxSize(600, 300))
{
   mParent = parent;
   mCurrentControlPoint = none;
   mBackgroundBitmap = NULL;

   ResetControlPoints();
}

EffectAutoDuckPanel::~EffectAutoDuckPanel()
{
   if (mBackgroundBitmap)
      delete mBackgroundBitmap;
}

void EffectAutoDuckPanel::ResetControlPoints()
{
   mControlPoints[innerFadeDown] = wxPoint(-100,-100);
   mControlPoints[innerFadeUp] = wxPoint(-100,-100);
   mControlPoints[outerFadeDown] = wxPoint(-100,-100);
   mControlPoints[outerFadeUp] = wxPoint(-100,-100);
   mControlPoints[duckAmount] = wxPoint(-100,-100);
}

void EffectAutoDuckPanel::OnPaint(wxPaintEvent& WXUNUSED(event))
{
   int clientWidth, clientHeight;
   GetSize(&clientWidth, &clientHeight);

   if (!mBackgroundBitmap || mBackgroundBitmap->GetWidth() != clientWidth ||
       mBackgroundBitmap->GetHeight() != clientHeight)
   {
      if (mBackgroundBitmap)
         delete mBackgroundBitmap;
      mBackgroundBitmap = new wxBitmap(clientWidth, clientHeight);
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
   mParent->mDuckAmountDbBox->GetValue().ToDouble(&duckAmountDb);
   mParent->mInnerFadeDownLenBox->GetValue().ToDouble(&innerFadeDownLen);
   mParent->mInnerFadeUpLenBox->GetValue().ToDouble(&innerFadeUpLen);
   mParent->mOuterFadeDownLenBox->GetValue().ToDouble(&outerFadeDownLen);
   mParent->mOuterFadeUpLenBox->GetValue().ToDouble(&outerFadeUpLen);

   if (innerFadeDownLen < MIN_FADE || innerFadeDownLen > MAX_FADE ||
       innerFadeUpLen < MIN_FADE || innerFadeUpLen > MAX_FADE ||
       outerFadeDownLen < MIN_FADE || outerFadeDownLen > MAX_FADE ||
       outerFadeUpLen < MIN_FADE || outerFadeUpLen > MAX_FADE ||
       duckAmountDb < MIN_DUCK_AMOUNT || duckAmountDb > MAX_DUCK_AMOUNT)
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
   wxMouseCaptureChangedEvent& WXUNUSED(event))
{
   SetCursor(wxNullCursor);
   mCurrentControlPoint = none;
}

void EffectAutoDuckPanel::OnMouseCaptureLost(
   wxMouseCaptureLostEvent& WXUNUSED(event))
{
   mCurrentControlPoint = none;

   if (HasCapture())
   {
      ReleaseMouse();
   }
}

EffectAutoDuckPanel::EControlPoint
   EffectAutoDuckPanel::GetNearestControlPoint(const wxPoint& pt)
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

void EffectAutoDuckPanel::OnLeftDown(wxMouseEvent &evt)
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

      CaptureMouse();
   }
}

void EffectAutoDuckPanel::OnLeftUp(wxMouseEvent& WXUNUSED(event))
{
   if (mCurrentControlPoint != none)
   {
      mCurrentControlPoint = none;
      ReleaseMouse();
   }
}

void EffectAutoDuckPanel::OnMotion(wxMouseEvent &evt)
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
            if (newValue < MIN_FADE)
               newValue = MIN_FADE;
            if (newValue > MAX_FADE)
               newValue = MAX_FADE;
            mParent->mOuterFadeDownLenBox->SetValue(
               Internat::ToDisplayString(newValue));
            break;
         case outerFadeUp:
            newValue = ((double)(evt.GetX() - FADE_UP_START)) / FADE_SCALE;
            if (newValue < MIN_FADE)
               newValue = MIN_FADE;
            if (newValue > MAX_FADE)
               newValue = MAX_FADE;
            mParent->mOuterFadeUpLenBox->SetValue(
               Internat::ToDisplayString(newValue));
            break;
         case innerFadeDown:
            newValue = ((double)(evt.GetX() - FADE_DOWN_START)) / FADE_SCALE;
            if (newValue < MIN_FADE)
               newValue = MIN_FADE;
            if (newValue > MAX_FADE)
               newValue = MAX_FADE;
            mParent->mInnerFadeDownLenBox->SetValue(
               Internat::ToDisplayString(newValue));
            break;
         case innerFadeUp:
            newValue = ((double)(FADE_UP_START - evt.GetX())) / FADE_SCALE;
            if (newValue < MIN_FADE)
               newValue = MIN_FADE;
            if (newValue > MAX_FADE)
               newValue = MAX_FADE;
            mParent->mInnerFadeUpLenBox->SetValue(
               Internat::ToDisplayString(newValue));
            break;
         case duckAmount:
            newValue = ((double)(DUCK_AMOUNT_START - evt.GetY())) /
                           DUCK_AMOUNT_SCALE;
            if (newValue < MIN_DUCK_AMOUNT)
               newValue = MIN_DUCK_AMOUNT;
            if (newValue > MAX_DUCK_AMOUNT)
               newValue = MAX_DUCK_AMOUNT;
            mParent->mDuckAmountDbBox->SetValue(
               Internat::ToDisplayString(newValue));
            break;
         case none:
            wxASSERT(false); // should not happen
         }

         Refresh(false);
      }
   }
}
