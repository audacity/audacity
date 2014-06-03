/**********************************************************************

  Audacity: A Digital Audio Editor

  Meter.cpp

  Dominic Mazzoni
  Vaughan Johnson

  2004.06.25 refresh rate limited to 30mS, by ChackoN

*******************************************************************//**

\class Meter
\brief VU Meter, for displaying recording/playback level

  This is a bunch of common code that can display many different
  forms of VU meters and other displays.

  But note that a lot of later code here assumes these are
  MeterToolBar meters, e.g., Meter::StartMonitoring,
  so these are not as generic/common as originally intended.

*//****************************************************************//**

\class MeterBar
\brief A struct used by Meter to hold the position of one bar.

*//****************************************************************//**

\class MeterUpdateMsg
\brief Message used to update the Meter

*//****************************************************************//**

\class MeterUpdateQueue
\brief Queue of MeterUpdateMsg used to feed the Meter.

*//******************************************************************/

#include "../Audacity.h"
#include "../AudacityApp.h"

#include <wx/defs.h>
#include <wx/dcmemory.h>
#include <wx/image.h>
#include <wx/intl.h>
#include <wx/menu.h>
#include <wx/settings.h>
#include <wx/textdlg.h>
#include <wx/numdlg.h>
#include <wx/tooltip.h>
#include <wx/msgdlg.h>

#if defined(__WXMAC__)
#include <wx/mac/uma.h>
#endif

#include <math.h>

#include "Meter.h"

#include "../AudioIO.h"
#include "../AColor.h"
#include "../ImageManipulation.h"
//#include "../../images/MixerImages.h"
#include "../Project.h"
#include "../toolbars/MeterToolBar.h"
#include "../toolbars/ControlToolBar.h"
#include "../Prefs.h"

#include "../Theme.h"
#include "../AllThemeResources.h"
#include "../Experimental.h"


/* Updates to the meter are passed accross via meter updates, each contained in
 * a MeterUpdateMsg object */
wxString MeterUpdateMsg::toString()
{
wxString output;  // somewhere to build up a string in
output = wxString::Format(wxT("Meter update msg: %i channels, %i samples\n"), \
      kMaxMeterBars, numFrames);
for (int i = 0; i<kMaxMeterBars; i++)
   {  // for each channel of the meters
   output += wxString::Format(wxT("%f peak, %f rms "), peak[i], rms[i]);
   if (clipping[i])
      output += wxString::Format(wxT("clipped "));
   else
      output += wxString::Format(wxT("no clip "));
   output += wxString::Format(wxT("%i head, %i tail\n"), headPeakCount[i], tailPeakCount[i]);
   }
return output;
}

wxString MeterUpdateMsg::toStringIfClipped()
{
for (int i = 0; i<kMaxMeterBars; i++)
   {
   if (clipping[i] || (headPeakCount[i] > 0) || (tailPeakCount[i] > 0))
      return toString();
   }
return wxT("");
}

//
// The Meter passes itself messages via this queue so that it can
// communicate between the audio thread and the GUI thread.
// This class is as simple as possible in order to be thread-safe
// without needing mutexes.
//

MeterUpdateQueue::MeterUpdateQueue(int maxLen):
   mBufferSize(maxLen)
{
   mBuffer = new MeterUpdateMsg[mBufferSize];
   Clear();
}

// destructor
MeterUpdateQueue::~MeterUpdateQueue()
{
   delete[] mBuffer;
}

void MeterUpdateQueue::Clear()
{
   mStart = 0;
   mEnd = 0;
}

// Add a message to the end of the queue.  Return false if the
// queue was full.
bool MeterUpdateQueue::Put(MeterUpdateMsg &msg)
{
   int len = (mEnd + mBufferSize - mStart) % mBufferSize;

   // Never completely fill the queue, because then the
   // state is ambiguous (mStart==mEnd)
   if (len >= mBufferSize-1)
      return false;

   //wxLogDebug(wxT("Put: %s"), msg.toString().c_str());

   mBuffer[mEnd] = msg;
   mEnd = (mEnd+1)%mBufferSize;

   return true;
}

// Get the next message from the start of the queue.
// Return false if the queue was empty.
bool MeterUpdateQueue::Get(MeterUpdateMsg &msg)
{
   int len = (mEnd + mBufferSize - mStart) % mBufferSize;

   if (len == 0)
      return false;

   msg = mBuffer[mStart];
   mStart = (mStart+1)%mBufferSize;

   return true;
}

//
// Meter class
//

enum {
   OnMeterUpdateID = 6000,
   OnDisableMeterID,
   OnMonitorID,
   OnHorizontalID,
   OnAutomatedInputLevelAdjustmentID,
   OnVerticalID,
   OnMultiID,
   OnEqualizerID,
   OnWaveformID,
   OnLinearID,
   OnDBID,
   OnClipID,
   OnFloatID,
   OnPreferencesID
};

BEGIN_EVENT_TABLE(Meter, wxPanel)
   EVT_TIMER(OnMeterUpdateID, Meter::OnMeterUpdate)
   EVT_MOUSE_EVENTS(Meter::OnMouse)
   EVT_ERASE_BACKGROUND(Meter::OnErase)
   EVT_PAINT(Meter::OnPaint)
   EVT_SIZE(Meter::OnSize)
   EVT_MENU(OnDisableMeterID, Meter::OnDisableMeter)
   EVT_MENU(OnHorizontalID, Meter::OnHorizontal)
   EVT_MENU(OnVerticalID, Meter::OnVertical)
   EVT_MENU(OnMultiID, Meter::OnMulti)
   EVT_MENU(OnEqualizerID, Meter::OnEqualizer)
   EVT_MENU(OnWaveformID, Meter::OnWaveform)
   EVT_MENU(OnLinearID, Meter::OnLinear)
   EVT_MENU(OnDBID, Meter::OnDB)
   EVT_MENU(OnClipID, Meter::OnClip)
   EVT_MENU(OnMonitorID, Meter::OnMonitor)
#ifdef AUTOMATED_INPUT_LEVEL_ADJUSTMENT
   EVT_MENU(OnAutomatedInputLevelAdjustmentID, Meter::OnAutomatedInputLevelAdjustment)
#endif
   EVT_MENU(OnFloatID, Meter::OnFloat)
   EVT_MENU(OnPreferencesID, Meter::OnPreferences)
END_EVENT_TABLE()

IMPLEMENT_CLASS(Meter, wxPanel)

Meter::Meter(wxWindow* parent, wxWindowID id,
             bool isInput,
             const wxPoint& pos /*= wxDefaultPosition*/,
             const wxSize& size /*= wxDefaultSize*/,
             Style style /*= HorizontalStereo*/,
             float fDecayRate /*= 60.0f*/)
: wxPanel(parent, id, pos, size),
   mQueue(1024),
   mWidth(size.x), mHeight(size.y),
   mIsInput(isInput),
   mStyle(style),
   mDB(true),
   mDBRange(ENV_DB_RANGE),
   mDecay(true),
   mDecayRate(fDecayRate),
   mClip(true),
   mNumPeakSamplesToClip(3),
   mPeakHoldDuration(3),
   mT(0),
   mRate(0),
   mNumBars(0),
   mLayoutValid(false),
   mBitmap(NULL),
   mIcon(NULL)
{
   int i;

    wxColour backgroundColour =
      wxSystemSettings::GetColour(wxSYS_COLOUR_3DFACE);
    mBkgndBrush = wxBrush(backgroundColour, wxSOLID);

   UpdatePrefs();

   mPeakPeakPen = wxPen(theTheme.Colour( clrMeterPeak),        1, wxSOLID);
   mDisabledPen = wxPen(theTheme.Colour( clrMeterDisabledPen), 1, wxSOLID);

   mLeftSize = wxSize(0, 0);
   mRightSize = wxSize(0, 0);

   if (mIsInput) {
      mPen       = wxPen(   theTheme.Colour( clrMeterInputPen         ), 1, wxSOLID);
      mBrush     = wxBrush( theTheme.Colour( clrMeterInputBrush       ), wxSOLID);
      mRMSBrush  = wxBrush( theTheme.Colour( clrMeterInputRMSBrush    ), wxSOLID);
      mClipBrush = wxBrush( theTheme.Colour( clrMeterInputClipBrush   ), wxSOLID);
      mLightPen  = wxPen(   theTheme.Colour( clrMeterInputLightPen    ), 1, wxSOLID);
      mDarkPen   = wxPen(   theTheme.Colour( clrMeterInputDarkPen     ), 1, wxSOLID);
   }
   else {
      mPen       = wxPen(   theTheme.Colour( clrMeterOutputPen        ), 1, wxSOLID);
      mBrush     = wxBrush( theTheme.Colour( clrMeterOutputBrush      ), wxSOLID);
      mRMSBrush  = wxBrush( theTheme.Colour( clrMeterOutputRMSBrush   ), wxSOLID);
      mClipBrush = wxBrush( theTheme.Colour( clrMeterOutputClipBrush  ), wxSOLID);
      mLightPen  = wxPen(   theTheme.Colour( clrMeterOutputLightPen   ), 1, wxSOLID);
      mDarkPen   = wxPen(   theTheme.Colour( clrMeterOutputDarkPen    ), 1, wxSOLID);
   }

   mDisabledBkgndBrush = wxBrush(theTheme.Colour( clrMeterDisabledBrush), wxSOLID);
//   mDisabledBkgndBrush = wxBrush(
//            wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DSHADOW), wxSOLID);
//            wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DLIGHT), wxSOLID);
   if (mMeterDisabled) {
      mSavedBkgndBrush = mBkgndBrush;
      mSavedBrush = mBrush;
      mSavedRMSBrush = mRMSBrush;

      mBkgndBrush = mDisabledBkgndBrush;
      mBrush = mDisabledBkgndBrush;
      mRMSBrush = mDisabledBkgndBrush;
   }

   // MixerTrackCluster style has no menu, so disallows SetStyle, so never needs icon.
   if (mStyle != MixerTrackCluster)
      CreateIcon(2);

   mRuler.SetFonts(GetFont(), GetFont(), GetFont());

   mTimer.SetOwner(this, OnMeterUpdateID);
   // TODO: Yikes.  Hard coded sample rate.
   // JKC: I've looked at this, and it's benignish.  It just means that the meter
   // balistics are right for 44KHz and a bit more frisky than they should be
   // for higher sample rates.
   Reset(44100.0, true);
   for(i=0; i<kMaxMeterBars; i++) {
      mBar[i].clipping = false;
      mBar[i].isclipping = false;
   }
}

void Meter::Clear()
{
   mQueue.Clear();
}

void Meter::CreateIcon(int WXUNUSED(aquaOffset))
{
   /// \todo Remove wasteful delete/new pair.  It is done in every call to layout.
   if (mIcon) {
      delete mIcon;
      mIcon = NULL;
   }
   if(mIsInput)
   {
      /// JKC: !!!! If you pass theTheme.Bitmap(bmpMic) you get a white rather than a black mic.
      /// Weird behaviour in wxWidgets, I guess.
      mIcon = new wxBitmap(theTheme.Image( bmpMic ));
   }
   else
   {
      mIcon = new wxBitmap(theTheme.Image( bmpSpeaker ));
   }
}

Meter::~Meter()
{
   // LLL:  This prevents a crash during termination if monitoring
   //       is active.
   if (gAudioIO->IsMonitoring())
      gAudioIO->StopStream();
   delete mIcon;
   if (mBitmap)
      delete mBitmap;
}

void Meter::UpdatePrefs()
{
   mDBRange = gPrefs->Read(wxT("/GUI/EnvdBRange"), ENV_DB_RANGE);
   mMeterRefreshRate = gPrefs->Read(wxT("/Meter/MeterRefreshRate"), 30);

   // MixerTrackCluster style has no menu, so disallows disabling the meter.
   if (mStyle == MixerTrackCluster)
      mMeterDisabled = 0L;
   else if (mIsInput)
      mMeterDisabled = gPrefs->Read(wxT("/Meter/MeterInputDisabled"), (long)0);
   else
      mMeterDisabled = gPrefs->Read(wxT("/Meter/MeterOutputDisabled"), (long)0);
}

void Meter::OnErase(wxEraseEvent & WXUNUSED(event))
{
   // Ignore it to prevent flashing
}

void Meter::OnPaint(wxPaintEvent & WXUNUSED(event))
{
   wxPaintDC dc(this);
  #ifdef __WXMAC__
   // Mac OS X automatically double-buffers the screen for you,
   // so our bitmap is unneccessary
   HandlePaint(dc);
  #else
   if (!mBitmap)
      mBitmap = new wxBitmap(mWidth, mHeight);
   wxMemoryDC memDC;
   memDC.SelectObject(*mBitmap);
   HandlePaint(memDC);
   dc.Blit(0, 0, mWidth, mHeight, &memDC, 0, 0, wxCOPY, FALSE);
  #endif
}

void Meter::OnSize(wxSizeEvent & WXUNUSED(event))
{
   delete mBitmap;
   mBitmap = NULL;
   GetClientSize(&mWidth, &mHeight);

//::wxMessageBox(wxString::Format(" mHeight=%d, mWidth=%d", mHeight,mWidth));
   mLayoutValid = false;
}

void Meter::OnMouse(wxMouseEvent &evt)
{
   if (mStyle == MixerTrackCluster) // MixerTrackCluster style has no menu.
      return;

  #if wxUSE_TOOLTIPS // Not available in wxX11
   if (evt.Leaving()){
      GetActiveProject()->TP_DisplayStatusMessage(wxT(""));
   }
   else if (evt.Entering()) {
      // Display the tooltip in the status bar
      wxToolTip * pTip = this->GetToolTip();
      if( pTip ) {
         wxString tipText = pTip->GetTip();
         GetActiveProject()->TP_DisplayStatusMessage(tipText);
      }
   }
  #endif

   if (evt.RightDown() ||
       (evt.ButtonDown() && mMenuRect.Contains(evt.m_x, evt.m_y)))
   {
      wxMenu *menu = new wxMenu();
      // Note: these should be kept in the same order as the enum
      if (mMeterDisabled)
         menu->Append(OnDisableMeterID, _("Enable Meter"));
      else
         menu->Append(OnDisableMeterID, _("Disable Meter"));
      if (mIsInput) {
         if (gAudioIO->IsMonitoring())
            menu->Append(OnMonitorID, _("Stop Monitoring"));
         else
            menu->Append(OnMonitorID, _("Start Monitoring"));

         #ifdef AUTOMATED_INPUT_LEVEL_ADJUSTMENT
            if (gAudioIO->AILAIsActive())
               menu->Append(OnAutomatedInputLevelAdjustmentID, _("Stop Automated Input Level Adjustment"));
            else
               menu->Append(OnAutomatedInputLevelAdjustmentID, _("Start Automated Input Level Adjustment"));

            bool AVActive;
            gPrefs->Read(wxT("/AudioIO/AutomatedInputLevelAdjustment"), &AVActive, false);
            if (!AVActive || !GetActiveProject()->GetControlToolBar()->IsRecordDown())
               menu->Enable(OnAutomatedInputLevelAdjustmentID, false);
         #endif

      }
      menu->AppendSeparator();

      menu->Append(OnHorizontalID, _("Horizontal Stereo"));
      menu->Append(OnVerticalID, _("Vertical Stereo"));
      //menu->Append(OnMultiID, _("Vertical Multichannel"));
      //menu->Append(OnEqualizerID, _("Equalizer"));
      //menu->Append(OnWaveformID, _("Waveform"));
      //menu->Enable(OnHorizontalID + mStyle, false);
      menu->Enable(mStyle==VerticalStereo? OnVerticalID: OnHorizontalID, false);
      menu->AppendSeparator();

      menu->Append(OnLinearID, _("Linear"));
      menu->Append(OnDBID, _("dB"));
      menu->Enable(mDB? OnDBID: OnLinearID, false);
      //menu->AppendSeparator();
      //menu->Append(OnClipID, _("Turn on clipping"));
      //menu->AppendSeparator();
      //menu->Append(OnFloatID, _("Float Window"));

      menu->AppendSeparator();
      menu->Append(OnPreferencesID, _("Preferences..."));


      if (evt.RightDown())
         PopupMenu(menu, evt.m_x, evt.m_y);
      else
         PopupMenu(menu, mMenuRect.x + 1, mMenuRect.y + mMenuRect.height + 1);
      delete menu;
   }
   else if (evt.ButtonDown()) {
      if (mIsInput)
         StartMonitoring();
   }
}

void Meter::SetStyle(Meter::Style newStyle)
{
   // MixerTrackCluster disallows style change.
   if (mStyle == MixerTrackCluster)
      return;
   mStyle = newStyle;
   mLayoutValid = false;
   Refresh(true);
}

void Meter::Reset(double sampleRate, bool resetClipping)
{
   int j;

   mT = 0;
   mRate = sampleRate;
   for(j=0; j<kMaxMeterBars; j++)
      ResetBar(&mBar[j], resetClipping);

   // wxTimers seem to be a little unreliable - sometimes they stop for
   // no good reason, so this "primes" it every now and then...
   mTimer.Stop();

   // While it's stopped, empty the queue
   MeterUpdateMsg msg;
   while(mQueue.Get(msg)) {
   }

   mTimer.Start(1000 / mMeterRefreshRate);

   mLayoutValid = false;
   Refresh(false);
}

static float floatMax(float a, float b)
{
   return a>b? a: b;
}

static int intmin(int a, int b)
{
   return a<b? a: b;
}

static int intmax(int a, int b)
{
   return a>b? a: b;
}

static float ClipZeroToOne(float z)
{
   if (z > 1.0)
      return 1.0;
   else if (z < 0.0)
      return 0.0;
   else
      return z;
}

static float ToDB(float v, float range)
{
   double db;
   if (v > 0)
      db = 20 * log10(fabs(v));
   else
      db = -999;
   return ClipZeroToOne((db + range) / range);
}

void Meter::UpdateDisplay(int numChannels, int numFrames, float *sampleData)
{
   int i, j;
   float *sptr = sampleData;
   int num = intmin(numChannels, mNumBars);
   MeterUpdateMsg msg;

   msg.numFrames = numFrames;
   for(j=0; j<mNumBars; j++) {
      msg.peak[j] = 0;
      msg.rms[j] = 0;
      msg.clipping[j] = false;
      msg.headPeakCount[j] = 0;
      msg.tailPeakCount[j] = 0;
   }

   for(i=0; i<numFrames; i++) {
      for(j=0; j<num; j++) {
         msg.peak[j] = floatMax(msg.peak[j], fabs(sptr[j]));
         msg.rms[j] += sptr[j]*sptr[j];

         // In addition to looking for mNumPeakSamplesToClip peaked
         // samples in a row, also send the number of peaked samples
         // at the head and tail, in case there's a run of peaked samples
         // that crosses block boundaries
         if (fabs(sptr[j])>=MAX_AUDIO) {
            if (msg.headPeakCount[j]==i)
               msg.headPeakCount[j]++;
            msg.tailPeakCount[j]++;
            if (msg.tailPeakCount[j] > mNumPeakSamplesToClip)
               msg.clipping[j] = true;
         }
         else
            msg.tailPeakCount[j] = 0;
      }
      sptr += numChannels;
   }
   for(j=0; j<mNumBars; j++)
      msg.rms[j] = sqrt(msg.rms[j]/numFrames);

   if (mDB) {
      for(j=0; j<mNumBars; j++) {
         msg.peak[j] = ToDB(msg.peak[j], mDBRange);
         msg.rms[j] = ToDB(msg.rms[j], mDBRange);
      }
   }

   mQueue.Put(msg);
}

// Vaughan, 2010-11-29: This not currently used. See comments in MixerTrackCluster::UpdateMeter().
//void Meter::UpdateDisplay(int numChannels, int numFrames,
//                           // Need to make these double-indexed arrays if we handle more than 2 channels.
//                           float* maxLeft, float* rmsLeft,
//                           float* maxRight, float* rmsRight,
//                           const sampleCount kSampleCount)
//{
//   int i, j;
//   int num = intmin(numChannels, mNumBars);
//   MeterUpdateMsg msg;
//
//   msg.numFrames = kSampleCount;
//   for(j=0; j<mNumBars; j++) {
//      msg.peak[j] = 0.0;
//      msg.rms[j] = 0.0;
//      msg.clipping[j] = false;
//      msg.headPeakCount[j] = 0;
//      msg.tailPeakCount[j] = 0;
//   }
//
//   for(i=0; i<numFrames; i++) {
//      for(j=0; j<num; j++) {
//         msg.peak[j] = floatMax(msg.peak[j], ((j == 0) ? maxLeft[i] : maxRight[i]));
//         msg.rms[j] = floatMax(msg.rms[j], ((j == 0) ? rmsLeft[i] : rmsRight[i]));
//
//         // In addition to looking for mNumPeakSamplesToClip peaked
//         // samples in a row, also send the number of peaked samples
//         // at the head and tail, in case there's a run
//         // of peaked samples that crosses block boundaries.
//         if (fabs((j == 0) ? maxLeft[i] : maxRight[i]) >= MAX_AUDIO)
//         {
//            if (msg.headPeakCount[j]==i)
//               msg.headPeakCount[j]++;
//            msg.tailPeakCount[j]++;
//            if (msg.tailPeakCount[j] > mNumPeakSamplesToClip)
//               msg.clipping[j] = true;
//         }
//         else
//            msg.tailPeakCount[j] = 0;
//      }
//   }
//
//   if (mDB) {
//      for(j=0; j<mNumBars; j++) {
//         msg.peak[j] = ToDB(msg.peak[j], mDBRange);
//         msg.rms[j] = ToDB(msg.rms[j], mDBRange);
//      }
//   }
//
//   mQueue.Put(msg);
//}

void Meter::OnMeterUpdate(wxTimerEvent & WXUNUSED(event))
{
   MeterUpdateMsg msg;
   int numChanges = 0;
#ifdef AUTOMATED_INPUT_LEVEL_ADJUSTMENT
   double maxPeak = 0.0;
   bool discarded = false;
#endif
   // There may have been several update messages since the last
   // time we got to this function.  Catch up to real-time by
   // popping them off until there are none left.  It is necessary
   // to process all of them, otherwise we won't handle peaks and
   // peak-hold bars correctly.
   while(mQueue.Get(msg)) {
      numChanges++;
      double deltaT = msg.numFrames / mRate;
      int j;

      // <Why is this in the loop, rather than top of the method?
      //    Or just a condition on the following, so we pop all the msgs while disabled?>
      if (mMeterDisabled)
         return;
      //wxLogDebug(wxT("Pop: %s"), msg.toString().c_str());

      mT += deltaT;
      for(j=0; j<mNumBars; j++) {
         mBar[j].isclipping = false;

         if (mDecay) {
            if (mDB) {
               float decayAmount = mDecayRate * deltaT / mDBRange;
               mBar[j].peak = floatMax(msg.peak[j],
                                       mBar[j].peak - decayAmount);
            }
            else {
               double decayAmount = mDecayRate * deltaT;
               double decayFactor = pow(10.0, -decayAmount/20);
               mBar[j].peak = floatMax(msg.peak[j],
                                       mBar[j].peak * decayFactor);
            }
         }
         else
            mBar[j].peak = msg.peak[j];

         // This smooths out the RMS signal
         float smooth = pow(0.9, (double)msg.numFrames/1024.0);
         mBar[j].rms = mBar[j].rms * smooth + msg.rms[j] * (1.0 - smooth);

         if (mT - mBar[j].peakHoldTime > mPeakHoldDuration ||
             mBar[j].peak > mBar[j].peakHold) {
            mBar[j].peakHold = mBar[j].peak;
            mBar[j].peakHoldTime = mT;
         }

         if (mBar[j].peak > mBar[j].peakPeakHold )
            mBar[j].peakPeakHold = mBar[j].peak;

         if (msg.clipping[j] ||
             mBar[j].tailPeakCount+msg.headPeakCount[j] >=
             mNumPeakSamplesToClip){
            mBar[j].clipping = true;
            mBar[j].isclipping = true;
         }

         mBar[j].tailPeakCount = msg.tailPeakCount[j];
#ifdef AUTOMATED_INPUT_LEVEL_ADJUSTMENT
         if (mT > gAudioIO->AILAGetLastDecisionTime()) {
            discarded = false;
            maxPeak = msg.peak[j] > maxPeak ? msg.peak[j] : maxPeak;
            printf("%f@%f ", msg.peak[j], mT);
         }
         else {
            discarded = true;
            printf("%f@%f discarded\n", msg.peak[j], mT);
         }
#endif
      }
   } // while

   if (numChanges > 0) {
      #ifdef AUTOMATED_INPUT_LEVEL_ADJUSTMENT
         if (gAudioIO->AILAIsActive() && mIsInput && !discarded) {
            gAudioIO->AILAProcess(maxPeak);
            putchar('\n');
         }
      #endif
      RepaintBarsNow();
   }
}

float Meter::GetMaxPeak()
{
   int j;
   float maxPeak = 0.;

   for(j=0; j<mNumBars; j++)
      maxPeak = mBar[j].peak > maxPeak ? mBar[j].peak : maxPeak;

   return(maxPeak);
}

double Meter::ToLinearIfDB(double value)
{
   if (mDB)
      value = pow(10.0, (-(1.0-value)*mDBRange)/20.0);
   return value;
}

wxFont Meter::GetFont()
{
   int fontSize = 10;
#if defined __WXMSW__
   fontSize = 8;
#endif

   return wxFont(fontSize, wxSWISS, wxNORMAL, wxNORMAL);
}

void Meter::ResetBar(MeterBar *b, bool resetClipping)
{
   b->peak = 0.0;
   b->rms = 0.0;
   b->peakHold = 0.0;
   b->peakHoldTime = 0.0;
   if (resetClipping)
   {
      b->clipping = false;
      b->peakPeakHold =0.0;
   }
   b->isclipping = false;
   b->tailPeakCount = 0;
}

bool Meter::IsClipping()
{
   for (int c = 0; c < kMaxMeterBars; c++)
      if (mBar[c].isclipping)
         return true;
   return false;
}

void Meter::HandleLayout()
{
   int iconWidth = 0;
   int iconHeight = 0;
   int menuWidth = 0;
   int menuHeight = 0;
   if (mStyle != MixerTrackCluster)
   {
      iconWidth = mIcon->GetWidth();
      iconHeight = mIcon->GetHeight();
      menuWidth = 17;
      menuHeight = 14;
   }
   int width = mWidth;
   int height = mHeight;
   int left = 0, top = 0;
   int right, bottom;
   int barw, barh;
   int i;

   mRuler.SetFlip(true);
   mRuler.SetLabelEdges(true);

   switch(mStyle) {
   default:
      wxPrintf(wxT("Style not handled yet!\n"));
      break;
   case VerticalStereo:
      mMenuRect = wxRect(mWidth - menuWidth - 5, mHeight - menuHeight - 2,
                         menuWidth, menuHeight);
      if (mHeight < (menuHeight + iconHeight + 8))
         mIconPos = wxPoint(-999, -999); // Don't display
      else
         mIconPos = wxPoint(mWidth - iconWidth - 1, 1);
      width = intmin(mWidth-(iconWidth+2), mWidth-(menuWidth+3));
      // No break. Fall-through to MixerTrackCluster is intentional.
   case MixerTrackCluster:
      // Doesn't show menu, icon, or L/R labels,
      // but otherwise like VerticalStereo.

      if (width >= mLeftSize.x + mRightSize.x + 24) {
         if (mStyle != MixerTrackCluster)
         {
            mLeftTextPos = wxPoint(2, height-2-mLeftSize.y);
            left += mLeftSize.x+4;
         }
         mRightTextPos = wxPoint(width-mLeftSize.x, height-2-mLeftSize.y);
         width -= mLeftSize.x + mRightSize.x + 8; //v ...but then -8 in UmixIt? -- for vertical only?
      }
      barw = (width-2)/2;
      barh = height - 4;
      mNumBars = 2;
      mBar[0].vert = true;
      ResetBar(&mBar[0], false);
      mBar[0].r = wxRect(left + width/2 - barw - 1, 2, barw, barh);
      if (mClip) {
         mBar[0].rClip = mBar[0].r;
         mBar[0].rClip.height = 3;
         mBar[0].r.y += 4;
         mBar[0].r.height -= 4;
      }
      mBar[1].vert = true;
      ResetBar(&mBar[1], false);
      mBar[1].r = wxRect(left + width/2 + 1, 2, barw, barh);
      if (mClip) {
         mBar[1].rClip = mBar[1].r;
         mBar[1].rClip.height = 3;
         mBar[1].r.y += 4;
         mBar[1].r.height -= 4;
      }
      mRuler.SetOrientation(wxVERTICAL);
      mRuler.SetBounds(mBar[1].r.x + mBar[1].r.width + 1,
                       mBar[1].r.y,
                       mBar[1].r.x + width,
                       mBar[1].r.y + mBar[1].r.height);
      if (mDB) {
         mRuler.SetRange(0, -mDBRange);
         mRuler.SetFormat(Ruler::LinearDBFormat);
      }
      else {
         mRuler.SetRange(1, 0);
         mRuler.SetFormat(Ruler::RealFormat);
      }
      if (mStyle != MixerTrackCluster) // MixerTrackCluster style has no menu.
         mRuler.OfflimitsPixels(mMenuRect.y-mBar[1].r.y, mBar[1].r.height);
      break;
   case HorizontalStereo:
      if (mWidth < menuWidth + iconWidth + 8) {
         mIconPos = wxPoint(-999, -999); // Don't display icon
         mMenuRect = wxRect(2, mHeight - menuHeight - 2,
                            menuWidth, menuHeight);
      }
      else {
         mIconPos = wxPoint(2, mHeight - iconHeight);
         mMenuRect = wxRect(iconWidth + 2, mHeight - menuHeight - 5,
                            menuWidth, menuHeight);
      }
      height = intmin(height-(menuHeight+3), height-iconHeight) - 2;
      left = 2 + intmax(mLeftSize.x, mRightSize.x);
      width -= left;
      mLeftTextPos = wxPoint(2, (height)/4 - mLeftSize.y/2);
      mRightTextPos = wxPoint(2, (height*3)/4 - mLeftSize.y/2);
      barw = width - 4;
      barh = (height-2)/2;
      mNumBars = 2;
      mBar[0].vert = false;
      ResetBar(&mBar[0], false);
      mBar[0].r = wxRect(left+2, height/2 - barh - 1, barw, barh);
      if (mClip) {
         mBar[0].rClip = mBar[0].r;
         mBar[0].rClip.x += mBar[0].rClip.width-3;
         mBar[0].rClip.width = 3;
         mBar[0].r.width -= 4;
      }
      mBar[1].vert = false;
      ResetBar(&mBar[1], false);
      mBar[1].r = wxRect(left+2, height/2 + 1, barw, barh);
      if (mClip) {
         mBar[1].rClip = mBar[1].r;
         mBar[1].rClip.x += mBar[1].rClip.width-3;
         mBar[1].rClip.width = 3;
         mBar[1].r.width -= 4;
      }
      mRuler.SetOrientation(wxHORIZONTAL);
      mRuler.SetBounds(mBar[1].r.x,
                       mBar[1].r.y + mBar[1].r.height + 1,
                       mBar[1].r.x + mBar[1].r.width,
                       mWidth);
      if (mDB) {
         mRuler.SetRange(-mDBRange, 0);
         mRuler.SetFormat(Ruler::LinearDBFormat);
      }
      else {
         mRuler.SetRange(0, 1);
         mRuler.SetFormat(Ruler::RealFormat);
      }
      mRuler.OfflimitsPixels(0, mMenuRect.x+mMenuRect.width-4);
      break;
   case Waveform:
      mNumBars = 0;
      break;
   }

   if (mNumBars > 0) {
      // Compute bounding rectangle of all bars (to save time when
      // blitting just the bars to the screen)
      left = mBar[0].r.x;
      top = mBar[0].r.y;
      right = mBar[0].r.x + mBar[0].r.width;
      bottom = mBar[0].r.y + mBar[0].r.height;
      for(i=1; i<mNumBars; i++) {
         left = intmin(left, mBar[i].r.x);
         top = intmin(top, mBar[i].r.y);
         right = intmax(right, mBar[i].r.x + mBar[i].r.width);
         bottom = intmax(bottom, mBar[i].r.y + mBar[i].r.height);
         left = intmin(left, mBar[i].rClip.x);
         top = intmin(top, mBar[i].rClip.y);
         right = intmax(right, mBar[i].rClip.x + mBar[i].rClip.width);
         bottom = intmax(bottom, mBar[i].rClip.y + mBar[i].rClip.height);

      }
      mAllBarsRect = wxRect(left, top, right-left+1, bottom-top+1);
   }

   // MixerTrackCluster style has no popup, so disallows SetStyle, so never needs icon.
   if (mStyle != MixerTrackCluster)
      CreateIcon(mIconPos.y % 4);

   mLayoutValid = true;
}

void Meter::HandlePaint(wxDC &dc)
{
   int i;
   /* i18n-hint: One-letter abbreviation for Left, in VU Meter */
   wxString mLeftText = _("L");  // used in a couple of places in this method.
   // not a class member so it changes when the UI language is changed.
   /* i18n-hint: One-letter abbreviation for Right, in VU Meter */
   wxString mRightText = _("R");

   dc.SetFont(GetFont());
   if (mLeftSize.x == 0) { // Not yet initialized to dc.
      if (mStyle != MixerTrackCluster) // MixerTrackCluster style has no L/R labels.
      {
         dc.GetTextExtent(mLeftText, &mLeftSize.x, &mLeftSize.y);
         dc.GetTextExtent(mRightText, &mRightSize.x, &mRightSize.y);
      }
      if ((mStyle == VerticalStereo) || (mStyle == MixerTrackCluster)) // VerticalMulti?
      {
         // For any vertical style, also need mRightSize big enough for Ruler width.
         int rulerWidth;
         int rulerHeight;
         dc.GetTextExtent(wxT("-888"), &rulerWidth, &rulerHeight); // -888 is nice and wide.
         if (mRightSize.x < rulerWidth)
            mRightSize.x = rulerWidth;
      }
   }

   if (!mLayoutValid)
      HandleLayout();

#ifndef USE_AQUA_THEME
#ifdef EXPERIMENTAL_THEMING
   if( !mMeterDisabled )
   {
      mBkgndBrush.SetColour( GetParent()->GetBackgroundColour() );
   }
#endif

   dc.SetPen(*wxTRANSPARENT_PEN);
   dc.SetBrush(mBkgndBrush);
   dc.DrawRectangle(0, 0, mWidth, mHeight);
#endif

   // MixerTrackCluster style has no icon or menu.
   if (mStyle != MixerTrackCluster)
   {
      dc.DrawBitmap(*mIcon, mIconPos.x, mIconPos.y, true);

      // Draws a beveled button and a down pointing triangle.
      // The style and sizing matches the ones in the title
      // bar of the waveform left-hand-side panels.

      wxRect r = mMenuRect;
      AColor::Bevel(dc, true, r);
      dc.SetBrush(*wxBLACK_BRUSH);
      dc.SetPen(*wxBLACK_PEN);
      AColor::Arrow(dc,
                    r.x + 3,
                    r.y + 5,
                    10);
   }

   if (mNumBars>0)
      mRuler.Draw(dc);

   // MixerTrackCluster style has no L/R labels.
   if (mStyle != MixerTrackCluster)
   {
      dc.SetFont(GetFont());
      dc.DrawText(mLeftText, mLeftTextPos.x, mLeftTextPos.y);
      dc.DrawText(mRightText, mRightTextPos.x, mRightTextPos.y);
   }

   for(i=0; i<mNumBars; i++)
      DrawMeterBar(dc, &mBar[i]);
}

void Meter::RepaintBarsNow()
{
   if (!mLayoutValid)
      return;

   wxClientDC dc(this);
   int i;

  #ifdef __WXMAC__
   // Mac OS X automatically double-buffers the screen for you,
   // so our bitmap is unneccessary
   for(i=0; i<mNumBars; i++)
      DrawMeterBar(dc, &mBar[i]);
  #else
   if (!mBitmap)
      mBitmap = new wxBitmap(mWidth, mHeight);
   wxMemoryDC memDC;
   memDC.SelectObject(*mBitmap);
   for(i=0; i<mNumBars; i++)
      DrawMeterBar(memDC, &mBar[i]);
   dc.Blit(mAllBarsRect.x, mAllBarsRect.y,
           mAllBarsRect.width, mAllBarsRect.height,
           &memDC, mAllBarsRect.x, mAllBarsRect.y,
           wxCOPY, false);
  #endif
}

void Meter::DrawMeterBar(wxDC &dc, MeterBar *meterBar)
{
   wxRect r = meterBar->r;
   wxRect rRMS = meterBar->r;
   dc.SetPen(*wxTRANSPARENT_PEN);
   dc.SetBrush(mBkgndBrush);
   dc.DrawRectangle(r);
   AColor::Bevel(dc, false, r);

   /*
   AColor::Dark(&dc, false);
   for(i=0; i<mNumTicks; i++)
      if (meterBar->vert)
         AColor::Line(dc, r.x+r.width/2-1, mTick[i], r.x+r.width/2+1, mTick[i]);
      else
         AColor::Line(dc, mTick[i], r.y+r.height/2-1, mTick[i], r.y+r.height/2+1);
   */

   dc.SetBrush(*wxTRANSPARENT_BRUSH);
   dc.SetPen(mPen);

   if (meterBar->vert) {
      int ht = (int)(meterBar->peakHold * r.height + 0.5);
      AColor::Line(dc, r.x+1, r.y + r.height - ht,
                   r.x+r.width-1, r.y + r.height - ht);
      if (ht > 1)
         AColor::Line(dc, r.x+1, r.y + r.height - ht + 1,
                      r.x+r.width-1, r.y + r.height - ht + 1);
      dc.SetPen(mPeakPeakPen);
      ht = (int)(meterBar->peakPeakHold * r.height + 0.5);
      AColor::Line(dc, r.x+1, r.y + r.height - ht,
                   r.x+r.width-1, r.y + r.height - ht);
      if (ht > 1)
         AColor::Line(dc, r.x+1, r.y + r.height - ht + 1,
                      r.x+r.width-1, r.y + r.height - ht + 1);

      dc.SetPen(mPen);
      ht = (int)(meterBar->peak * r.height + 0.5);
      r = wxRect(r.x, r.y + r.height - ht,
                 r.width, ht);
      ht = (int)(meterBar->rms * rRMS.height + 0.5);
      rRMS = wxRect(rRMS.x, rRMS.y + rRMS.height - ht,
                    rRMS.width, ht);

   }
   else {
      int wd = (int)(meterBar->peakHold * r.width + 0.5);
      AColor::Line(dc, r.x + wd, r.y + 1, r.x + wd, r.y + r.height - 1);
      if (wd > 1)
         AColor::Line(dc, r.x + wd - 1, r.y + 1, r.x + wd - 1, r.y + r.height - 1);

      dc.SetPen(mPeakPeakPen);
      wd = (int)(meterBar->peakPeakHold * r.width + 0.5);
      AColor::Line(dc, r.x + wd, r.y + 1, r.x + wd, r.y + r.height - 1);
      if (wd > 1)
         AColor::Line(dc, r.x + wd - 1, r.y + 1, r.x + wd - 1, r.y + r.height - 1);

      dc.SetPen(mPen);
      wd = (int)(meterBar->peak * r.width + 0.5);
      r = wxRect(r.x, r.y,
                 wd, r.height);
      wd = (int)(meterBar->rms * rRMS.width + 0.5);
      rRMS = wxRect(rRMS.x, rRMS.y,
                    wd, rRMS.height);
   }
   dc.SetPen(*wxTRANSPARENT_PEN);
   dc.SetBrush(mBrush);
   dc.DrawRectangle(r);
   dc.SetBrush(mRMSBrush);
   dc.DrawRectangle(rRMS);

   dc.SetBrush(*wxTRANSPARENT_BRUSH);
   dc.SetPen(mLightPen);
   AColor::Line(dc, r.x, r.y, r.x + r.width, r.y);
   AColor::Line(dc, r.x, r.y, r.x, r.y + r.height);
   dc.SetPen(mDarkPen);
   AColor::Line(dc, r.x + r.width, r.y, r.x + r.width, r.y + r.height);
   AColor::Line(dc, r.x, r.y + r.height, r.x + r.width, r.y + r.height);

   if (mClip) {
      if (meterBar->clipping)
         dc.SetBrush(mClipBrush);
      else
         dc.SetBrush(mBkgndBrush);
      dc.SetPen(*wxTRANSPARENT_PEN);
      dc.DrawRectangle(meterBar->rClip);
      dc.SetBrush(*wxTRANSPARENT_BRUSH);
      AColor::Bevel(dc, false, meterBar->rClip);
   }
}

bool Meter::IsMeterDisabled() {return mMeterDisabled!=0;}

void Meter::StartMonitoring()
{

   if (gAudioIO->IsMonitoring())
      gAudioIO->StopStream();
   else {
      if (mMeterDisabled){
         wxCommandEvent dummy;
         OnDisableMeter(dummy);
      }

      AudacityProject *p = GetActiveProject();
      if (p) {
         gAudioIO->StartMonitoring(p->GetRate());

         MeterToolBar *bar = p->GetMeterToolBar();
         if (bar) {
            Meter *play, *record;
            bar->GetMeters(&play, &record);
            gAudioIO->SetMeters(record, play);
         }
      }
   }

}

//
// Pop-up menu handlers
//

void Meter::OnDisableMeter(wxCommandEvent & WXUNUSED(event))
{
   if (mMeterDisabled) //Enable
      {
      mLightPen = mSavedLightPen;
      mDarkPen = mSavedDarkPen;
      mBkgndBrush = mSavedBkgndBrush;
      mBrush = mSavedBrush;
      mRMSBrush = mSavedRMSBrush;

      mBkgndBrush = mSavedBkgndBrush;
      mLightPen = mSavedLightPen;
      Refresh(false);

      mMeterDisabled = false;
      }
   else
      {
      if (mIsInput) {
         if (gAudioIO->IsMonitoring())
            gAudioIO->StopStream();
      }
      mSavedLightPen = mLightPen;
      mSavedDarkPen = mDarkPen;
      mSavedBkgndBrush = mBkgndBrush;
      mSavedBrush = mBrush;
      mSavedRMSBrush = mRMSBrush;

      mLightPen = mDisabledPen;
      mDarkPen = mDisabledPen;
      mBkgndBrush = mDisabledBkgndBrush;
      mBrush = mDisabledBkgndBrush;
      mRMSBrush = mDisabledBkgndBrush;
      mLayoutValid = false;
      Refresh(false);

      mMeterDisabled = true;
      }
   if (mIsInput)
      {
      gPrefs->Write(wxT("/Meter/MeterInputDisabled"), mMeterDisabled);
      }
   else
      {
      gPrefs->Write(wxT("/Meter/MeterOutputDisabled"), mMeterDisabled);
      }
   gPrefs->Flush();
}

void Meter::OnHorizontal(wxCommandEvent & WXUNUSED(event))
{
   SetStyle(HorizontalStereo);
}

void Meter::OnVertical(wxCommandEvent & WXUNUSED(event))
{
   SetStyle(VerticalStereo);
}

void Meter::OnMulti(wxCommandEvent & WXUNUSED(event))
{
   SetStyle(VerticalMulti);
}

void Meter::OnEqualizer(wxCommandEvent & WXUNUSED(event))
{
   SetStyle(Equalizer);
}

void Meter::OnWaveform(wxCommandEvent & WXUNUSED(event))
{
   SetStyle(Waveform);
}

void Meter::OnLinear(wxCommandEvent & WXUNUSED(event))
{
   mDB = false;
   mLayoutValid = false;
   Refresh(false);
}

void Meter::OnDB(wxCommandEvent & WXUNUSED(event))
{
   mDB = true;
   mLayoutValid = false;
   Refresh(false);
}

void Meter::OnClip(wxCommandEvent & WXUNUSED(event))
{
}

void Meter::OnMonitor(wxCommandEvent & WXUNUSED(event))
{
   StartMonitoring();
}

#ifdef AUTOMATED_INPUT_LEVEL_ADJUSTMENT
void Meter::OnAutomatedInputLevelAdjustment(wxCommandEvent &evt)
{
   if (gAudioIO->AILAIsActive()) {
      gAudioIO->AILADisable();
      AudacityProject *p = GetActiveProject();
      if (p) p->TP_DisplayStatusMessage(_("Automated Input Level Adjustment stopped as requested by user."));
   }
   else
      gAudioIO->AILAInitialize();
}
#endif

void Meter::OnFloat(wxCommandEvent & WXUNUSED(event))
{
}

void Meter::OnPreferences(wxCommandEvent & WXUNUSED(event))
{
   wxNumberEntryDialog
      d(this,
        _("Higher refresh rates make the meter show more frequent\nchanges. A rate of 30 per second or less should prevent\nthe meter affecting audio quality on slower machines."),
        _("Meter refresh rate per second [1-100]: "),
        _("Meter Preferences"),
        mMeterRefreshRate,
       1,
       100);

   //#if defined(__WXMAC__)
      // WXMAC doesn't support wxFRAME_FLOAT_ON_PARENT, so we do
      //
      // LL:  I've commented this out because if you have, for instance, the meter
      //      toolbar undocked and large and then you open a dialog like an effect,
      //      the dialog may appear behind the dialog and you can't move either one.
      //
      //      However, I'm leaving it here because I don't remember why I'd included
      //      it in the first place.
      // SetWindowClass((WindowRef)d.MacGetWindowRef(), kFloatingWindowClass);
   //#endif

   if (d.ShowModal() == wxID_OK) {
      mMeterRefreshRate = d.GetValue();
      gPrefs->Write(wxT("/Meter/MeterRefreshRate"), mMeterRefreshRate);
      gPrefs->Flush();
   }

   mTimer.Start(1000 / mMeterRefreshRate);
}
