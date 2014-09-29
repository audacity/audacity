   /**********************************************************************

  Audacity: A Digital Audio Editor

  Meter.h

  Dominic Mazzoni

  VU Meter, for displaying recording/playback level

  This is a bunch of common code that can display many different
  forms of VU meters and other displays.

**********************************************************************/

#ifndef __AUDACITY_METER__
#define __AUDACITY_METER__

#include <wx/defs.h>
#include <wx/panel.h>
#include <wx/timer.h>

#include "../SampleFormat.h"
#include "../Sequence.h"
#include "Ruler.h"

// Increase this when we add support for multichannel meters
// (most of the code is already there)
const int kMaxMeterBars = 2;

struct MeterBar {
   bool   vert;
   wxRect r;
   float  peak;
   float  rms;
   float  peakHold;
   double peakHoldTime;
   wxRect rClip;
   bool   clipping;
   bool   isclipping; //ANSWER-ME: What's the diff between these bools?! "clipping" vs "isclipping" is not clear.
   int    tailPeakCount;
   float  peakPeakHold;
};

class MeterUpdateMsg
{
   public:
   int numFrames;
   float peak[kMaxMeterBars];
   float rms[kMaxMeterBars];
   bool clipping[kMaxMeterBars];
   int headPeakCount[kMaxMeterBars];
   int tailPeakCount[kMaxMeterBars];

   /* neither constructor nor destructor do anything */
   MeterUpdateMsg() { };
   ~MeterUpdateMsg() { };
   /* for debugging purposes, printing the values out is really handy */
   /** \brief Print out all the values in the meter update message */
   wxString toString();
   /** \brief Only print meter updates if clipping may be happening */
   wxString toStringIfClipped();
};

// Thread-safe queue of update messages
class MeterUpdateQueue
{
 public:
   MeterUpdateQueue(int maxLen);
   ~MeterUpdateQueue();

   bool Put(MeterUpdateMsg &msg);
   bool Get(MeterUpdateMsg &msg);

   void Clear();

 private:
   int              mStart;
   int              mEnd;
   int              mBufferSize;
   MeterUpdateMsg  *mBuffer;
};

class Meter : public wxPanel
{
   DECLARE_DYNAMIC_CLASS(Meter)

 public:
   // These should be kept in the same order as they appear
   // in the menu
   enum Style {
      HorizontalStereo,
      VerticalStereo,
      VerticalMulti,
      Equalizer,
      Waveform,
      MixerTrackCluster // Doesn't show menu, icon, or L/R labels, but otherwise like VerticalStereo.
   };


   Meter(wxWindow* parent, wxWindowID id,
         bool isInput,
         const wxPoint& pos = wxDefaultPosition,
         const wxSize& size = wxDefaultSize,
         Style style = HorizontalStereo,
         float fDecayRate = 60.0f);

   ~Meter();

   void UpdatePrefs();
   void Clear();

   Style GetStyle() { return mStyle; }
   void SetStyle(Style newStyle);

   /** \brief
    *
    * This method is thread-safe!  Feel free to call from a
    * different thread (like from an audio I/O callback).
    */
   void Reset(double sampleRate, bool resetClipping);

   /** \brief Update the meters with a block of audio data
    *
    * Process the supplied block of audio data, extracting the peak and RMS
    * levels to send to the meter. Also record runs of clipped samples to detect
    * clipping that lies on block boundaries.
    * This method is thread-safe!  Feel free to call from a different thread
    * (like from an audio I/O callback).
    *
    * First overload:
    * \param numChannels The number of channels of audio being played back or
    * recorded.
    * \param numFrames The number of frames (samples) in this data block. It is
    * assumed that there are the same number of frames in each channel.
    * \param sampleData The audio data itself, as interleaved samples. So
    * indexing through the array we get the first sample of channel, first
    * sample of channel 2 etc up to the first sample of channel (numChannels),
    * then the second sample of channel 1, second sample of channel 2, and so
    * to the second sample of channel (numChannels). The last sample in the
    * array will be the (numFrames) sample for channel (numChannels).
    *
    * The second overload is for ease of use in MixerBoard.
    */
   void UpdateDisplay(int numChannels,
                      int numFrames, float *sampleData);
   // Vaughan, 2010-11-29: This not currently used. See comments in MixerTrackCluster::UpdateMeter().
   //void UpdateDisplay(int numChannels, int numFrames,
   //                     // Need to make these double-indexed max and min arrays if we handle more than 2 channels.
   //                     float* maxLeft, float* rmsLeft,
   //                     float* maxRight, float* rmsRight,
   //                     const sampleCount kSampleCount);

   /** \brief Find out if the level meter is disabled or not.
    *
    * This method is thread-safe!  Feel free to call from a
    * different thread (like from an audio I/O callback).
    */
   bool IsMeterDisabled();

   float GetMaxPeak();

   double ToLinearIfDB(double value);

   //
   // Event handlers
   //

   void OnErase(wxEraseEvent &evt);
   void OnPaint(wxPaintEvent &evt);
   void OnSize(wxSizeEvent &evt);
   void OnMouse(wxMouseEvent &evt);

   void OnMeterUpdate(wxTimerEvent &evt);

   void HandlePaint(wxDC &dc);
   void HandleLayout();

   //
   // Pop-up menu handlers
   //

   void OnDisableMeter(wxCommandEvent &evt);
   void OnHorizontal(wxCommandEvent &evt);
   void OnVertical(wxCommandEvent &evt);
   void OnMulti(wxCommandEvent &evt);
   void OnEqualizer(wxCommandEvent &evt);
   void OnWaveform(wxCommandEvent &evt);
   void OnLinear(wxCommandEvent &evt);
   void OnDB(wxCommandEvent &evt);
   void OnClip(wxCommandEvent &evt);
   void OnMonitor(wxCommandEvent &evt);
#ifdef AUTOMATED_INPUT_LEVEL_ADJUSTMENT
   void OnAutomatedInputLevelAdjustment(wxCommandEvent &evt);
#endif
   void OnFloat(wxCommandEvent &evt);
   void OnPreferences(wxCommandEvent &evt);
   bool IsClipping();

   void StartMonitoring();

 private:
   void DrawMeterBar(wxDC &dc, MeterBar *meterBar);
   void ResetBar(MeterBar *bar, bool resetClipping);
   void RepaintBarsNow();
   void CreateIcon(int aquaOffset);
   wxFont GetFont();

   MeterUpdateQueue mQueue;
   wxTimer          mTimer;

   int       mWidth;
   int       mHeight;

   bool      mIsInput;

   Style     mStyle, mSavedStyle;
   bool      mDB;
   int       mDBRange;
   bool      mDecay;
   float     mDecayRate; // dB/sec
   bool      mClip;
   int       mNumPeakSamplesToClip;
   double    mPeakHoldDuration;
   double    mT;
   double    mRate;
   long      mMeterRefreshRate;
   long      mMeterDisabled; //is used as a bool, needs long for easy gPrefs...

   int       mNumBars;
   MeterBar  mBar[kMaxMeterBars];

   bool      mLayoutValid;

   wxBitmap *mBitmap;
   wxRect    mMenuRect;
   wxPoint   mIconPos;
   wxPoint   mLeftTextPos;
   wxPoint   mRightTextPos;
   wxSize    mLeftSize;
   wxSize    mRightSize;
   wxBitmap *mIcon;
   wxPen     mPen;
   wxPen     mLightPen;
   wxPen     mSavedLightPen;
   wxPen     mDarkPen;
   wxPen     mSavedDarkPen;
   wxPen     mDisabledPen;
   wxPen     mPeakPeakPen;
   wxBrush   mBrush;
   wxBrush   mRMSBrush;
   wxBrush   mClipBrush;
   wxBrush   mBkgndBrush;
   wxBrush   mSavedBkgndBrush;
   wxBrush   mSavedBrush;
   wxBrush   mSavedRMSBrush;
   wxBrush   mDisabledBkgndBrush;
   wxRect    mAllBarsRect;
   Ruler     mRuler;

   DECLARE_EVENT_TABLE()
};

#endif // __AUDACITY_METER__
