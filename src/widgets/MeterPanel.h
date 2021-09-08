/**********************************************************************

  Audacity: A Digital Audio Editor

  MeterPanel.h

  Dominic Mazzoni

  VU Meter, for displaying recording/playback level

  This is a bunch of common code that can display many different
  forms of VU meters and other displays.

**********************************************************************/

#ifndef __AUDACITY_METER_PANEL__
#define __AUDACITY_METER_PANEL__

#include <wx/setup.h> // for wxUSE_* macros
#include <wx/brush.h> // member variable
#include <wx/defs.h>
#include <wx/timer.h> // member variable

#include "SampleFormat.h"
#include "Prefs.h"
#include "MeterPanelBase.h" // to inherit
#include "Ruler.h" // member variable

class AudacityProject;

// Increase this when we add support for multichannel meters
// (most of the code is already there)
const int kMaxMeterBars = 2;

struct MeterBar {
   bool   vert;
   wxRect b;         // Bevel around bar
   wxRect r;         // True bar drawing area
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
   MeterUpdateMsg() { }
   ~MeterUpdateMsg() { }
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
   explicit MeterUpdateQueue(size_t maxLen);
   ~MeterUpdateQueue();

   bool Put(MeterUpdateMsg &msg);
   bool Get(MeterUpdateMsg &msg);

   void Clear();

 private:
   int              mStart;
   int              mEnd;
   size_t           mBufferSize;
   ArrayOf<MeterUpdateMsg> mBuffer{mBufferSize};
};

class MeterAx;

/********************************************************************//**
\brief MeterPanel is a panel that paints the meter used for monitoring
or playback.
************************************************************************/
class AUDACITY_DLL_API MeterPanel final
   : public MeterPanelBase, private PrefsListener
{
   DECLARE_DYNAMIC_CLASS(MeterPanel)

 public:
   // These should be kept in the same order as they appear
   // in the menu
   enum Style {
      AutomaticStereo,
      HorizontalStereo,
      VerticalStereo,
      MixerTrackCluster, // Doesn't show menu, icon, or L/R labels, but otherwise like VerticalStereo.
      HorizontalStereoCompact, // Thinner.
      VerticalStereoCompact, // Narrower.
   };


   MeterPanel(AudacityProject *,
         wxWindow* parent, wxWindowID id,
         bool isInput,
         const wxPoint& pos = wxDefaultPosition,
         const wxSize& size = wxDefaultSize,
         Style style = HorizontalStereo,
         float fDecayRate = 60.0f);

   void SetFocusFromKbd() override;

   void Clear() override;

   Style GetStyle() const { return mStyle; }
   Style GetDesiredStyle() const { return mDesiredStyle; }
   void SetStyle(Style newStyle);

   /** \brief
    *
    * This method is thread-safe!  Feel free to call from a
    * different thread (like from an audio I/O callback).
    */
   void Reset(double sampleRate, bool resetClipping) override;

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
   void UpdateDisplay(unsigned numChannels,
                      int numFrames, const float *sampleData) override;

   // Vaughan, 2010-11-29: This not currently used. See comments in MixerTrackCluster::UpdateMeter().
   //void UpdateDisplay(int numChannels, int numFrames,
   //                     // Need to make these double-indexed max and min arrays if we handle more than 2 channels.
   //                     float* maxLeft, float* rmsLeft,
   //                     float* maxRight, float* rmsRight,
   //                     const size_t kSampleCount);

   /** \brief Find out if the level meter is disabled or not.
    *
    * This method is thread-safe!  Feel free to call from a
    * different thread (like from an audio I/O callback).
    */
   bool IsMeterDisabled() const override;

   float GetMaxPeak() const override;

   bool IsClipping() const override;

   void StartMonitoring();
   void StopMonitoring();

   // These exist solely for the purpose of resetting the toolbars
   struct State{ bool mSaved, mMonitoring, mActive; };
   State SaveState();
   void RestoreState(const State &state);

   int GetDBRange() const override { return mDB ? mDBRange : -1; }

 private:
   void UpdatePrefs() override;
   void UpdateSelectedPrefs( int ) override;

 private:
   //
   // Event handlers
   //
   void OnErase(wxEraseEvent &evt);
   void OnPaint(wxPaintEvent &evt);
   void OnSize(wxSizeEvent &evt);
   bool InIcon(wxMouseEvent *pEvent = nullptr) const;
   void OnMouse(wxMouseEvent &evt);
   void OnKeyDown(wxKeyEvent &evt);
   void OnKeyUp(wxKeyEvent &evt);
   void OnContext(wxContextMenuEvent &evt);
   void OnSetFocus(wxFocusEvent &evt);
   void OnKillFocus(wxFocusEvent &evt);

   void OnAudioIOStatus(wxCommandEvent &evt);

   void OnMeterUpdate(wxTimerEvent &evt);

   void HandleLayout(wxDC &dc);
   void SetActiveStyle(Style style);
   void SetBarAndClip(int iBar, bool vert);
   void DrawMeterBar(wxDC &dc, MeterBar *meterBar);
   void ResetBar(MeterBar *bar, bool resetClipping);
   void RepaintBarsNow();
   wxFont GetFont() const;

   //
   // Pop-up menu
   //
   void ShowMenu(const wxPoint & pos);
   void OnMonitor(wxCommandEvent &evt);
   void OnPreferences(wxCommandEvent &evt);

   wxString Key(const wxString & key) const;

   AudacityProject *mProject;
   MeterUpdateQueue mQueue;
   wxTimer          mTimer;

   int       mWidth;
   int       mHeight;

   int       mRulerWidth;
   int       mRulerHeight;

   bool      mIsInput;

   Style     mStyle;
   Style     mDesiredStyle;
   bool      mGradient;
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

   bool      mMonitoring;

   bool      mActive;

   unsigned  mNumBars;
   MeterBar  mBar[kMaxMeterBars];

   bool      mLayoutValid;

   std::unique_ptr<wxBitmap> mBitmap;
   wxRect    mIconRect;
   wxPoint   mLeftTextPos;
   wxPoint   mRightTextPos;
   wxSize    mLeftSize;
   wxSize    mRightSize;
   std::unique_ptr<wxBitmap> mIcon;
   wxPen     mPen;
   wxPen     mDisabledPen;
   wxPen     mPeakPeakPen;
   wxBrush   mBrush;
   wxBrush   mRMSBrush;
   wxBrush   mClipBrush;
   wxBrush   mBkgndBrush;
   wxBrush   mDisabledBkgndBrush;
   Ruler     mRuler;
   wxString  mLeftText;
   wxString  mRightText;

   bool mIsFocused;
   wxRect mFocusRect;
#if defined(__WXMSW__)
   bool mHadKeyDown;
#endif

   bool mAccSilent;

   friend class MeterAx;

   bool mHighlighted {};

   DECLARE_EVENT_TABLE()
};

#endif // __AUDACITY_METER_PANEL__
