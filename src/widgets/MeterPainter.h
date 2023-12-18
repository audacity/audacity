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

#include <atomic>
#include <wx/setup.h> // for wxUSE_* macros
#include <wx/brush.h> // member variable
#include <wx/defs.h>
#include <wx/timer.h> // member variable

#include "PeakAndRmsMeter.h"
#include "ASlider.h"
#include "SampleFormat.h"
#include "Prefs.h"
#include "MeterPanelBase.h" // to inherit
#include "Observer.h"
#include "Ruler.h" // member variable

class AudacityProject;
struct AudioIOEvent;

struct MeterBar {
   // How many pixels between items?
   static constexpr int gap = 2;

   //! Given the bounding rectangle, subdivide it
   void SetRectangles(wxRect bounding, bool vertical, bool clip);

   bool   vert{};
   wxRect bevel{}; // Bevel around bar
   wxRect rect{}; // True bar drawing area
   wxRect rClip{}; // Rectangle for clipping, nonoverlapping with bevel
};

class MeterPainter {
public:
   using Stats = PeakAndRmsMeter::Stats;

   MeterPainter(bool clip, bool gradient, bool input,
      int bgColor //!< Theme color code
   );
   void SetBackgroundColor(int bgColor);

   //! Destroy any existing bitmap first; make new one filled with bg color
   /*!
    @post `GetBitmap() != nullptr`
    */
   void AllocateBitmap(wxDC &dc, int width, int height);

   //! Color the bitmap as for maximum levels
   void FillBitmap(const MeterBar &bar, bool dB, int dBRange);

   //! Blit parts of the stored bitmap to dc and fill the rest as background,
   //! according to levels in stats
   void DrawMeterBar(wxDC &dc, bool disabled,
      const MeterBar &meterBar, Stats &stats) const;

   bool GetGradient() const { return mGradient; }
   void SetGradient(bool gradient) { mGradient = gradient; }

   bool GetClip() const { return mClip; }
   void SetClip(bool clip) { mClip = clip; }

   wxBitmap *GetBitmap() { return mBitmap.get(); }

private:
   wxPen     mPen;
   wxPen     mPeakPeakPen;
   wxBrush   mBrush;
   wxBrush   mRMSBrush;
   wxBrush   mClipBrush;
   wxBrush   mDisabledBkgndBrush;
   wxBrush   mBkgndBrush;
   std::unique_ptr<wxBitmap> mBitmap;
   bool mClip;
   bool mGradient;
};

class MeterAx;

/********************************************************************//**
\brief MeterPanel is a panel that paints the meter used for monitoring
or playback.
************************************************************************/
class AUDACITY_DLL_API MeterPanel final
   : public MeterPanelBase
   , public PeakAndRmsMeter
   , private PrefsListener
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
         Style style = HorizontalStereo);

   void SetFocusFromKbd() override;

   Style GetStyle() const { return mStyle; }
   Style GetDesiredStyle() const { return mDesiredStyle; }
   void SetStyle(Style newStyle);

   /** \brief
    *
    * This method is thread-safe!  Feel free to call from a
    * different thread (like from an audio I/O callback).
    */
   void Reset(double sampleRate, bool resetClipping) override;

   void Receive(double time, const MeterUpdateMsg &msg) override;

   // Vaughan, 2010-11-29: This not currently used. See comments in MixerTrackCluster::UpdateMeter().
   //void UpdateDisplay(int numChannels, int numFrames,
   //                     // Need to make these double-indexed max and min arrays if we handle more than 2 channels.
   //                     float* maxLeft, float* rmsLeft,
   //                     float* maxRight, float* rmsRight,
   //                     const size_t kSampleCount);

   float GetPeakHold() const;

   bool IsMonitoring() const;
   bool IsActive() const;

   void StartMonitoring();
   void StopMonitoring();

   // These exist solely for the purpose of resetting the toolbars
   struct State{ bool mSaved, mMonitoring, mActive; };
   State SaveState();
   void RestoreState(const State &state);
   void SetMixer(wxCommandEvent& event);

   bool ShowDialog();
   void Increase(float steps);
   void Decrease(float steps);
   void UpdateSliderControl();
   
   void ShowMenu(const wxPoint & pos);

   void SetName(const TranslatableString& name);

 private:
   static constexpr int gap = MeterBar::gap;

   void UpdatePrefs() override;
   void UpdateSelectedPrefs( int ) override;

   //
   // Event handlers
   //
   void OnErase(wxEraseEvent &evt);
   void OnPaint(wxPaintEvent &evt);
   void OnSize(wxSizeEvent &evt);
   void OnMouse(wxMouseEvent &evt);
   void OnKeyDown(wxKeyEvent &evt);
   void OnCharHook(wxKeyEvent &evt);
   void OnContext(wxContextMenuEvent &evt);
   void OnSetFocus(wxFocusEvent &evt);
   void OnKillFocus(wxFocusEvent &evt);

   void OnAudioIOStatus(const AudioIOEvent &event);
   void OnAudioCapture(const AudioIOEvent &event);

   void OnMeterUpdate(wxTimerEvent &evt);
   void OnTipTimeout(wxTimerEvent& evt);

   void HandleLayout();
   void SetActiveStyle(Style style);
   void RepaintBarsNow();
   wxFont GetFont() const;

   //
   // Pop-up menu
   //
   void OnMonitor(wxCommandEvent &evt);
   void OnPreferences(wxCommandEvent &evt);

   wxString Key(const wxString & key) const;

   Observer::Subscription mAudioIOStatusSubscription;
   Observer::Subscription mAudioCaptureSubscription;

   AudacityProject *mProject;
   wxTimer          mTimer;
   wxTimer          mTipTimer;

   int       mWidth;
   int       mHeight;

   int       mRulerWidth{};
   int       mRulerHeight{};

   bool      mIsInput;

   Style     mStyle{};
   Style     mDesiredStyle;
   int       mNumPeakSamplesToClip;
   double    mPeakHoldDuration;
   double    mRate;
   long      mMeterRefreshRate{};

   bool      mMonitoring;

   bool      mActive;

   MeterBar  mBar[kMaxMeterBars]{};

   MeterPainter mPainter;

   bool      mLayoutValid;

   wxPoint   mLeftTextPos;
   wxPoint   mRightTextPos;
   wxSize    mLeftSize;
   wxSize    mRightSize;
   Ruler     mRuler;
   wxString  mLeftText;
   wxString  mRightText;

   std::unique_ptr<LWSlider> mSlider;
   wxPoint mSliderPos;
   wxSize mSliderSize;

   bool mEnabled{ true };

   bool mIsFocused{};
   wxRect mFocusRect;

   /*! @name state variables during OnMeterUpdate
     @{
    */
   double mMaxPeak{};
   unsigned mNumChanges{};
   bool mDiscarded{};
   /*!
     @}
    */

   friend class MeterAx;

   DECLARE_EVENT_TABLE()
};

#endif // __AUDACITY_METER_PANEL__
