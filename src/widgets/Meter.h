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
#include <wx/timer.h>

#include "../SampleFormat.h"
#include "Ruler.h"

// Event used to notify all meters of preference changes
DECLARE_EXPORTED_EVENT_TYPE(AUDACITY_DLL_API, EVT_METER_PREFERENCES_CHANGED, -1);

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

class MeterAx;

class Meter final : public wxPanelWrapper
{
   DECLARE_DYNAMIC_CLASS(Meter)

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


   Meter(AudacityProject *,
         wxWindow* parent, wxWindowID id,
         bool isInput,
         const wxPoint& pos = wxDefaultPosition,
         const wxSize& size = wxDefaultSize,
         Style style = HorizontalStereo,
         float fDecayRate = 60.0f);

   ~Meter();

   bool AcceptsFocus() const override { return s_AcceptsFocus; }
   bool AcceptsFocusFromKeyboard() const override { return true; }

   void SetFocusFromKbd() override;

   void UpdatePrefs();
   void Clear();

   Style GetStyle() const { return mStyle; }
   Style GetDesiredStyle() const { return mDesiredStyle; }
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
   void UpdateDisplay(unsigned numChannels,
                      int numFrames, float *sampleData);

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
   bool IsMeterDisabled() const;

   float GetMaxPeak() const;

   bool IsClipping() const;

   void StartMonitoring();

   // These exist solely for the purpose of reseting the toolbars
   void *SaveState();
   void RestoreState(void *state);

 private:
   static bool s_AcceptsFocus;
   struct Resetter { void operator () (bool *p) const { if(p) *p = false; } };
   using TempAllowFocus = std::unique_ptr<bool, Resetter>;

 public:
   static TempAllowFocus TemporarilyAllowFocus();

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
   void OnMeterPrefsUpdated(wxCommandEvent &evt);

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

#if wxUSE_ACCESSIBILITY

class MeterAx final : public wxWindowAccessible
{
public:
   MeterAx(wxWindow * window);

   virtual ~ MeterAx();

   // Performs the default action. childId is 0 (the action for this object)
   // or > 0 (the action for a child).
   // Return wxACC_NOT_SUPPORTED if there is no default action for this
   // window (e.g. an edit control).
   wxAccStatus DoDefaultAction(int childId) override;

   // Retrieves the address of an IDispatch interface for the specified child.
   // All objects must support this property.
   wxAccStatus GetChild(int childId, wxAccessible** child) override;

   // Gets the number of children.
   wxAccStatus GetChildCount(int* childCount) override;

   // Gets the default action for this object (0) or > 0 (the action for a child).
   // Return wxACC_OK even if there is no action. actionName is the action, or the empty
   // string if there is no action.
   // The retrieved string describes the action that is performed on an object,
   // not what the object does as a result. For example, a toolbar button that prints
   // a document has a default action of "Press" rather than "Prints the current document."
   wxAccStatus GetDefaultAction(int childId, wxString *actionName) override;

   // Returns the description for this object or a child.
   wxAccStatus GetDescription(int childId, wxString *description) override;

   // Gets the window with the keyboard focus.
   // If childId is 0 and child is NULL, no object in
   // this subhierarchy has the focus.
   // If this object has the focus, child should be 'this'.
   wxAccStatus GetFocus(int *childId, wxAccessible **child) override;

   // Returns help text for this object or a child, similar to tooltip text.
   wxAccStatus GetHelpText(int childId, wxString *helpText) override;

   // Returns the keyboard shortcut for this object or child.
   // Return e.g. ALT+K
   wxAccStatus GetKeyboardShortcut(int childId, wxString *shortcut) override;

   // Returns the rectangle for this object (id = 0) or a child element (id > 0).
   // rect is in screen coordinates.
   wxAccStatus GetLocation(wxRect& rect, int elementId) override;

   // Gets the name of the specified object.
   wxAccStatus GetName(int childId, wxString *name) override;

   // Returns a role constant.
   wxAccStatus GetRole(int childId, wxAccRole *role) override;

   // Gets a variant representing the selected children
   // of this object.
   // Acceptable values:
   // - a null variant (IsNull() returns TRUE)
   // - a list variant (GetType() == wxT("list"))
   // - an integer representing the selected child element,
   //   or 0 if this object is selected (GetType() == wxT("long"))
   // - a "void*" pointer to a wxAccessible child object
   wxAccStatus GetSelections(wxVariant *selections) override;

   // Returns a state constant.
   wxAccStatus GetState(int childId, long* state) override;

   // Returns a localized string representing the value for the object
   // or child.
   wxAccStatus GetValue(int childId, wxString* strValue) override;

};

#endif // wxUSE_ACCESSIBILITY

#endif // __AUDACITY_METER__
