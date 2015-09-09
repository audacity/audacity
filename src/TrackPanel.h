/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackPanel.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_TRACK_PANEL__
#define __AUDACITY_TRACK_PANEL__

#include "MemoryX.h"
#include <vector>

#include <wx/timer.h>

#include "Experimental.h"

#include "SelectedRegion.h"

#include "widgets/OverlayPanel.h"

#include "SelectionState.h"

class wxMenu;
class wxRect;

class LabelTrack;
class SpectrumAnalyst;
class Track;
class TrackList;
class TrackPanel;
class TrackPanelCell;
class TrackArtist;
class Ruler;
class SnapManager;
class AdornedRulerPanel;
class LWSlider;
class ControlToolBar; //Needed because state of controls can affect what gets drawn.
class ToolsToolBar; //Needed because state of controls can affect what gets drawn.
class MixerBoard;
class AudacityProject;

class TrackPanelAx;

class ViewInfo;

class NoteTrack;
class WaveTrack;
class WaveClip;
class UIHandle;

// Declared elsewhere, to reduce compilation dependencies
class TrackPanelListener;

enum class UndoPush : unsigned char;

// JKC Nov 2011: Disabled warning C4251 which is to do with DLL linkage
// and only a worry when there are DLLs using the structures.
// Array classes are private in TrackInfo, so we will not
// access them directly from the DLL.
// TrackClipArray in TrackPanel needs to be handled with care in the derived
// class, but the C4251 warning is no worry in core Audacity.
// wxWidgets doesn't cater to the exact details we need in
// WX_DECLARE_EXPORTED_OBJARRAY to be able to use that for these two arrays.
#ifdef _MSC_VER
#pragma warning( push )
#pragma warning( disable: 4251 )
#endif

DECLARE_EXPORTED_EVENT_TYPE(AUDACITY_DLL_API, EVT_TRACK_PANEL_TIMER, -1);

enum {
   kTimerInterval = 50, // milliseconds
};


class AUDACITY_DLL_API TrackInfo
{
public:
   TrackInfo(TrackPanel * pParentIn);
   ~TrackInfo();
   void ReCreateSliders();

   struct TCPLine;

   static void DrawItems
      ( wxDC *dc, const wxRect &rect, const Track &track, int mouseCapture,
        bool captured );

   static void DrawItems
      ( wxDC *dc, const wxRect &rect, const Track *pTrack,
        const std::vector<TCPLine> &topLines,
        const std::vector<TCPLine> &bottomLines,
        int mouseCapture, bool captured );

   static void CloseTitleDrawFunction
      ( wxDC *dc, const wxRect &rect, const Track *pTrack, int pressed,
        bool captured );

   static void MinimizeSyncLockDrawFunction
      ( wxDC *dc, const wxRect &rect, const Track *pTrack, int pressed,
        bool captured );

   static void MidiControlsDrawFunction
      ( wxDC *dc, const wxRect &rect, const Track *pTrack, int pressed,
        bool captured );

   template<typename TrackClass>
   static void SliderDrawFunction
      ( LWSlider *(*Selector)
           (const wxRect &sliderRect, const TrackClass *t, bool captured,
            wxWindow*),
        wxDC *dc, const wxRect &rect, const Track *pTrack, bool captured );

   static void PanSliderDrawFunction
      ( wxDC *dc, const wxRect &rect, const Track *pTrack, int pressed,
        bool captured );

   static void GainSliderDrawFunction
      ( wxDC *dc, const wxRect &rect, const Track *pTrack, int pressed,
        bool captured );

#ifdef EXPERIMENTAL_MIDI_OUT
   static void VelocitySliderDrawFunction
      ( wxDC *dc, const wxRect &rect, const Track *pTrack, int pressed,
        bool captured );
#endif

   static void MuteOrSoloDrawFunction
      ( wxDC *dc, const wxRect &rect, const Track *pTrack, int pressed,
        bool captured, bool solo );

   static void WideMuteDrawFunction
      ( wxDC *dc, const wxRect &rect, const Track *pTrack, int pressed,
        bool captured );

   static void WideSoloDrawFunction
      ( wxDC *dc, const wxRect &rect, const Track *pTrack, int pressed,
        bool captured );

   static void MuteAndSoloDrawFunction
      ( wxDC *dc, const wxRect &rect, const Track *pTrack, int pressed,
        bool captured );

   static void StatusDrawFunction
      ( const wxString &string, wxDC *dc, const wxRect &rect );

   static void Status1DrawFunction
      ( wxDC *dc, const wxRect &rect, const Track *pTrack, int pressed,
        bool captured );

   static void Status2DrawFunction
      ( wxDC *dc, const wxRect &rect, const Track *pTrack, int pressed,
        bool captured );

public:
   int GetTrackInfoWidth() const;
   static void SetTrackInfoFont(wxDC *dc);


   void DrawBackground(wxDC * dc, const wxRect & rect, bool bSelected, bool bHasMuteSolo, const int labelw, const int vrul) const;
   void DrawBordersWithin(wxDC * dc, const wxRect & rect, const Track &track ) const;
   void DrawVRuler(wxDC * dc, const wxRect & rect, Track * t) const;

   static void GetCloseBoxHorizontalBounds( const wxRect & rect, wxRect &dest );
   static void GetCloseBoxRect(const wxRect & rect, wxRect &dest);

   static void GetTitleBarHorizontalBounds( const wxRect & rect, wxRect &dest );
   static void GetTitleBarRect(const wxRect & rect, wxRect &dest);

   static void GetNarrowMuteHorizontalBounds
      ( const wxRect & rect, wxRect &dest );
   static void GetNarrowSoloHorizontalBounds
      ( const wxRect & rect, wxRect &dest );
   static void GetWideMuteSoloHorizontalBounds
      ( const wxRect & rect, wxRect &dest );
   static void GetMuteSoloRect
      (const wxRect & rect, wxRect &dest, bool solo, bool bHasSoloButton,
       const Track *pTrack);

   static void GetSliderHorizontalBounds( const wxPoint &topleft, wxRect &dest );

   static void GetGainRect(const wxPoint & topLeft, wxRect &dest);

   static void GetPanRect(const wxPoint & topLeft, wxRect &dest);

#ifdef EXPERIMENTAL_MIDI_OUT
   static void GetVelocityRect(const wxPoint & topLeft, wxRect &dest);
#endif

   static void GetMinimizeHorizontalBounds( const wxRect &rect, wxRect &dest );
   static void GetMinimizeRect(const wxRect & rect, wxRect &dest);

   static void GetSyncLockHorizontalBounds( const wxRect &rect, wxRect &dest );
   static void GetSyncLockIconRect(const wxRect & rect, wxRect &dest);

#ifdef USE_MIDI
   static void GetMidiControlsHorizontalBounds
      ( const wxRect &rect, wxRect &dest );
   static void GetMidiControlsRect(const wxRect & rect, wxRect &dest);
#endif

   static bool HideTopItem( const wxRect &rect, const wxRect &subRect,
                               int allowance = 0 );

   static unsigned DefaultNoteTrackHeight();
   static unsigned DefaultWaveTrackHeight();

   static LWSlider * GainSlider
      (const wxRect &sliderRect, const WaveTrack *t, bool captured,
       wxWindow *pParent);
   static LWSlider * PanSlider
      (const wxRect &sliderRect, const WaveTrack *t, bool captured,
       wxWindow *pParent);

#ifdef EXPERIMENTAL_MIDI_OUT
   static LWSlider * VelocitySlider
      (const wxRect &sliderRect, const NoteTrack *t, bool captured,
       wxWindow *pParent);
#endif

private:
   void UpdatePrefs();

   TrackPanel * pParent;
   static wxFont gFont;
   // These are on separate lines to work around an MSVC 2013 compiler bug.
   static std::unique_ptr<LWSlider> gGainCaptured;
   static std::unique_ptr<LWSlider> gPanCaptured;
   static std::unique_ptr<LWSlider> gGain;
   static std::unique_ptr<LWSlider> gPan;
#ifdef EXPERIMENTAL_MIDI_OUT
   static std::unique_ptr<LWSlider> gVelocityCaptured;
   static std::unique_ptr<LWSlider> gVelocity;
#endif

   friend class TrackPanel;
};


const int DragThreshold = 3;// Anything over 3 pixels is a drag, else a click.


class AUDACITY_DLL_API TrackPanel final : public OverlayPanel {
 public:

   TrackPanel(wxWindow * parent,
              wxWindowID id,
              const wxPoint & pos,
              const wxSize & size,
              const std::shared_ptr<TrackList> &tracks,
              ViewInfo * viewInfo,
              TrackPanelListener * listener,
              AdornedRulerPanel * ruler );

   virtual ~ TrackPanel();

   virtual void UpdatePrefs();
   virtual void ApplyUpdatedTheme();

   virtual void OnPaint(wxPaintEvent & event);
   virtual void OnMouseEvent(wxMouseEvent & event);
   virtual void OnCaptureLost(wxMouseCaptureLostEvent & event);
   virtual void OnCaptureKey(wxCommandEvent & event);
   virtual void OnKeyDown(wxKeyEvent & event);
   virtual void OnChar(wxKeyEvent & event);
   virtual void OnKeyUp(wxKeyEvent & event);

   virtual void OnSetFocus(wxFocusEvent & event);
   virtual void OnKillFocus(wxFocusEvent & event);

   virtual void OnContextMenu(wxContextMenuEvent & event);

   virtual void OnTrackListResized(wxCommandEvent & event);
   virtual void OnTrackListUpdated(wxCommandEvent & event);
   virtual void UpdateViewIfNoTracks(); // Call this to update mViewInfo, etc, after track(s) removal, before Refresh().

   virtual double GetMostRecentXPos();

   virtual void OnIdle(wxIdleEvent & event);
   virtual void OnTimer(wxTimerEvent& event);

   virtual int GetLeftOffset() const { return GetLabelWidth() + 1;}

   // Width and height, relative to upper left corner at (GetLeftOffset(), 0)
   // Either argument may be NULL
   virtual void GetTracksUsableArea(int *width, int *height) const;

   virtual void Refresh(bool eraseBackground = true,
                        const wxRect *rect = (const wxRect *) NULL);
   virtual void RefreshTrack(Track *trk, bool refreshbacking = true);

   virtual void DisplaySelection();

   // These two are neither used nor defined as of Nov-2011
   //virtual void SetSelectionFormat(int iformat)
   //virtual void SetSnapTo(int snapto)

   virtual void HandleInterruptedDrag();
   virtual void CancelDragging();
   virtual bool HandleEscapeKey(bool down);
   virtual void HandleAltKey(bool down);
   virtual void HandleShiftKey(bool down);
   virtual void HandleControlKey(bool down);
   virtual void HandlePageUpKey();
   virtual void HandlePageDownKey();
   virtual AudacityProject * GetProject() const;

   virtual void ScrollIntoView(double pos);
   virtual void ScrollIntoView(int x);

   virtual void OnTrackMenu(Track *t = NULL);
   virtual Track * GetFirstSelectedTrack();
   virtual bool IsMouseCaptured();

   virtual void EnsureVisible(Track * t);

   virtual Track *GetFocusedTrack();
   virtual void SetFocusedTrack(Track *t);

   virtual void HandleCursorForLastMouseEvent();

   virtual void UpdateVRulers();
   virtual void UpdateVRuler(Track *t);
   virtual void UpdateTrackVRuler(const Track *t);
   virtual void UpdateVRulerSize();

   // Returns the time corresponding to the pixel column one past the track area
   // (ignoring any fisheye)
   virtual double GetScreenEndTime() const;

 protected:
   virtual MixerBoard* GetMixerBoard();

   virtual bool IsAudioActive();
   virtual void HandleTrackSpecificMouseEvent(wxMouseEvent & event);

public:
   size_t GetTrackCount() const;
   size_t GetSelectedTrackCount() const;

protected:
   virtual void UpdateSelectionDisplay();

public:
   virtual void UpdateAccessibility();
   void MessageForScreenReader(const wxString& message);

   virtual void HandleCursor(wxMouseEvent & event);

   // MM: Handle mouse wheel rotation
   virtual void HandleWheelRotation(wxMouseEvent & event);

   virtual void MakeParentRedrawScrollbars();

protected:
   virtual void MakeParentModifyState(bool bWantsAutoSave);    // if true, writes auto-save file. Should set only if you really want the state change restored after
                                                               // a crash, as it can take many seconds for large (eg. 10 track-hours) projects

   // Find track info by coordinate
   enum class CellType { Label, Track, VRuler, Background };
   struct FoundCell {
      Track *pTrack;
      TrackPanelCell *pCell;
      CellType type;
      wxRect rect;
   };
   virtual FoundCell FindCell(int mouseX, int mouseY);

   // If label, rectangle includes track control panel only.
   // If !label, rectangle includes all of that, and the vertical ruler, and
   // the proper track area.
   virtual wxRect FindTrackRect( const Track * target, bool label );

   virtual int GetVRulerWidth() const;
   virtual int GetVRulerOffset() const { return mTrackInfo.GetTrackInfoWidth(); }

   virtual int GetLabelWidth() const { return mTrackInfo.GetTrackInfoWidth() + GetVRulerWidth(); }

// JKC Nov-2011: These four functions only used from within a dll such as mod-track-panel
// They work around some messy problems with constructors.
public:
   const TrackList * GetTracks() const { return mTracks.get(); }
   TrackList * GetTracks() { return mTracks.get(); }
   ViewInfo * GetViewInfo(){ return mViewInfo;}
   TrackPanelListener * GetListener(){ return mListener;}
   AdornedRulerPanel * GetRuler(){ return mRuler;}
// JKC and here is a factory function which just does 'NEW' in standard Audacity.
   // Precondition: parent != NULL
   static TrackPanel *(*FactoryFunction)(wxWindow * parent,
              wxWindowID id,
              const wxPoint & pos,
              const wxSize & size,
              const std::shared_ptr<TrackList> &tracks,
              ViewInfo * viewInfo,
              TrackPanelListener * listener,
              AdornedRulerPanel * ruler);

protected:
   virtual void DrawTracks(wxDC * dc);

   virtual void DrawEverythingElse(wxDC *dc, const wxRegion & region,
                           const wxRect & clip);
   virtual void DrawOutside(Track *t, wxDC *dc, const wxRect & rec);

   virtual void HighlightFocusedTrack (wxDC* dc, const wxRect &rect);
   virtual void DrawShadow            (Track *t, wxDC* dc, const wxRect & rect);
   virtual void DrawBordersAroundTrack(Track *t, wxDC* dc, const wxRect & rect, const int labelw, const int vrul);
   virtual void DrawOutsideOfTrack    (Track *t, wxDC* dc, const wxRect & rect);

public:
   // Set the object that performs catch-all event handling when the pointer
   // is not in any track or ruler or control panel.
   virtual void SetBackgroundCell
      (const std::shared_ptr< TrackPanelCell > &pCell);

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   void UpdateVirtualStereoOrder();
#endif

public:
   // Accessors...
   static bool HasSoloButton(){  return gSoloPref!=wxT("None");}

protected:

   TrackInfo mTrackInfo;

public:

   LWSlider *GainSlider( const WaveTrack *wt );
   LWSlider *PanSlider( const WaveTrack *wt );
#ifdef EXPERIMENTAL_MIDI_OUT
   LWSlider *VelocitySlider( const NoteTrack *nt );
#endif

   TrackInfo *GetTrackInfo() { return &mTrackInfo; }
   const TrackInfo *GetTrackInfo() const { return &mTrackInfo; }

protected:
   TrackPanelListener *mListener;

   std::shared_ptr<TrackList> mTracks;
   ViewInfo *mViewInfo;

   AdornedRulerPanel *mRuler;

   std::unique_ptr<TrackArtist> mTrackArtist;

   class AUDACITY_DLL_API AudacityTimer final : public wxTimer {
   public:
     void Notify() override{
       // (From Debian)
       //
       // Don't call parent->OnTimer(..) directly here, but instead post
       // an event. This ensures that this is a pure wxWidgets event
       // (no GDK event behind it) and that it therefore isn't processed
       // within the YieldFor(..) of the clipboard operations (workaround
       // for Debian bug #765341).
       // QueueEvent() will take ownership of the event
       parent->GetEventHandler()->QueueEvent(safenew wxTimerEvent(*this));
     }
     TrackPanel *parent;
   } mTimer;

   int mTimeCount;

   bool mRefreshBacking;

#ifdef EXPERIMENTAL_SPECTRAL_EDITING

protected:

#endif

   bool mRedrawAfterStop;

   wxMouseEvent mLastMouseEvent;

   int mMouseMostRecentX;
   int mMouseMostRecentY;

public:
   // Old enumeration of click-and-drag states, which will shrink and disappear
   // as UIHandle subclasses take over the repsonsibilities.
   enum   MouseCaptureEnum
   {
      IsUncaptured = 0,
      IsClosing,
      IsMuting,
      IsSoloing,
      IsMinimizing,
      IsPopping,
   };

protected:
   friend class TrackPanelAx;

#if wxUSE_ACCESSIBILITY
   TrackPanelAx *mAx{};
#else
   std::unique_ptr<TrackPanelAx> mAx;
#endif

public:
   TrackPanelAx &GetAx() { return *mAx; }

protected:

   static wxString gSoloPref;

   // The screenshot class needs to access internals
   friend class ScreenshotCommand;

   SelectedRegion mLastDrawnSelectedRegion {};

 public:
   wxSize vrulerSize;

 protected:
   Track *mpClickedTrack {};
   // TrackPanel is not responsible for memory management:
   UIHandle *mUIHandle {};

   std::shared_ptr<TrackPanelCell> mpBackground;

   DECLARE_EVENT_TABLE()
};

// See big pictorial comment in TrackPanel for explanation of these numbers
enum : int {
   kLeftInset = 4,
   kRightInset = kLeftInset,
   kTopInset = 4,
   kShadowThickness = 1,
   kBorderThickness = 1,
   kTopMargin = kTopInset + kBorderThickness,
   kBottomMargin = kShadowThickness + kBorderThickness,
   kLeftMargin = kLeftInset + kBorderThickness,
   kRightMargin = kRightInset + kShadowThickness + kBorderThickness,
};

enum : int {
   kTrackInfoWidth = 100,
   kTrackInfoBtnSize = 18, // widely used dimension, usually height
   kTrackInfoSliderHeight = 25,
   kTrackInfoSliderWidth = 84,
   kTrackInfoSliderAllowance = 5,
   kTrackInfoSliderExtra = 5,
};

#ifdef USE_MIDI
enum : int {
   kMidiCellWidth = (kTrackInfoWidth / 4) - 2,
   kMidiCellHeight = kTrackInfoBtnSize
};
#endif

#ifdef _MSC_VER
#pragma warning( pop )
#endif


//This constant determines the size of the vertical region (in pixels) around
//the bottom of a track that can be used for vertical track resizing.
#define TRACK_RESIZE_REGION 5

#endif
