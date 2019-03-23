/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackPanel.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_TRACK_PANEL__
#define __AUDACITY_TRACK_PANEL__

#include "Audacity.h" // for USE_* macros
#include "Experimental.h"

#include "MemoryX.h"
#include <vector>

#include <wx/setup.h> // for wxUSE_* macros
#include <wx/timer.h>

#include "HitTestResult.h"

#include "SelectedRegion.h"

#include "CellularPanel.h"

#include "commands/CommandManagerWindowClasses.h"


class wxMenu;
class wxRect;

class LabelTrack;
class SpectrumAnalyst;
class Track;
class TrackList;
struct TrackListEvent;
class TrackPanel;
class TrackArtist;
class Ruler;
class SnapManager;
class AdornedRulerPanel;
class LWSlider;
class ControlToolBar; //Needed because state of controls can affect what gets drawn.
class ToolsToolBar; //Needed because state of controls can affect what gets drawn.

class TrackPanelAx;

class NoteTrack;
class WaveTrack;
class WaveClip;

// Declared elsewhere, to reduce compilation dependencies
class TrackPanelListener;

struct TrackPanelDrawingContext;

enum class UndoPush : unsigned char;

wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API,
                         EVT_TRACK_PANEL_TIMER, wxCommandEvent);

enum {
   kTimerInterval = 50, // milliseconds
};


namespace TrackInfo
{
   void ReCreateSliders( wxWindow *pParent );

   unsigned MinimumTrackHeight();

   struct TCPLine;

   void DrawItems
      ( TrackPanelDrawingContext &context,
        const wxRect &rect, const Track &track );

   void DrawItems
      ( TrackPanelDrawingContext &context,
        const wxRect &rect, const Track *pTrack,
        const std::vector<TCPLine> &topLines,
        const std::vector<TCPLine> &bottomLines );

   void CloseTitleDrawFunction
      ( TrackPanelDrawingContext &context,
        const wxRect &rect, const Track *pTrack );

   void MinimizeSyncLockDrawFunction
      ( TrackPanelDrawingContext &context,
        const wxRect &rect, const Track *pTrack );

   void MidiControlsDrawFunction
      ( TrackPanelDrawingContext &context,
        const wxRect &rect, const Track *pTrack );

   template<typename TrackClass>
   void SliderDrawFunction
      ( LWSlider *(*Selector)
           (const wxRect &sliderRect, const TrackClass *t, bool captured,
            wxWindow*),
        wxDC *dc, const wxRect &rect, const Track *pTrack,
        bool captured, bool highlight );

   void PanSliderDrawFunction
      ( TrackPanelDrawingContext &context,
        const wxRect &rect, const Track *pTrack );

   void GainSliderDrawFunction
      ( TrackPanelDrawingContext &context,
        const wxRect &rect, const Track *pTrack );

#ifdef EXPERIMENTAL_MIDI_OUT
   void VelocitySliderDrawFunction
      ( TrackPanelDrawingContext &context,
        const wxRect &rect, const Track *pTrack );
#endif

   void MuteOrSoloDrawFunction
      ( wxDC *dc, const wxRect &rect, const Track *pTrack, bool down,
        bool captured, bool solo, bool hit );

   void WideMuteDrawFunction
      ( TrackPanelDrawingContext &context,
        const wxRect &rect, const Track *pTrack );

   void WideSoloDrawFunction
      ( TrackPanelDrawingContext &context,
        const wxRect &rect, const Track *pTrack );

   void MuteAndSoloDrawFunction
      ( TrackPanelDrawingContext &context,
        const wxRect &rect, const Track *pTrack );

   void StatusDrawFunction
      ( const wxString &string, wxDC *dc, const wxRect &rect );

   void Status1DrawFunction
      ( TrackPanelDrawingContext &context,
        const wxRect &rect, const Track *pTrack );

   void Status2DrawFunction
      ( TrackPanelDrawingContext &context,
        const wxRect &rect, const Track *pTrack );

   void SetTrackInfoFont(wxDC *dc);


   void DrawBackground(
      wxDC * dc, const wxRect & rect, bool bSelected, const int vrul );
   // void DrawBordersWithin(
   //   wxDC * dc, const wxRect & rect, const Track &track ) const;

   void GetCloseBoxHorizontalBounds( const wxRect & rect, wxRect &dest );
   void GetCloseBoxRect(const wxRect & rect, wxRect &dest);

   void GetTitleBarHorizontalBounds( const wxRect & rect, wxRect &dest );
   void GetTitleBarRect(const wxRect & rect, wxRect &dest);

   void GetNarrowMuteHorizontalBounds
      ( const wxRect & rect, wxRect &dest );
   void GetNarrowSoloHorizontalBounds
      ( const wxRect & rect, wxRect &dest );
   void GetWideMuteSoloHorizontalBounds
      ( const wxRect & rect, wxRect &dest );
   void GetMuteSoloRect
      (const wxRect & rect, wxRect &dest, bool solo, bool bHasSoloButton,
       const Track *pTrack);

   void GetSliderHorizontalBounds( const wxPoint &topleft, wxRect &dest );

   void GetGainRect(const wxPoint & topLeft, wxRect &dest);

   void GetPanRect(const wxPoint & topLeft, wxRect &dest);

#ifdef EXPERIMENTAL_MIDI_OUT
   void GetVelocityRect(const wxPoint & topLeft, wxRect &dest);
#endif

   void GetMinimizeHorizontalBounds( const wxRect &rect, wxRect &dest );
   void GetMinimizeRect(const wxRect & rect, wxRect &dest);

   void GetSelectButtonHorizontalBounds( const wxRect &rect, wxRect &dest );
   void GetSelectButtonRect(const wxRect & rect, wxRect &dest);

   void GetSyncLockHorizontalBounds( const wxRect &rect, wxRect &dest );
   void GetSyncLockIconRect(const wxRect & rect, wxRect &dest);

#ifdef USE_MIDI
   void GetMidiControlsHorizontalBounds
      ( const wxRect &rect, wxRect &dest );
   void GetMidiControlsRect(const wxRect & rect, wxRect &dest);
#endif

   bool HideTopItem( const wxRect &rect, const wxRect &subRect,
                               int allowance = 0 );

   unsigned DefaultNoteTrackHeight();
   unsigned DefaultWaveTrackHeight();

   LWSlider * GainSlider
      (const wxRect &sliderRect, const WaveTrack *t, bool captured,
       wxWindow *pParent);
   LWSlider * PanSlider
      (const wxRect &sliderRect, const WaveTrack *t, bool captured,
       wxWindow *pParent);

#ifdef EXPERIMENTAL_MIDI_OUT
   LWSlider * VelocitySlider
      (const wxRect &sliderRect, const NoteTrack *t, bool captured,
       wxWindow *pParent);
#endif

   void UpdatePrefs( wxWindow *pParent );
};


const int DragThreshold = 3;// Anything over 3 pixels is a drag, else a click.


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
   kSeparatorThickness = kBottomMargin + kTopMargin,
};

enum : int {
   kTrackInfoWidth = 100 - kLeftMargin,
   kTrackInfoBtnSize = 18, // widely used dimension, usually height
   kTrackInfoSliderHeight = 25,
   kTrackInfoSliderWidth = 84,
   kTrackInfoSliderAllowance = 5,
   kTrackInfoSliderExtra = 5,
};

#ifdef USE_MIDI
enum : int {
   // PRL:  was it correct to include the margin?
   kMidiCellWidth = ( ( kTrackInfoWidth + kLeftMargin ) / 4) - 2,
   kMidiCellHeight = kTrackInfoBtnSize
};
#endif

class AUDACITY_DLL_API TrackPanel final
   : public CellularPanel
   , public NonKeystrokeInterceptingWindow
{
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

   void UpdatePrefs();
   void ApplyUpdatedTheme();

   void OnPaint(wxPaintEvent & event);
   void OnMouseEvent(wxMouseEvent & event);
   void OnKeyDown(wxKeyEvent & event);

   void OnPlayback(wxEvent &);
   void OnTrackListResizing(TrackListEvent & event);
   void OnTrackListDeletion(wxEvent & event);
   void UpdateViewIfNoTracks(); // Call this to update mViewInfo, etc, after track(s) removal, before Refresh().

   double GetMostRecentXPos();

   void OnIdle(wxIdleEvent & event);
   void OnTimer(wxTimerEvent& event);

   int GetLeftOffset() const { return GetLabelWidth() + 1;}

   // Width and height, relative to upper left corner at (GetLeftOffset(), 0)
   // Either argument may be NULL
   void GetTracksUsableArea(int *width, int *height) const;

   void Refresh
      (bool eraseBackground = true, const wxRect *rect = (const wxRect *) NULL)
      override;

   void RefreshTrack(Track *trk, bool refreshbacking = true);

   void DisplaySelection();

   // These two are neither used nor defined as of Nov-2011
   // void SetSelectionFormat(int iformat)
   // void SetSnapTo(int snapto)

   void HandlePageUpKey();
   void HandlePageDownKey();
   AudacityProject * GetProject() const override;

   void ScrollIntoView(double pos);
   void ScrollIntoView(int x);

   void OnTrackMenu(Track *t = NULL);
   Track * GetFirstSelectedTrack();

   void EnsureVisible(Track * t);
   void VerticalScroll( float fracPosition);

   TrackPanelCell *GetFocusedCell() override;
   void SetFocusedCell() override;
   Track *GetFocusedTrack();
   void SetFocusedTrack(Track *t);

   void UpdateVRulers();
   void UpdateVRuler(Track *t);
   void UpdateTrackVRuler(const Track *t);
   void UpdateVRulerSize();

   // Returns the time corresponding to the pixel column one past the track area
   // (ignoring any fisheye)
   double GetScreenEndTime() const;

 protected:
   bool IsAudioActive();

public:
   size_t GetTrackCount() const;
   size_t GetSelectedTrackCount() const;

protected:
   void UpdateSelectionDisplay();

public:
   void UpdateAccessibility();
   void MessageForScreenReader(const wxString& message);

   void MakeParentRedrawScrollbars();

   // Rectangle includes track control panel, and the vertical ruler, and
   // the proper track area of all channels, and the separators between them.
   wxRect FindTrackRect( const Track * target );

protected:
   void MakeParentModifyState(bool bWantsAutoSave);    // if true, writes auto-save file. Should set only if you really want the state change restored after
                                                               // a crash, as it can take many seconds for large (eg. 10 track-hours) projects

   // Get the root object defining a recursive subdivision of the panel's
   // area into cells
   std::shared_ptr<TrackPanelNode> Root() override;

   int GetVRulerWidth() const;
   int GetVRulerOffset() const { return kTrackInfoWidth + kLeftMargin; }

public:
   int GetLabelWidth() const
      { return GetVRulerOffset() + GetVRulerWidth(); }

// JKC Nov-2011: These four functions only used from within a dll such as mod-track-panel
// They work around some messy problems with constructors.
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
   void DrawTracks(wxDC * dc);

   void DrawEverythingElse(TrackPanelDrawingContext &context,
                           const wxRegion & region,
                           const wxRect & clip);
   void DrawOutside(
      TrackPanelDrawingContext &context,
      const Track *leaderTrack, const wxRect & teamRect);

   void HighlightFocusedTrack (wxDC* dc, const wxRect &rect);
   void DrawShadow            ( wxDC* dc, const wxRect & rect );
   void DrawBordersAroundTrack(wxDC* dc, const wxRect & rect );
   void ClearTopMargin        (
      TrackPanelDrawingContext &context, const wxRect &clip);
   void ClearLeftAndRightMargins    (
      TrackPanelDrawingContext &context, const wxRect & clip);
   void ClearSeparator    (
      TrackPanelDrawingContext &context, const wxRect & rect);
   void DrawSash              (
      wxDC* dc, const wxRect & rect, int labelw, bool bSelected );

public:
   // Set the object that performs catch-all event handling when the pointer
   // is not in any track or ruler or control panel.
   void SetBackgroundCell
      (const std::shared_ptr< TrackPanelCell > &pCell);
   std::shared_ptr< TrackPanelCell > GetBackgroundCell();

public:
   // Accessors...
   static bool HasSoloButton(){  return gSoloPref!=wxT("None");}

public:

   LWSlider *GainSlider( const WaveTrack *wt );
   LWSlider *PanSlider( const WaveTrack *wt );
#ifdef EXPERIMENTAL_MIDI_OUT
   LWSlider *VelocitySlider( const NoteTrack *nt );
#endif

protected:
   TrackPanelListener *mListener;

   std::shared_ptr<TrackList> mTracks;

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

   std::shared_ptr<TrackPanelCell> mpBackground;

   DECLARE_EVENT_TABLE()

   void ProcessUIHandleResult
      (TrackPanelCell *pClickedTrack, TrackPanelCell *pLatestCell,
       unsigned refreshResult) override;

   void UpdateStatusMessage( const wxString &status ) override;

   // friending GetInfoCommand allow automation to get sizes of the
   // tracks, track control panel and such.
   friend class GetInfoCommand;
};

// A predicate class
struct IsVisibleTrack
{
   IsVisibleTrack(AudacityProject *project);

   bool operator () (const Track *pTrack) const;

   wxRect mPanelRect;
};

#endif
