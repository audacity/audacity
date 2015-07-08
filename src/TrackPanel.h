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
#include "WaveTrackLocation.h"

#include "Track.h"
#include "Snap.h"
#include "widgets/OverlayPanel.h"

#include "SelectionState.h"

class wxMenu;
class wxRect;

class EnvelopeEditor;
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
class Envelope;
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
   void DrawCloseBox(wxDC * dc, const wxRect & rect, Track * t, bool down) const;
   void DrawTitleBar(wxDC * dc, const wxRect & rect, Track * t, bool down) const;
   void DrawMuteSolo(wxDC * dc, const wxRect & rect, Track * t, bool down, bool solo, bool bHasSoloButton) const;
   void DrawVRuler(wxDC * dc, const wxRect & rect, Track * t) const;

   // Draw the minimize button *and* the sync-lock track icon, if necessary.
   void DrawMinimize(wxDC * dc, const wxRect & rect, Track * t, bool down) const;

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

public:
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


struct ClipMoveState {
   // non-NULL only if click was in a WaveTrack and without Shift key:
   WaveClip *capturedClip {};

   bool capturedClipIsSelection {};
   TrackArray trackExclusions {};
   double hSlideAmount {};
   TrackClipArray capturedClipArray {};
   wxInt64 snapLeft { -1 }, snapRight { -1 };

   void clear()
   {
      capturedClip = nullptr;
      capturedClipIsSelection = false;
      trackExclusions.clear();
      hSlideAmount = 0;
      capturedClipArray.clear();
      snapLeft = snapRight = -1;
   }
};

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

   virtual void BuildMenusIfNeeded(void);
   virtual void BuildMenus(void);

   virtual void DeleteMenus(void);

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
   virtual void OnVRulerMenu(Track *t, wxMouseEvent *pEvent = NULL);
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

   static double OnClipMove
      (ViewInfo &viewInfo, Track *track,
       TrackList &trackList, bool syncLocked, bool right);

 protected:
   virtual MixerBoard* GetMixerBoard();
   /** @brief Populates the track pop-down menu with the common set of
    * initial items.
    *
    * Ensures that all pop-down menus start with Name, and the commands for moving
    * the track around, via a single set of c ode.
    * @param menu the menu to add the commands to.
    */
   virtual void BuildCommonDropMenuItems(wxMenu * menu);

   // left over from PRL's vertical ruler context menu experiment in 2.1.2
   // static void BuildVRulerMenuItems(wxMenu * menu, int firstId, const wxArrayString &names);

   virtual bool IsAudioActive();
   virtual bool IsUnsafe();
   virtual bool HandleLabelTrackClick(LabelTrack * lTrack, const wxRect &rect, wxMouseEvent & event);
   virtual void HandleGlyphDragRelease(LabelTrack * lTrack, wxMouseEvent & event);
   virtual void HandleTextDragRelease(LabelTrack * lTrack, wxMouseEvent & event);
   virtual bool HandleTrackLocationMouseEvent(WaveTrack * track, const wxRect &rect, wxMouseEvent &event);
   virtual bool IsOverCutline(WaveTrack * track, const wxRect &rect, const wxMouseEvent &event);
   virtual void HandleTrackSpecificMouseEvent(wxMouseEvent & event);

   virtual void ScrollDuringDrag();

   // Working out where to dispatch the event to.
   virtual int DetermineToolToUse( ToolsToolBar * pTtb, const wxMouseEvent & event);
   virtual bool HitTestEnvelope(Track *track, const wxRect &rect, const wxMouseEvent & event);
#ifdef USE_MIDI
   // data for NoteTrack interactive stretch operations:
   // Stretching applies to a selected region after quantizing the
   // region to beat boundaries (subbeat stretching is not supported,
   // but maybe it should be enabled with shift or ctrl or something)
   // Stretching can drag the left boundary (the right stays fixed),
   // the right boundary (the left stays fixed), or the center (splits
   // the selection into two parts: when left part grows, the right
   // part shrinks, keeping the leftmost and rightmost boundaries
   // fixed.
   enum StretchEnum {
      stretchNone = 0, // false value!
      stretchLeft,
      stretchCenter,
      stretchRight
   };
   struct StretchState {
      StretchEnum mMode { stretchCenter }; // remembers what to drag

      using QuantizedTimeAndBeat = std::pair< double, double >;

      bool mStretching {}; // true between mouse down and mouse up
      double mOrigT0 {};
      double mOrigT1 {};
      QuantizedTimeAndBeat mBeatCenter { 0, 0 };
      QuantizedTimeAndBeat mBeat0 { 0, 0 };
      QuantizedTimeAndBeat mBeat1 { 0, 0 };
      double mLeftBeats {}; // how many beats from left to cursor
      double mRightBeats {}; // how many beats from cursor to right
   } mStretchState;

   virtual StretchEnum HitTestStretch
      ( const Track *track, const wxRect &rect, const wxMouseEvent & event,
        StretchState *pState = nullptr );
   wxCursor *ChooseStretchCursor( StretchEnum mode );
   static StretchEnum ChooseStretchMode
      ( const wxMouseEvent &event, const wxRect &rect, const ViewInfo &viewInfo,
        const NoteTrack *nt, StretchState *pState = nullptr );
   virtual void Stretch(int mouseXCoordinate, int trackLeftEdge, Track *pTrack);
#endif

   // AS: Selection handling
   size_t GetTrackCount();
   size_t GetSelectedTrackCount();
   virtual void HandleSelect(wxMouseEvent & event);
   virtual void SelectionHandleDrag(wxMouseEvent &event, Track *pTrack);

protected:

   virtual void SelectionHandleClick(wxMouseEvent &event,
                                     Track* pTrack, wxRect rect);
   virtual void StartSelection (int mouseXCoordinate, int trackLeftEdge);
   virtual void ExtendSelection(int mouseXCoordinate, int trackLeftEdge,
                        Track *pTrack);
   virtual void UpdateSelectionDisplay();

public:
   virtual void UpdateAccessibility();
   void MessageForScreenReader(const wxString& message);

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
public:
   void SnapCenterOnce (const WaveTrack *pTrack, bool up);
protected:
   void StartSnappingFreqSelection (const WaveTrack *pTrack);
   void MoveSnappingFreqSelection (int mouseYCoordinate,
                                   int trackTopEdge,
                                   int trackHeight, Track *pTrack);
   void StartFreqSelection (int mouseYCoordinate, int trackTopEdge,
                            int trackHeight, Track *pTrack);
   void ExtendFreqSelection(int mouseYCoordinate, int trackTopEdge,
                            int trackHeight);
   void ResetFreqSelectionPin(double hintFrequency, bool logF);

#endif

   virtual void SelectTracksByLabel( LabelTrack *t );

   // AS: Cursor handling
   virtual bool SetCursorByActivity( );
   virtual bool SetCursorForCutline(WaveTrack * track, const wxRect &rect, const wxMouseEvent &event);
   virtual void SetCursorAndTipWhenInLabel( Track * t, const wxMouseEvent &event, wxString &tip );
   virtual void SetCursorAndTipWhenInVResizeArea( bool blinked, wxString &tip );
   virtual void SetCursorAndTipWhenInLabelTrack( LabelTrack * pLT, const wxMouseEvent & event, wxString &tip );
   virtual void SetCursorAndTipWhenSelectTool
      ( Track * t, const wxMouseEvent & event, const wxRect &rect, bool bMultiToolMode, wxString &tip, const wxCursor ** ppCursor );
   virtual void SetCursorAndTipByTool( int tool, const wxMouseEvent & event, wxString &tip );

public:
   virtual void HandleCursor(wxMouseEvent & event);

protected:
   virtual void MaySetOnDemandTip( Track * t, wxString &tip );

   // AS: Envelope editing handlers
   virtual void HandleEnvelope(wxMouseEvent & event);
   virtual void ForwardEventToTimeTrackEnvelope(wxMouseEvent & event);
   virtual void ForwardEventToWaveTrackEnvelope(wxMouseEvent & event);
   virtual void ForwardEventToEnvelope(wxMouseEvent &event);

public:
   static void DoSlideHorizontal
      ( ClipMoveState &state, TrackList &trackList, Track &capturedTrack );
   static void CreateListOfCapturedClips
      ( ClipMoveState &state, const ViewInfo &viewInfo, Track &capturedTrack,
        TrackList &trackList, bool syncLocked, double clickTime );
   static void AddClipsToCaptured
      ( ClipMoveState &state, const ViewInfo &viewInfo,
        Track *t, bool withinSelection );
   static void AddClipsToCaptured
      ( ClipMoveState &state, Track *t, double t0, double t1 );

protected:
   static bool IsDragZooming(int zoomStart, int zoomEnd);
   virtual bool IsDragZooming() { return IsDragZooming(mZoomStart, mZoomEnd); }

   virtual void HandleVZoom(wxMouseEvent & event);
   virtual void HandleVZoomClick(wxMouseEvent & event);
   virtual void HandleVZoomDrag(wxMouseEvent & event);
   virtual void HandleVZoomButtonUp(wxMouseEvent & event);
   virtual void HandleWaveTrackVZoom(WaveTrack *track, bool shiftDown, bool rightUp);
   static void HandleWaveTrackVZoom
      (TrackList *tracks, const wxRect &rect,
       int zoomStart, int zoomEnd,
       WaveTrack *track, bool shiftDown, bool rightUp,
       bool fixedMousePoint);

   // MM: Handle mouse wheel rotation
   virtual void HandleWheelRotation(wxMouseEvent & event);
   virtual void HandleWheelRotationInVRuler
      (wxMouseEvent &event, double steps, Track *pTrack, const wxRect &rect);

   // Handle resizing.
   virtual void HandleResizeClick(wxMouseEvent & event);
   virtual void HandleResizeDrag(wxMouseEvent & event);
   virtual void HandleResizeButtonUp(wxMouseEvent & event);
   virtual void HandleResize(wxMouseEvent & event);

   virtual void HandleLabelClick(wxMouseEvent & event);

   virtual void HandleRearrange(wxMouseEvent & event);
   virtual void CalculateRearrangingThresholds(wxMouseEvent & event);
   virtual void HandleClosing(wxMouseEvent & event);
   virtual void HandlePopping(wxMouseEvent & event);
   virtual void HandleMutingSoloing(wxMouseEvent & event, bool solo);
   virtual void HandleMinimizing(wxMouseEvent & event);
   virtual void HandleSliders(wxMouseEvent &event, bool pan);
#ifdef EXPERIMENTAL_MIDI_OUT
   virtual void HandleVelocitySlider(wxMouseEvent &event);
#endif


   // These *Func methods are used in TrackPanel::HandleLabelClick to set up
   // for actual handling in methods called by TrackPanel::OnMouseEvent, and
   // to draw button-down states, etc.
   virtual bool CloseFunc(Track * t, wxRect rect, int x, int y);
   virtual bool PopupFunc(Track * t, wxRect rect, int x, int y);

   virtual bool MuteSoloFunc(Track *t, wxRect rect, int x, int f, bool solo);
   virtual bool MinimizeFunc(Track *t, wxRect rect, int x, int f);
   virtual bool GainFunc(Track * t, wxRect rect, wxMouseEvent &event,
                 int x, int y);
   virtual bool PanFunc(Track * t, wxRect rect, wxMouseEvent &event,
                int x, int y);
#ifdef EXPERIMENTAL_MIDI_OUT
   virtual bool VelocityFunc(Track * t, wxRect rect, wxMouseEvent &event,
      int x, int y);
#endif


public:
   virtual void MakeParentRedrawScrollbars();

protected:
   // AS: Pushing the state preserves state for Undo operations.
   virtual void MakeParentPushState(const wxString &desc, const wxString &shortDesc); // use UndoPush::AUTOSAVE
   virtual void MakeParentPushState(const wxString &desc, const wxString &shortDesc,
                            UndoPush flags);
   virtual void MakeParentModifyState(bool bWantsAutoSave);    // if true, writes auto-save file. Should set only if you really want the state change restored after
                                                               // a crash, as it can take many seconds for large (eg. 10 track-hours) projects
   virtual void OnSetName(wxCommandEvent &event);

   virtual void OnSetFont(wxCommandEvent &event);

   virtual void OnMoveTrack    (wxCommandEvent &event);
   virtual void OnChangeOctave (wxCommandEvent &event);
   virtual void OnChannelChange(wxCommandEvent &event);
   virtual void OnSpectrogramSettings(wxCommandEvent &event);
   virtual void OnSetDisplay   (wxCommandEvent &event);
   virtual void OnSetTimeTrackRange (wxCommandEvent &event);
   virtual void OnTimeTrackLin(wxCommandEvent &event);
   virtual void OnTimeTrackLog(wxCommandEvent &event);
   virtual void OnTimeTrackLogInt(wxCommandEvent &event);

   virtual void OnWaveformScaleType(wxCommandEvent &event);
   virtual void OnSpectrumScaleType(wxCommandEvent &event);

   virtual void OnZoomInVertical(wxCommandEvent &event);
   virtual void OnZoomOutVertical(wxCommandEvent &event);
   virtual void OnZoomFitVertical(wxCommandEvent &event);

   virtual void SetMenuCheck( wxMenu & menu, int newId );
   virtual void SetRate(WaveTrack *pTrack, double rate);
   virtual void OnRateChange(wxCommandEvent &event);
   virtual void OnRateOther(wxCommandEvent &event);

   virtual void OnFormatChange(wxCommandEvent &event);

   virtual void OnSwapChannels(wxCommandEvent &event);
   virtual void OnSplitStereo(wxCommandEvent &event);
   virtual void OnSplitStereoMono(wxCommandEvent &event);
   virtual void SplitStereo(bool stereo);
   virtual void OnMergeStereo(wxCommandEvent &event);

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
   TrackList * GetTracks(){ return mTracks.get(); }
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
   virtual void DrawZooming(wxDC* dc, const wxRect & clip);

   virtual void HighlightFocusedTrack (wxDC* dc, const wxRect &rect);
   virtual void DrawShadow            (Track *t, wxDC* dc, const wxRect & rect);
   virtual void DrawBordersAroundTrack(Track *t, wxDC* dc, const wxRect & rect, const int labelw, const int vrul);
   virtual void DrawOutsideOfTrack    (Track *t, wxDC* dc, const wxRect & rect);

public:
   // Set the object that performs catch-all event handling when the pointer
   // is not in any track or ruler or control panel.
   virtual void SetBackgroundCell
      (const std::shared_ptr< TrackPanelCell > &pCell);

protected:
   virtual int IdOfRate( int rate );
   virtual int IdOfFormat( int format );

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   void UpdateVirtualStereoOrder();
#endif

public:
   // Accessors...
   static bool HasSoloButton(){  return gSoloPref!=wxT("None");}

protected:
   //JKC: These two belong in the label track.
   int mLabelTrackStartXPos;
   int mLabelTrackStartYPos;

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

   SelectedRegion mInitialSelection;
   SelectionState &GetSelectionState();
   std::unique_ptr<SelectionStateChanger> mSelectionStateChanger{};

   bool mSelStartValid;
   double mSelStart;

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   enum eFreqSelMode {
      FREQ_SEL_INVALID,

      FREQ_SEL_SNAPPING_CENTER,
      FREQ_SEL_PINNED_CENTER,
      FREQ_SEL_DRAG_CENTER,

      FREQ_SEL_FREE,
      FREQ_SEL_TOP_FREE,
      FREQ_SEL_BOTTOM_FREE,
   }  mFreqSelMode;
   // Following holds:
   // the center for FREQ_SEL_PINNED_CENTER,
   // the ratio of top to center (== center to bottom) for FREQ_SEL_DRAG_CENTER,
   // a frequency boundary for FREQ_SEL_FREE, FREQ_SEL_TOP_FREE, or
   // FREQ_SEL_BOTTOM_FREE,
   // and is ignored otherwise.
   double mFreqSelPin;
   const WaveTrack *mFreqSelTrack = NULL;
   std::unique_ptr<SpectrumAnalyst> mFrequencySnapper;

protected:

#endif

   Track *mCapturedTrack;
   Envelope *mCapturedEnvelope;
   WaveTrackLocation mCapturedTrackLocation;
   wxRect mCapturedTrackLocationRect;
   wxRect mCapturedRect;

   bool mRedrawAfterStop;

   wxMouseEvent mLastMouseEvent;

   int mMouseClickY;

   int mMouseMostRecentX;
   int mMouseMostRecentY;

   int mZoomStart;
   int mZoomEnd;

   // Handles snapping the selection boundaries or track boundaries to
   // line up with existing tracks or labels.  mSnapLeft and mSnapRight
   // are the horizontal index of pixels to display user feedback
   // guidelines so the user knows when such snapping is taking place.
   std::unique_ptr<SnapManager> mSnapManager;

   wxInt64 mSnapLeft { -1 };
   wxInt64 mSnapRight { -1 };

public:
   wxInt64 GetSnapLeft () const
   {
         return mSnapLeft ;
   }
   wxInt64 GetSnapRight() const
   {
         return mSnapRight;
   }

protected:

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   void HandleCenterFrequencyCursor
      (bool shiftDown, wxString &tip, const wxCursor ** ppCursor);

   void HandleCenterFrequencyClick
      (bool shiftDown, const WaveTrack *pTrack, double value);

   double PositionToFrequency(const WaveTrack *wt,
                              bool maySnap,
                              wxInt64 mouseYCoordinate,
                              wxInt64 trackTopEdge,
                              int trackHeight) const;
   wxInt64 FrequencyToPosition(const WaveTrack *wt,
                               double frequency,
                               wxInt64 trackTopEdge,
                               int trackHeight) const;
#endif

   enum SelectionBoundary {
      SBNone,
      SBLeft, SBRight,
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
      SBBottom, SBTop, SBCenter, SBWidth,
#endif
   };
   SelectionBoundary ChooseTimeBoundary
      (double selend, bool onlyWithinSnapDistance,
       wxInt64 *pPixelDist = NULL, double *pPinValue = NULL) const;
   SelectionBoundary ChooseBoundary
      (const wxMouseEvent & event, const Track *pTrack,
       const wxRect &rect,
       bool mayDragWidth,
       bool onlyWithinSnapDistance,
       double *pPinValue = NULL) const;

   bool mInitialMinimized;
   int mInitialTrackHeight;
   int mInitialActualHeight;
   int mInitialUpperTrackHeight;
   int mInitialUpperActualHeight;
   bool mAutoScrolling;

public:
   enum   MouseCaptureEnum
   {
      IsUncaptured=0,   // This is the normal state for the mouse
      IsVZooming,
      IsClosing,
      IsSelecting,
      IsAdjustingLabel,
      IsSelectingLabelText,
      IsResizing,
      IsResizingBetweenLinkedTracks,
      IsResizingBelowLinkedTracks,
      IsRearranging,
      IsEnveloping,
      IsMuting,
      IsSoloing,
      IsGainSliding,
      IsPanSliding,
      IsMinimizing,
      WasOverCutLine,
      IsPopping,
#ifdef EXPERIMENTAL_MIDI_OUT
      IsVelocitySliding,
#endif

   };

protected:
   enum MouseCaptureEnum mMouseCapture;
   virtual void SetCapturedTrack( Track * t, enum MouseCaptureEnum MouseCapture=IsUncaptured );

   // JH: if the user is dragging a track, at what y
   //   coordinate should the dragging track move up or down?
   int mMoveUpThreshold;
   int mMoveDownThreshold;
   int mRearrangeCount;

   std::unique_ptr<wxCursor>
      mArrowCursor, mSelectCursor,
      mResizeCursor, mEnvelopeCursor, // doubles as the center frequency cursor
                              // for spectral selection
      mZoomInCursor, mZoomOutCursor,
      mRearrangeCursor,
      mDisabledCursor, mAdjustLeftSelectionCursor, mAdjustRightSelectionCursor;
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   std::unique_ptr<wxCursor>
      mBottomFrequencyCursor, mTopFrequencyCursor, mBandWidthCursor;
#endif
#ifdef USE_MIDI
   std::unique_ptr<wxCursor>
      mStretchCursor, mStretchLeftCursor, mStretchRightCursor;
#endif

   std::unique_ptr<wxMenu> mWaveTrackMenu;
   size_t mChannelItemsInsertionPoint {};

   std::unique_ptr<wxMenu>
      mNoteTrackMenu, mTimeTrackMenu, mLabelTrackMenu,
      mRulerWaveformMenu, mRulerSpectrumMenu;

   // These sub-menus are owned by parent menus,
   // so not unique_ptrs
   wxMenu *mRateMenu{}, *mFormatMenu{};

   Track *mPopupMenuTarget {};

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

   // Keeps track of extra fractional vertical scroll steps
   double mVertScrollRemainder;

   std::unique_ptr<EnvelopeEditor> mEnvelopeEditor;
   std::unique_ptr<EnvelopeEditor> mEnvelopeEditorRight;

 protected:

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

//This constant determines the size of the horizontal region (in pixels) around
//the right and left selection bounds that can be used for horizontal selection adjusting
//(or, vertical distance around top and bottom bounds in spectrograms,
// for vertical selection adjusting)
#define SELECTION_RESIZE_REGION 3

#endif

