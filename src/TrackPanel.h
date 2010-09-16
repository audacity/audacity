/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackPanel.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_TRACK_PANEL__
#define __AUDACITY_TRACK_PANEL__

#include <wx/dcmemory.h>
#include <wx/dynarray.h>
#include <wx/panel.h>
#include <wx/timer.h>
#include <wx/window.h>

//Stm:  The following included because of the sampleCount struct.
#include "Sequence.h"  
#include "WaveClip.h"
#include "WaveTrack.h"
#include "LabelTrack.h"

class wxMenu;
class wxRect;

class TrackPanel;
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

struct ViewInfo;

WX_DEFINE_ARRAY(LWSlider *, LWSliderArray);

class TrackClip
{
 public:
   TrackClip(Track *t, WaveClip *c) { track = t; clip = c; }
   Track *track;
   WaveClip *clip;
};

WX_DECLARE_OBJARRAY(TrackClip, TrackClipArray);

class AUDACITY_DLL_API TrackPanelListener {

 public:
   TrackPanelListener(){};
   virtual ~TrackPanelListener(){};

   virtual void TP_DisplaySelection() = 0;
   virtual void TP_DisplayStatusMessage(wxString msg) = 0;

   virtual int TP_GetCurrentTool() = 0;
   virtual ToolsToolBar * TP_GetToolsToolBar() = 0;
   virtual ControlToolBar * TP_GetControlToolBar() = 0;

   virtual void TP_OnPlayKey() = 0;
   virtual void TP_PushState(wxString shortDesc, wxString longDesc,
                             bool consolidate = false) = 0;
   virtual void TP_ModifyState() = 0;
   virtual void TP_RedrawScrollbars() = 0;
   virtual void TP_ScrollLeft() = 0;
   virtual void TP_ScrollRight() = 0;
   virtual void TP_ScrollWindow(double scrollto) = 0;
   virtual void TP_ScrollUpDown(int delta) = 0;
   virtual void TP_HandleResize() = 0;
};

// 
// TrackInfo sliders: we keep a constant number of sliders, and attach them to
// tracks as they come on screen (this helps deal with very large numbers of
// tracks, esp. on MSW).
//

const unsigned int kInitialSliders = 100;
// kInitialSliders-kSliderPageFlipshould be around the most tracks you could
// ever fit vertically on the screen (but if more fit on that's OK too)
const unsigned int kSliderPageFlip = 80;

class TrackInfo
{
public:
   TrackInfo(wxWindow * pParentIn);
   ~TrackInfo();

   int GetTrackInfoWidth() const;

   void UpdateSliderOffset(Track *t);

private:
   void MakeMoreSliders();
   void EnsureSufficientSliders(int index);

   void SetTrackInfoFont(wxDC *dc);
   void DrawBackground(wxDC * dc, const wxRect r, bool bSelected, bool bHasMuteSolo, const int labelw, const int vrul);
   void DrawBordersWithin(wxDC * dc, const wxRect r, bool bHasMuteSolo );
   void DrawCloseBox(wxDC * dc, const wxRect r, bool down);
   void DrawTitleBar(wxDC * dc, const wxRect r, Track * t, bool down);
   void DrawMuteSolo(wxDC * dc, const wxRect r, Track * t, bool down, bool solo, bool bHasSoloButton);
   void DrawVRuler(wxDC * dc, const wxRect r, Track * t);
   void DrawSliders(wxDC * dc, WaveTrack *t, wxRect r);

   // Draw the minimize button *and* the sync-lock track icon, if necessary.
   void DrawMinimize(wxDC * dc, const wxRect r, Track * t, bool down);

   void GetTrackControlsRect(const wxRect r, wxRect &dest) const;
   void GetCloseBoxRect(const wxRect r, wxRect &dest) const;
   void GetTitleBarRect(const wxRect r, wxRect &dest) const;
   void GetMuteSoloRect(const wxRect r, wxRect &dest, bool solo, bool bHasSoloButton) const;
   void GetGainRect(const wxRect r, wxRect &dest) const;
   void GetPanRect(const wxRect r, wxRect &dest) const;
   void GetMinimizeRect(const wxRect r, wxRect &dest) const;
   void GetSyncLockIconRect(const wxRect r, wxRect &dest) const;

   // These arrays are always kept the same size.
   LWSliderArray mGains;
   LWSliderArray mPans;

   // index of track whose pan/gain sliders are at index 0 in the above arrays
   unsigned int mSliderOffset;

public:

   // Slider access by track index
   LWSlider * GainSlider(int trackIndex);
   LWSlider * PanSlider(int trackIndex);

   wxWindow * pParent;
   wxFont mFont;

   friend class TrackPanel;
};



const int DragThreshold = 3;// Anything over 3 pixels is a drag, else a click.


class TrackPanel:public wxPanel {
 public:


   TrackPanel(wxWindow * parent,
              wxWindowID id,
              const wxPoint & pos,
              const wxSize & size,
              TrackList * tracks,
              ViewInfo * viewInfo,
              TrackPanelListener * listener,
              AdornedRulerPanel * ruler );

   virtual ~ TrackPanel();

   void BuildMenus(void);
   void DeleteMenus(void);

   void UpdatePrefs();

   void OnSize(wxSizeEvent & event);
   void OnErase(wxEraseEvent & event);
   void OnPaint(wxPaintEvent & event);
   void OnMouseEvent(wxMouseEvent & event);
   void OnCaptureLost(wxMouseCaptureLostEvent & event);
   void OnCaptureKey(wxCommandEvent & event);
   void OnKeyDown(wxKeyEvent & event);
   void OnChar(wxKeyEvent & event);

   void OnSetFocus(wxFocusEvent & event);
   void OnKillFocus(wxFocusEvent & event);

   void OnContextMenu(wxContextMenuEvent & event);

   void OnTrackListResized(wxCommandEvent & event);
   void OnTrackListUpdated(wxCommandEvent & event);

   double GetMostRecentXPos();

   void OnTimer();

   int GetLeftOffset() const { return GetLabelWidth() + 1;}

   void GetTracksUsableArea(int *width, int *height) const;

   void SelectNone();

   void SetStop(bool bStopped);

   virtual void Refresh(bool eraseBackground = true,
                        const wxRect *rect = (const wxRect *) NULL);
   void RefreshTrack(Track *trk, bool refreshbacking = true);

   void DisplaySelection();

   void SetSelectionFormat(int iformat);
   void SetSnapTo(int snapto);

   void HandleShiftKey(bool down);
   void HandleControlKey(bool down);
   AudacityProject * GetProject() const;

   void OnPrevTrack(bool shift = false);
   void OnNextTrack(bool shift = false);
   void OnToggle();

   void OnCursorLeft(bool shift, bool ctrl);
   void OnCursorRight(bool shift, bool ctrl);
   void OnCursorMove(bool forward, bool jump, bool longjump);
   void OnBoundaryMove(bool left, bool boundaryContract);
   void ScrollIntoView(double pos);
   void ScrollIntoView(int x);

   void OnTrackPan();
   void OnTrackPanLeft();
   void OnTrackPanRight();
   void OnTrackGain();
   void OnTrackGainDec();
   void OnTrackGainInc();
   void OnTrackMenu(Track *t = NULL);
   void OnTrackMute(bool shiftdown, Track *t = NULL);
   void OnTrackSolo(bool shiftdown, Track *t = NULL);
   void OnTrackClose();
   Track * GetFirstSelectedTrack();

   void EnsureVisible(Track * t);

   Track *GetFocusedTrack();
   void SetFocusedTrack(Track *t);

   void HandleCursorForLastMouseEvent();

   void UpdateVRulers();
   void UpdateVRuler(Track *t);
   void UpdateTrackVRuler(Track *t);
   void UpdateVRulerSize();

 private:
   MixerBoard* GetMixerBoard();
   bool IsUnsafe();
   bool HandleLabelTrackMouseEvent(LabelTrack * lTrack, wxRect &r, wxMouseEvent & event);
   bool HandleTrackLocationMouseEvent(WaveTrack * track, wxRect &r, wxMouseEvent &event);
   void HandleTrackSpecificMouseEvent(wxMouseEvent & event);
   void DrawIndicator();
   void DoDrawIndicator(wxDC & dc);
   void DrawCursor();
   void DoDrawCursor(wxDC & dc);

   void ScrollDuringDrag();

   // Working out where to dispatch the event to.
   int DetermineToolToUse( ToolsToolBar * pTtb, wxMouseEvent & event);
   bool HitTestEnvelope(Track *track, wxRect &r, wxMouseEvent & event);
   bool HitTestSamples(Track *track, wxRect &r, wxMouseEvent & event);
   bool HitTestSlide(Track *track, wxRect &r, wxMouseEvent & event);

   // AS: Selection handling
   void HandleSelect(wxMouseEvent & event);
   void SelectionHandleDrag(wxMouseEvent &event, Track *pTrack);
   void SelectionHandleClick(wxMouseEvent &event, 
			     Track* pTrack, wxRect r);
   void StartSelection (int mouseXCoordinate, int trackLeftEdge);
   void ExtendSelection(int mouseXCoordinate, int trackLeftEdge,
                        Track *pTrack);
   void SelectTracksByLabel( LabelTrack *t );
   void SelectTrackLength(Track *t);

   // Helper for moving by keyboard with snap-to-grid enabled
   double GridMove(double t, int minPix);

   // AS: Cursor handling
   bool SetCursorByActivity( );
   void SetCursorAndTipWhenInLabel( Track * t, wxMouseEvent &event, const wxChar ** ppTip );
   void SetCursorAndTipWhenInVResizeArea( Track * label, bool blinked, const wxChar ** ppTip );
   void SetCursorAndTipWhenInLabelTrack( LabelTrack * pLT, wxMouseEvent & event, const wxChar ** ppTip );
   void SetCursorAndTipWhenSelectTool( Track * t, wxMouseEvent & event, wxRect &r, bool bMultiToolMode, const wxChar ** ppTip );
   void SetCursorAndTipByTool( int tool, wxMouseEvent & event, const wxChar **ppTip );
   void HandleCursor(wxMouseEvent & event);

   // AS: Envelope editing handlers
   void HandleEnvelope(wxMouseEvent & event);
   void ForwardEventToTimeTrackEnvelope(wxMouseEvent & event);
   void ForwardEventToWaveTrackEnvelope(wxMouseEvent & event);
   void ForwardEventToEnvelope(wxMouseEvent &event);

   // AS: Track sliding handlers
   void HandleSlide(wxMouseEvent & event);
   void StartSlide(wxMouseEvent &event);
   void DoSlide(wxMouseEvent &event);
   void AddClipsToCaptured(Track *t, bool withinSelection);
   void AddClipsToCaptured(Track *t, double t0, double t1);

   // AS: Handle zooming into tracks
   void HandleZoom(wxMouseEvent & event);
   void HandleZoomClick(wxMouseEvent & event);
   void HandleZoomDrag(wxMouseEvent & event);
   void HandleZoomButtonUp(wxMouseEvent & event);

   bool IsDragZooming();
   void DragZoom(wxMouseEvent &event, int x);
   void DoZoomInOut(wxMouseEvent &event, int x);

   void HandleVZoom(wxMouseEvent & event);
   void HandleVZoomClick(wxMouseEvent & event);
   void HandleVZoomDrag(wxMouseEvent & event);
   void HandleVZoomButtonUp(wxMouseEvent & event);

   // Handle sample editing using the 'draw' tool.
   bool IsSampleEditingPossible( wxMouseEvent & event, Track * t );
   void HandleSampleEditing(wxMouseEvent & event);
   void HandleSampleEditingClick( wxMouseEvent & event );
   void HandleSampleEditingDrag( wxMouseEvent & event );
   void HandleSampleEditingButtonUp( wxMouseEvent & event );

   // MM: Handle mouse wheel rotation
   void HandleWheelRotation(wxMouseEvent & event);

   // Handle resizing.
   void HandleResizeClick(wxMouseEvent & event);
   void HandleResizeDrag(wxMouseEvent & event);
   void HandleResizeButtonUp(wxMouseEvent & event);
   void HandleResize(wxMouseEvent & event);

   void HandleLabelClick(wxMouseEvent & event);
   void HandleRearrange(wxMouseEvent & event);
   void CalculateRearrangingThresholds(wxMouseEvent & event);
   void HandleClosing(wxMouseEvent & event);
   void HandlePopping(wxMouseEvent & event);
   void HandleMutingSoloing(wxMouseEvent & event, bool solo);
   void HandleMinimizing(wxMouseEvent & event);
   void HandleSliders(wxMouseEvent &event, bool pan);


   // These *Func methods are used in TrackPanel::HandleLabelClick to set up 
   // for actual handling in methods called by TrackPanel::OnMouseEvent, and 
   // to draw button-down states, etc.
   bool CloseFunc(Track * t, wxRect r, int x, int y);
   bool PopupFunc(Track * t, wxRect r, int x, int y);

   // TrackSelFunc, unlike the other *Func methods, returns true if the click is not 
   // set up to be handled, but click is on the sync-lock icon or the blank area to 
   // the left of the minimize button, and we want to pass it forward to be a track select. 
   bool TrackSelFunc(Track * t, wxRect r, int x, int y);

   bool MuteSoloFunc(Track *t, wxRect r, int x, int f, bool solo);
   bool MinimizeFunc(Track *t, wxRect r, int x, int f);
   bool GainFunc(Track * t, wxRect r, wxMouseEvent &event,
                 int x, int y);
   bool PanFunc(Track * t, wxRect r, wxMouseEvent &event,
                int x, int y);


   void MakeParentRedrawScrollbars();
   
   // AS: Pushing the state preserves state for Undo operations.
   void MakeParentPushState(wxString desc, wxString shortDesc,
                            bool consolidate = false);
   void MakeParentModifyState();

   void MakeParentResize();

   void OnSetName(wxCommandEvent &event);

   void OnSetFont(wxCommandEvent &event);

   void OnMoveTrack    (wxCommandEvent &event);
   void OnChangeOctave (wxCommandEvent &event);
   void OnChannelChange(wxCommandEvent &event);
   void OnSetDisplay   (wxCommandEvent &event);
   void OnSetTimeTrackRange (wxCommandEvent &event);

   void SetMenuCheck( wxMenu & menu, int newId );
   void SetRate(Track *pTrack, double rate);
   void OnRateChange(wxCommandEvent &event);
   void OnRateOther(wxCommandEvent &event);

   void OnFormatChange(wxCommandEvent &event);

   void OnSplitStereo(wxCommandEvent &event);
   void OnSplitStereoMono(wxCommandEvent &event);
   void SplitStereo(bool stereo);
   void OnMergeStereo(wxCommandEvent &event);
   void OnCutSelectedText(wxCommandEvent &event);
   void OnCopySelectedText(wxCommandEvent &event);
   void OnPasteSelectedText(wxCommandEvent &event);

   void SetTrackPan(Track * t, LWSlider * s);
   void SetTrackGain(Track * t, LWSlider * s);

   void RemoveTrack(Track * toRemove);

   // Find track info by coordinate
   Track *FindTrack(int mouseX, int mouseY, bool label, bool link,
                     wxRect * trackRect = NULL);

   wxRect FindTrackRect(Track * target, bool label);

   int GetVRulerWidth() const;
   int GetVRulerOffset() const { return mTrackInfo.GetTrackInfoWidth(); };

   int GetLabelWidth() const { return mTrackInfo.GetTrackInfoWidth() + GetVRulerWidth(); };

private:
   void DrawTracks(wxDC * dc);

   void DrawEverythingElse(wxDC *dc, const wxRegion region,
                           const wxRect panelRect, const wxRect clip);
   void DrawOutside(Track *t, wxDC *dc, const wxRect rec,
                    const wxRect trackRect);
   void DrawZooming(wxDC* dc, const wxRect clip);

   void HighlightFocusedTrack (wxDC* dc, const wxRect r);
   void DrawShadow            (Track *t, wxDC* dc, const wxRect r);
   void DrawBordersAroundTrack(Track *t, wxDC* dc, const wxRect r, const int labelw, const int vrul);
   void DrawOutsideOfTrack    (Track *t, wxDC* dc, const wxRect r);

   int IdOfRate( int rate );
   int IdOfFormat( int format );

   // Accessors...
   bool HasSoloButton(){  return mSoloPref!=wxT("None");};

   //JKC: These two belong in the label track.
   int mLabelTrackStartXPos;
   int mLabelTrackStartYPos;
   
   wxString TrackSubText(Track *t);

   bool MoveClipToTrack(WaveClip *clip, WaveTrack* src, WaveTrack* dst);

   TrackInfo mTrackInfo;

   TrackPanelListener *mListener;

   TrackList *mTracks;
   ViewInfo *mViewInfo;

   AdornedRulerPanel *mRuler;

   double mSeekShort;
   double mSeekLong;

   TrackArtist *mTrackArtist;

   class AudacityTimer:public wxTimer {
   public:
     virtual void Notify() { parent->OnTimer(); }
     TrackPanel *parent;
   } mTimer;
   

   // This stores the parts of the screen that get overwritten by the indicator
   // and cursor
   double mLastIndicator;
   double mLastCursor;

   int mTimeCount;

   wxMemoryDC mBackingDC;
   wxBitmap *mBacking;
   bool mRefreshBacking;
   int mPrevWidth;
   int mPrevHeight;

   wxLongLong mLastSelectionAdjustment;

   double mSelStart;

   Track *mCapturedTrack;
   Envelope *mCapturedEnvelope;
   WaveClip *mCapturedClip;
   TrackClipArray mCapturedClipArray;
   bool mCapturedClipIsSelection;
   WaveTrack::Location mCapturedTrackLocation;
   wxRect mCapturedTrackLocationRect;
   wxRect mCapturedRect;

   // When sliding horizontally, the moving clip may automatically
   // snap to the beginning and ending of other clips, or to label
   // starts and stops.  When you start sliding, SlideSnapFromPoints
   // gets populated with the start and stop times of selected clips,
   // and SlideSnapToPoints gets populated with the start and stop times
   // of other clips.  In both cases, times that are within 3 pixels
   // of another at the same zoom level are eliminated; you can't snap
   // when there are two things arbitrarily close at that zoom level.
   wxBaseArrayDouble mSlideSnapFromPoints;
   wxBaseArrayDouble mSlideSnapToPoints;
   wxArrayInt mSlideSnapLinePixels;

   // The amount that clips are sliding horizontally; this allows
   // us to undo the slide and then slide it by another amount
   double mHSlideAmount;

   bool mDidSlideVertically;

   bool mRedrawAfterStop;

   bool mIndicatorShowing;

   wxMouseEvent mLastMouseEvent;

   int mMouseClickX;
   int mMouseClickY;

   int mMouseMostRecentX;
   int mMouseMostRecentY;

   int mZoomStart;
   int mZoomEnd;

   // Handles snapping the selection boundaries or track boundaries to
   // line up with existing tracks or labels.  mSnapLeft and mSnapRight
   // are the horizontal index of pixels to display user feedback
   // guidelines so the user knows when such snapping is taking place.
   SnapManager *mSnapManager;
   wxInt64 mSnapLeft;
   wxInt64 mSnapRight;
   bool mSnapPreferRightEdge;

   Track * mDrawingTrack;          // Keeps track of which track you are drawing on between events cf. HandleDraw()
   int mDrawingTrackTop;           // Keeps track of the top position of the drawing track.
   sampleCount mDrawingStartSample;   // sample of last click-down
   float mDrawingStartSampleValue;    // value of last click-down
   sampleCount mDrawingLastDragSample; // sample of last drag-over
   float mDrawingLastDragSampleValue;  // value of last drag-over
 
   double PositionToTime(wxInt64 mouseXCoordinate,
                         wxInt64 trackLeftEdge) const;
   wxInt64 TimeToPosition(double time,
                          wxInt64 trackLeftEdge) const;

   int mInitialTrackHeight;
   int mInitialUpperTrackHeight;
   bool mAutoScrolling;

   enum   MouseCaptureEnum
   {
      IsUncaptured=0,   // This is the normal state for the mouse
      IsVZooming,
      IsClosing,
      IsSelecting,
      IsAdjustingLabel,
      IsResizing,
      IsResizingBetweenLinkedTracks,
      IsResizingBelowLinkedTracks,
      IsRearranging,
      IsSliding,
      IsEnveloping,
      IsMuting,
      IsSoloing,
      IsGainSliding,
      IsPanSliding,
      IsMinimizing,
      IsOverCutLine,
      WasOverCutLine,
      IsPopping,
      IsZooming
   };

   enum MouseCaptureEnum mMouseCapture;
   void SetCapturedTrack( Track * t, enum MouseCaptureEnum MouseCapture=IsUncaptured );

   bool mAdjustSelectionEdges;
   bool mSlideUpDownOnly;
   bool mCircularTrackNavigation;
   float mdBr;

   // JH: if the user is dragging a track, at what y
   //   coordinate should the dragging track move up or down?
   int mMoveUpThreshold;
   int mMoveDownThreshold;

   wxCursor *mArrowCursor;
   wxCursor *mPencilCursor;
   wxCursor *mSelectCursor;
   wxCursor *mResizeCursor;
   wxCursor *mSlideCursor;
   wxCursor *mEnvelopeCursor;
   wxCursor *mSmoothCursor;
   wxCursor *mZoomInCursor;
   wxCursor *mZoomOutCursor;
   wxCursor *mLabelCursorLeft;
   wxCursor *mLabelCursorRight;
   wxCursor *mRearrangeCursor;
   wxCursor *mDisabledCursor;
   wxCursor *mAdjustLeftSelectionCursor;
   wxCursor *mAdjustRightSelectionCursor;

   wxMenu *mWaveTrackMenu;
   wxMenu *mNoteTrackMenu;
   wxMenu *mTimeTrackMenu;
   wxMenu *mLabelTrackMenu;
   wxMenu *mRateMenu;
   wxMenu *mFormatMenu;
   wxMenu *mLabelTrackInfoMenu;

   Track *mPopupMenuTarget;

   friend class TrackPanelAx;

   TrackPanelAx *mAx;

   wxString mSoloPref;

   // Keeps track of extra fractional vertical scroll steps
   double mVertScrollRemainder;

 private:

   // The screenshot class needs to access internals
   friend class ScreenshotCommand;

 public:
   wxSize vrulerSize;

   DECLARE_EVENT_TABLE()
};

//This constant determines the size of the vertical region (in pixels) around
//the bottom of a track that can be used for vertical track resizing.
#define TRACK_RESIZE_REGION 5

//This constant determines the size of the horizontal region (in pixels) around
//the right and left selection bounds that can be used for horizontal selection adjusting
#define SELECTION_RESIZE_REGION 3

#define SMOOTHING_KERNEL_RADIUS 3
#define SMOOTHING_BRUSH_RADIUS 5
#define SMOOTHING_PROPORTION_MAX 0.7
#define SMOOTHING_PROPORTION_MIN 0.0

#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 1f8c3d0e-849e-4f3c-95b5-9ead0789f999
