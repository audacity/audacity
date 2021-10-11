/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackPanel.cpp

  Dominic Mazzoni
  and lots of other contributors

  Implements TrackPanel.

********************************************************************//*!

\file TrackPanel.cpp
\brief
  Implements TrackPanel.

*//***************************************************************//**

\class TrackPanel
\brief
  The TrackPanel class coordinates updates and operations on the
  main part of the screen which contains multiple tracks.

  It uses many other classes, but in particular it uses the
  TrackInfo class to draw the controls area on the left of a track,
  and the TrackArtist class to draw the actual waveforms.

  Note that in some of the older code here,
  "Label" means the TrackInfo plus the vertical ruler.
  Confusing relative to LabelTrack labels.

  The TrackPanel manages multiple tracks and their TrackInfos.

  Note that with stereo tracks there will be one TrackInfo
  being used by two wavetracks.

*//*****************************************************************//**

\class TrackPanel::AudacityTimer
\brief Timer class dedicated to informing the TrackPanel that it
is time to refresh some aspect of the screen.

*//*****************************************************************/


#include "TrackPanel.h"
#include "TrackPanelConstants.h"

#include <wx/app.h>
#include <wx/setup.h> // for wxUSE_* macros

#include "AdornedRulerPanel.h"
#include "tracks/ui/CommonTrackPanelCell.h"
#include "KeyboardCapture.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "ProjectAudioManager.h"
#include "ProjectHistory.h"
#include "ProjectWindows.h"
#include "ProjectSettings.h"
#include "ProjectStatus.h"
#include "ProjectWindow.h"
#include "Theme.h"
#include "TrackPanelMouseEvent.h"
#include "TrackPanelResizeHandle.h"
//#define DEBUG_DRAW_TIMING 1

#include "UndoManager.h"

#include "AColor.h"
#include "AllThemeResources.h"
#include "AudioIO.h"
#include "float_cast.h"

#include "Prefs.h"
#include "RefreshCode.h"
#include "TrackArtist.h"
#include "TrackPanelAx.h"
#include "TrackPanelResizerCell.h"
#include "WaveTrack.h"

#include "tracks/ui/TrackControls.h"
#include "tracks/ui/TrackView.h"
#include "tracks/ui/TrackVRulerControls.h"

//This loads the appropriate set of cursors, depending on platform.
#include "../images/Cursors.h"

#include <algorithm>

#include <wx/dc.h>
#include <wx/dcclient.h>
#include <wx/graphics.h>

static_assert( kVerticalPadding == kTopMargin + kBottomMargin );
static_assert( kTrackInfoBtnSize == kAffordancesAreaHeight, "Drag bar is misaligned with the menu button");

/**

\class TrackPanel

This is a diagram of TrackPanel's division of one (non-stereo) track rectangle.
Total height equals TrackView::GetHeight()'s value.  Total width is the wxWindow's
width.  Each character that is not . represents one pixel.

Inset space of this track, and top inset of the next track, are used to draw the
focus highlight.

Top inset of the right channel of a stereo track, and bottom shadow line of the
left channel, are used for the channel separator.

"Margin" is a term used for inset plus border (top and left) or inset plus
shadow plus border (right and bottom).

GetVRulerOffset() counts columns from the left edge up to and including
controls, and is a constant.

GetVRulerWidth() is variable -- all tracks have the same ruler width at any
time, but that width may be adjusted when tracks change their vertical scales.

GetLeftOffset() counts columns up to and including the VRuler and one more,
the "one pixel" column.

Cell for label has a rectangle that OMITS left, top, and bottom
margins

Cell for vruler has a rectangle right of the label,
up to and including the One Pixel column, and OMITS top and bottom margins

Cell() for track returns a rectangle with x == GetLeftOffset(), and OMITS
right, top, and bottom margins

+--------------- ... ------ ... --------------------- ...       ... -------------+
| Top Inset                                                                      |
|                                                                                |
|  +------------ ... ------ ... --------------------- ...       ... ----------+  |
| L|+-Border---- ... ------ ... --------------------- ...       ... -Border-+ |R |
| e||+---------- ... -++--- ... -+++----------------- ...       ... -------+| |i |
| f|B|                ||         |||                                       |BS|g |
| t|o| Controls       || V       |O|  The good stuff                       |oh|h |
|  |r|                || R       |n|                                       |ra|t |
| I|d|                || u       |e|                                       |dd|  |
| n|e|                || l       | |                                       |eo|I |
| s|r|                || e       |P|                                       |rw|n |
| e|||                || r       |i|                                       ||||s |
| t|||                ||         |x|                                       ||||e |
|  |||                ||         |e|                                       ||||t |
|  |||                ||         |l|                                       ||||  |
|  |||                ||         |||                                       ||||  |

.  ...                ..         ...                                       ....  .
.  ...                ..         ...                                       ....  .
.  ...                ..         ...                                       ....  .

|  |||                ||         |||                                       ||||  |
|  ||+----------     -++--  ... -+++----------------- ...       ... -------+|||  |
|  |+-Border---- ... -----  ... --------------------- ...       ... -Border-+||  |
|  |  Shadow---- ... -----  ... --------------------- ...       ... --Shadow-+|  |
*/

// Is the distance between A and B less than D?
template < class A, class B, class DIST > bool within(A a, B b, DIST d)
{
   return (a > b - d) && (a < b + d);
}

BEGIN_EVENT_TABLE(TrackPanel, CellularPanel)
    EVT_MOUSE_EVENTS(TrackPanel::OnMouseEvent)
    EVT_KEY_DOWN(TrackPanel::OnKeyDown)

    EVT_PAINT(TrackPanel::OnPaint)

    EVT_TIMER(wxID_ANY, TrackPanel::OnTimer)

    EVT_SIZE(TrackPanel::OnSize)

END_EVENT_TABLE()

/// Makes a cursor from an XPM, uses CursorId as a fallback.
/// TODO:  Move this function to some other source file for reuse elsewhere.
std::unique_ptr<wxCursor> MakeCursor( int WXUNUSED(CursorId), const char * const pXpm[36],  int HotX, int HotY )
{
#define CURSORS_SIZE32
#ifdef CURSORS_SIZE32
   const int HotAdjust =0;
#else
   const int HotAdjust =8;
#endif

   wxImage Image = wxImage(wxBitmap(pXpm).ConvertToImage());
   Image.SetMaskColour(255,0,0);
   Image.SetMask();// Enable mask.

   Image.SetOption( wxIMAGE_OPTION_CUR_HOTSPOT_X, HotX-HotAdjust );
   Image.SetOption( wxIMAGE_OPTION_CUR_HOTSPOT_Y, HotY-HotAdjust );
   return std::make_unique<wxCursor>( Image );
}


namespace{

AttachedWindows::RegisteredFactory sKey{
   []( AudacityProject &project ) -> wxWeakRef< wxWindow > {
      auto &ruler = AdornedRulerPanel::Get( project );
      auto &viewInfo = ViewInfo::Get( project );
      auto &window = ProjectWindow::Get( project );
      auto mainPage = window.GetMainPage();
      wxASSERT( mainPage ); // to justify safenew

      auto &tracks = TrackList::Get( project );
      auto result = safenew TrackPanel(mainPage,
         window.NextWindowID(),
         wxDefaultPosition,
         wxDefaultSize,
         tracks.shared_from_this(),
         &viewInfo,
         &project,
         &ruler);
      SetProjectPanel( project, *result );
      return result;
   }
};

}

TrackPanel &TrackPanel::Get( AudacityProject &project )
{
   return GetAttachedWindows(project).Get< TrackPanel >( sKey );
}

const TrackPanel &TrackPanel::Get( const AudacityProject &project )
{
   return Get( const_cast< AudacityProject & >( project ) );
}

void TrackPanel::Destroy( AudacityProject &project )
{
   auto *pPanel = GetAttachedWindows(project).Find<TrackPanel>( sKey );
   if (pPanel) {
      pPanel->wxWindow::Destroy();
      GetAttachedWindows(project).Assign(sKey, nullptr);
   }
}

// Don't warn us about using 'this' in the base member initializer list.
#ifndef __WXGTK__ //Get rid if this pragma for gtk
#pragma warning( disable: 4355 )
#endif
TrackPanel::TrackPanel(wxWindow * parent, wxWindowID id,
                       const wxPoint & pos,
                       const wxSize & size,
                       const std::shared_ptr<TrackList> &tracks,
                       ViewInfo * viewInfo,
                       AudacityProject * project,
                       AdornedRulerPanel * ruler)
   : CellularPanel(parent, id, pos, size, viewInfo,
                   wxWANTS_CHARS | wxNO_BORDER),
     mListener( &ProjectWindow::Get( *project ) ),
     mTracks(tracks),
     mRuler(ruler),
     mTrackArtist(nullptr),
     mRefreshBacking(false)
#ifndef __WXGTK__   //Get rid if this pragma for gtk
#pragma warning( default: 4355 )
#endif
{
   SetLayoutDirection(wxLayout_LeftToRight);
   SetLabel(XO("Track Panel"));
   SetName(XO("Track Panel"));
   SetBackgroundStyle(wxBG_STYLE_PAINT);

   {
      auto pAx = std::make_unique <TrackPanelAx>( *project );
      pAx->SetWindow( this );
      wxWeakRef< TrackPanel > weakThis{ this };
      pAx->SetFinder(
         [weakThis]( const Track &track ) -> wxRect {
            if (weakThis)
               return weakThis->FindTrackRect( &track );
            return {};
         }
      );
      TrackFocus::Get( *GetProject() ).SetAccessible(
         *this, std::move( pAx ) );
   }

   mTrackArtist = std::make_unique<TrackArtist>( this );

   mTimeCount = 0;
   mTimer.parent = this;
   // Timer is started after the window is visible
   ProjectWindow::Get( *GetProject() ).Bind(wxEVT_IDLE,
      &TrackPanel::OnIdle, this);

   // Register for tracklist updates
   mTracks->Bind(EVT_TRACKLIST_RESIZING,
                    &TrackPanel::OnTrackListResizing,
                    this);
   mTracks->Bind(EVT_TRACKLIST_ADDITION,
                    &TrackPanel::OnTrackListResizing,
                    this);
   mTracks->Bind(EVT_TRACKLIST_DELETION,
                    &TrackPanel::OnTrackListDeletion,
                    this);
   mTracks->Bind(EVT_TRACKLIST_TRACK_REQUEST_VISIBLE,
                    &TrackPanel::OnEnsureVisible,
                    this);

   auto theProject = GetProject();
   theProject->Bind(
      EVT_PROJECT_SETTINGS_CHANGE, &TrackPanel::OnProjectSettingsChange, this);
   theProject->Bind(
      EVT_TRACK_FOCUS_CHANGE, &TrackPanel::OnTrackFocusChange, this );

   theProject->Bind(EVT_UNDO_RESET, &TrackPanel::OnUndoReset, this);

   wxTheApp->Bind(EVT_AUDIOIO_PLAYBACK,
                     &TrackPanel::OnAudioIO,
                     this);
   wxTheApp->Bind(EVT_AUDIOIO_CAPTURE,
                     &TrackPanel::OnAudioIO,
                     this);
   UpdatePrefs();
}


TrackPanel::~TrackPanel()
{
   mTimer.Stop();

   // This can happen if a label is being edited and the user presses
   // ALT+F4 or Command+Q
   if (HasCapture())
      ReleaseMouse();
}

void TrackPanel::UpdatePrefs()
{
   // All vertical rulers must be recalculated since the minimum and maximum
   // frequencies may have been changed.
   UpdateVRulers();

   Refresh();
}

/// Gets the pointer to the AudacityProject that
/// goes with this track panel.
AudacityProject * TrackPanel::GetProject() const
{
   //JKC casting away constness here.
   //Do it in two stages in case 'this' is not a wxWindow.
   //when the compiler will flag the error.
   wxWindow const * const pConstWind = this;
   wxWindow * pWind=(wxWindow*)pConstWind;
#ifdef EXPERIMENTAL_NOTEBOOK
   pWind = pWind->GetParent(); //Page
   wxASSERT( pWind );
   pWind = pWind->GetParent(); //Notebook
   wxASSERT( pWind );
#endif
   pWind = pWind->GetParent(); //MainPanel
   wxASSERT( pWind );
   pWind = pWind->GetParent(); //ProjectWindow
   wxASSERT( pWind );
   return &static_cast<ProjectWindow*>( pWind )->GetProject();
}

void TrackPanel::OnSize( wxSizeEvent &evt )
{
   evt.Skip();
   const auto &size = evt.GetSize();
   mViewInfo->SetWidth( size.GetWidth() );
   mViewInfo->SetHeight( size.GetHeight() );
}

void TrackPanel::OnIdle(wxIdleEvent& event)
{
   event.Skip();
   // The window must be ready when the timer fires (#1401)
   if (IsShownOnScreen())
   {
      mTimer.Start(kTimerInterval, FALSE);

      // Timer is started, we don't need the event anymore
      GetProjectFrame( *GetProject() ).Unbind(wxEVT_IDLE,
         &TrackPanel::OnIdle, this);
   }
   else
   {
      // Get another idle event, wx only guarantees we get one
      // event after "some other normal events occur"
      event.RequestMore();
   }
}

/// AS: This gets called on our wx timer events.
void TrackPanel::OnTimer(wxTimerEvent& )
{
   mTimeCount++;

   AudacityProject *const p = GetProject();
   auto &window = ProjectWindow::Get( *p );

   auto &projectAudioIO = ProjectAudioIO::Get( *p );
   auto gAudioIO = AudioIO::Get();

   // Check whether we were playing or recording, but the stream has stopped.
   if (projectAudioIO.GetAudioIOToken()>0 && !IsAudioActive())
   {
      //the stream may have been started up after this one finished (by some other project)
      //in that case reset the buttons don't stop the stream
      auto &projectAudioManager = ProjectAudioManager::Get( *p );
      projectAudioManager.Stop(!gAudioIO->IsStreamActive());
   }

   // Next, check to see if we were playing or recording
   // audio, but now Audio I/O is completely finished.
   if (projectAudioIO.GetAudioIOToken()>0 &&
         !gAudioIO->IsAudioTokenActive(projectAudioIO.GetAudioIOToken()))
   {
      projectAudioIO.SetAudioIOToken(0);
      window.RedrawProject();
   }
   if (mLastDrawnSelectedRegion != mViewInfo->selectedRegion) {
      UpdateSelectionDisplay();
   }

   // Notify listeners for timer ticks
   {
      wxCommandEvent e(EVT_TRACK_PANEL_TIMER);
      p->ProcessEvent(e);
   }

   DrawOverlays(false);
   mRuler->DrawOverlays(false);

   if(IsAudioActive() && gAudioIO->GetNumCaptureChannels()) {

      // Periodically update the display while recording

      if ((mTimeCount % 5) == 0) {
         // Must tell OnPaint() to recreate the backing bitmap
         // since we've not done a full refresh.
         mRefreshBacking = true;
         Refresh( false );
      }
   }
   if(mTimeCount > 1000)
      mTimeCount = 0;
}

void TrackPanel::OnProjectSettingsChange( wxCommandEvent &event )
{
   event.Skip();
   switch ( static_cast<ProjectSettings::EventCode>( event.GetInt() ) ) {
   case ProjectSettings::ChangedSyncLock:
      Refresh(false);
      break;
   default:
      break;
   }
}

void TrackPanel::OnUndoReset( wxCommandEvent &event )
{
   event.Skip();
   TrackFocus::Get( *GetProject() ).Set( nullptr );
   Refresh( false );
}

/// AS: OnPaint( ) is called during the normal course of
///  completing a repaint operation.
void TrackPanel::OnPaint(wxPaintEvent & /* event */)
{
   mLastDrawnSelectedRegion = mViewInfo->selectedRegion;

#if DEBUG_DRAW_TIMING
   wxStopWatch sw;
#endif

   {
      wxPaintDC dc(this);

      // Retrieve the damage rectangle
      wxRect box = GetUpdateRegion().GetBox();

      // Recreate the backing bitmap if we have a full refresh
      // (See TrackPanel::Refresh())
      if (mRefreshBacking || (box == GetRect()))
      {
         // Reset (should a mutex be used???)
         mRefreshBacking = false;

         // Redraw the backing bitmap
         DrawTracks(&GetBackingDCForRepaint());

         // Copy it to the display
         DisplayBitmap(dc);
      }
      else
      {
         // Copy full, possibly clipped, damage rectangle
         RepairBitmap(dc, box.x, box.y, box.width, box.height);
      }

      // Done with the clipped DC

      // Drawing now goes directly to the client area.
      // DrawOverlays() may need to draw outside the clipped region.
      // (Used to make a NEW, separate wxClientDC, but that risks flashing
      // problems on Mac.)
      dc.DestroyClippingRegion();
      DrawOverlays(true, &dc);
   }

#if DEBUG_DRAW_TIMING
   sw.Pause();
   wxLogDebug(wxT("Total: %ld milliseconds"), sw.Time());
   wxPrintf(wxT("Total: %ld milliseconds\n"), sw.Time());
#endif
}

void TrackPanel::MakeParentRedrawScrollbars()
{
   mListener->TP_RedrawScrollbars();
}

namespace {
   std::shared_ptr<Track> FindTrack(TrackPanelCell *pCell )
   {
      if (pCell)
         return static_cast<CommonTrackPanelCell*>( pCell )->FindTrack();
      return {};
   }
}

void TrackPanel::ProcessUIHandleResult
   (TrackPanelCell *pClickedCell, TrackPanelCell *pLatestCell,
    UIHandle::Result refreshResult)
{
   const auto panel = this;
   auto pLatestTrack = FindTrack( pLatestCell ).get();

   // This precaution checks that the track is not only nonnull, but also
   // really owned by the track list
   auto pClickedTrack = GetTracks()->Lock(
      std::weak_ptr<Track>{ FindTrack( pClickedCell ) }
   ).get();

   // TODO:  make a finer distinction between refreshing the track control area,
   // and the waveform area.  As it is, redraw both whenever you must redraw either.

   // Copy data from the underlying tracks to the pending tracks that are
   // really displayed
   TrackList::Get( *panel->GetProject() ).UpdatePendingTracks();

   using namespace RefreshCode;

   if (refreshResult & DestroyedCell) {
      panel->UpdateViewIfNoTracks();
      // Beware stale pointer!
      if (pLatestTrack == pClickedTrack)
         pLatestTrack = NULL;
      pClickedTrack = NULL;
   }

   if (pClickedTrack && (refreshResult & RefreshCode::UpdateVRuler))
      panel->UpdateVRuler(pClickedTrack);

   if (refreshResult & RefreshCode::DrawOverlays) {
      panel->DrawOverlays(false);
      mRuler->DrawOverlays(false);
   }

   // Refresh all if told to do so, or if told to refresh a track that
   // is not known.
   const bool refreshAll =
      (    (refreshResult & RefreshAll)
       || ((refreshResult & RefreshCell) && !pClickedTrack)
       || ((refreshResult & RefreshLatestCell) && !pLatestTrack));

   if (refreshAll)
      panel->Refresh(false);
   else {
      if (refreshResult & RefreshCell)
         panel->RefreshTrack(pClickedTrack);
      if (refreshResult & RefreshLatestCell)
         panel->RefreshTrack(pLatestTrack);
   }

   if (refreshResult & FixScrollbars)
      panel->MakeParentRedrawScrollbars();

   if (refreshResult & Resize)
      panel->GetListener()->TP_HandleResize();

   if ((refreshResult & RefreshCode::EnsureVisible) && pClickedTrack) {
      TrackFocus::Get(*GetProject()).Set(pClickedTrack);
      pClickedTrack->EnsureVisible();
   }
}

void TrackPanel::HandlePageUpKey()
{
   mListener->TP_ScrollWindow(2 * mViewInfo->h - mViewInfo->GetScreenEndTime());
}

void TrackPanel::HandlePageDownKey()
{
   mListener->TP_ScrollWindow(mViewInfo->GetScreenEndTime());
}

bool TrackPanel::IsAudioActive()
{
   AudacityProject *p = GetProject();
   return ProjectAudioIO::Get( *p ).IsAudioActive();
}

void TrackPanel::UpdateStatusMessage( const TranslatableString &st )
{
   auto status = st;
   if (HasEscape())
      /* i18n-hint Esc is a key on the keyboard */
      status.Join( XO("(Esc to cancel)"), " " );
   ProjectStatus::Get( *GetProject() ).Set( status );
}

void TrackPanel::UpdateSelectionDisplay()
{
   // Full refresh since the label area may need to indicate
   // newly selected tracks.
   Refresh(false);

   // Make sure the ruler follows suit.
   mRuler->DrawSelection();
}

// Counts selected tracks, counting stereo tracks as one track.
size_t TrackPanel::GetSelectedTrackCount() const
{
   return GetTracks()->SelectedLeaders().size();
}

void TrackPanel::UpdateViewIfNoTracks()
{
   if (mTracks->empty())
   {
      // BG: There are no more tracks on screen
      //BG: Set zoom to normal
      mViewInfo->SetZoom(ZoomInfo::GetDefaultZoom());

      //STM: Set selection to 0,0
      //PRL: and default the rest of the selection information
      mViewInfo->selectedRegion = SelectedRegion();

      // PRL:  Following causes the time ruler to align 0 with left edge.
      // Bug 972
      mViewInfo->h = 0;

      mListener->TP_HandleResize();
      //STM: Clear message if all tracks are removed
      ProjectStatus::Get( *GetProject() ).Set({});
   }
}

// The tracks positions within the list have changed, so update the vertical
// ruler size for the track that triggered the event.
void TrackPanel::OnTrackListResizing(TrackListEvent & e)
{
   auto t = e.mpTrack.lock();
   // A deleted track can trigger the event.  In which case do nothing here.
   // A deleted track can have a valid pointer but no owner, bug 2060
   if( t && t->HasOwner() )
      UpdateVRuler(t.get());
   e.Skip();

   // fix for bug 2477
   mListener->TP_RedrawScrollbars();
}

// Tracks have been removed from the list.
void TrackPanel::OnTrackListDeletion(wxEvent & e)
{
   // copy shared_ptr for safety, as in HandleClick
   auto handle = Target();
   if (handle) {
      handle->OnProjectChange(GetProject());
   }

   // If the focused track disappeared but there are still other tracks,
   // this reassigns focus.
   TrackFocus( *GetProject() ).Get();

   UpdateVRulerSize();

   e.Skip();
}

void TrackPanel::OnKeyDown(wxKeyEvent & event)
{
   switch (event.GetKeyCode())
   {
      // Allow PageUp and PageDown keys to
      //scroll the Track Panel left and right
   case WXK_PAGEUP:
      HandlePageUpKey();
      return;

   case WXK_PAGEDOWN:
      HandlePageDownKey();
      return;
      
   default:
      // fall through to base class handler
      event.Skip();
   }
}

void TrackPanel::OnMouseEvent(wxMouseEvent & event)
{
   if (event.LeftDown()) {
      // wxTimers seem to be a little unreliable, so this
      // "primes" it to make sure it keeps going for a while...

      // When this timer fires, we call TrackPanel::OnTimer and
      // possibly update the screen for offscreen scrolling.
      mTimer.Stop();
      mTimer.Start(kTimerInterval, FALSE);
   }


   if (event.ButtonUp()) {
      //EnsureVisible should be called after processing the up-click.
      this->CallAfter( [this, event]{
         const auto foundCell = FindCell(event.m_x, event.m_y);
         const auto t = FindTrack( foundCell.pCell.get() );
         if ( t ) {
            TrackFocus::Get(*GetProject()).Set(t.get());
            t->EnsureVisible();
         }
      } );
   }

   // Must also fall through to base class handler
   event.Skip();
}

double TrackPanel::GetMostRecentXPos()
{
   return mViewInfo->PositionToTime(
      MostRecentXCoord(), mViewInfo->GetLeftOffset());
}

void TrackPanel::RefreshTrack(Track *trk, bool refreshbacking)
{
   if (!trk)
      return;

   // Always move to the first channel of the group, and use only
   // the sum of channel heights, not the height of any channel alone!
   trk = *GetTracks()->FindLeader(trk);
   auto &view = TrackView::Get( *trk );
   auto height =
      TrackList::Channels(trk).sum( TrackView::GetTrackHeight );

   // Set rectangle top according to the scrolling position, `vpos`
   // Subtract the inset (above) and shadow (below) from the height of the
   // rectangle, but not the border
   // This matters because some separators do paint over the border
   const auto top =
      -mViewInfo->vpos + view.GetCumulativeHeightBefore() + kTopInset;
   height -= (kTopInset + kShadowThickness);

   // Width also subtracts insets (left and right) plus shadow (right)
   const auto left = kLeftInset;
   const auto width = GetRect().GetWidth()
      - (kLeftInset + kRightInset + kShadowThickness);

   wxRect rect(left, top, width, height);

   if( refreshbacking )
      mRefreshBacking = true;

   Refresh( false, &rect );
}


/// This method overrides Refresh() of wxWindow so that the
/// boolean play indicator can be set to false, so that an old play indicator that is
/// no longer there won't get  XORed (to erase it), thus redrawing it on the
/// TrackPanel
void TrackPanel::Refresh(bool eraseBackground /* = TRUE */,
                         const wxRect *rect /* = NULL */)
{
   // Tell OnPaint() to refresh the backing bitmap.
   //
   // Originally I had the check within the OnPaint() routine and it
   // was working fine.  That was until I found that, even though a full
   // refresh was requested, Windows only set the onscreen portion of a
   // window as damaged.
   //
   // So, if any part of the trackpanel was off the screen, full refreshes
   // didn't work and the display got corrupted.
   if( !rect || ( *rect == GetRect() ) )
   {
      mRefreshBacking = true;
   }
   wxWindow::Refresh(eraseBackground, rect);

   CallAfter([this]{ CellularPanel::HandleCursorForPresentMouseState(); } );
}

void TrackPanel::OnAudioIO(wxCommandEvent & evt)
{
   evt.Skip();
   // Some hit tests want to change their cursor to and from the ban symbol
   CallAfter( [this]{ CellularPanel::HandleCursorForPresentMouseState(); } );
}

#include "TrackPanelDrawingContext.h"

/// Draw the actual track areas.  We only draw the borders
/// and the little buttons and menues and whatnot here, the
/// actual contents of each track are drawn by the TrackArtist.
void TrackPanel::DrawTracks(wxDC * dc)
{
   wxRegion region = GetUpdateRegion();

   const wxRect clip = GetRect();

   const SelectedRegion &sr = mViewInfo->selectedRegion;
   mTrackArtist->pSelectedRegion = &sr;
   mTrackArtist->pZoomInfo = mViewInfo;
   TrackPanelDrawingContext context {
      *dc, Target(), mLastMouseState, mTrackArtist.get()
   };

   // Don't draw a bottom margin here.

   const auto &settings = ProjectSettings::Get( *GetProject() );
   bool bMultiToolDown =
      (ToolCodes::multiTool == settings.GetTool());
   bool envelopeFlag   =
      bMultiToolDown || (ToolCodes::envelopeTool == settings.GetTool());
   bool bigPointsFlag  =
      bMultiToolDown || (ToolCodes::drawTool == settings.GetTool());
   bool sliderFlag     = bMultiToolDown;
   bool brushFlag   = false;
#ifdef EXPERIMENTAL_BRUSH_TOOL
   brushFlag   = (ToolCodes::brushTool == settings.GetTool());
#endif

   const bool hasSolo = GetTracks()->Any< PlayableTrack >()
      .any_of( []( const PlayableTrack *pt ) {
         pt = static_cast< const PlayableTrack * >(
            pt->SubstitutePendingChangedTrack().get() );
         return (pt && pt->GetSolo());
      } );

   mTrackArtist->drawEnvelope = envelopeFlag;
   mTrackArtist->bigPoints = bigPointsFlag;
   mTrackArtist->drawSliders = sliderFlag;
   mTrackArtist->onBrushTool = brushFlag;
   mTrackArtist->hasSolo = hasSolo;

   this->CellularPanel::Draw( context, TrackArtist::NPasses );
}

void TrackPanel::SetBackgroundCell
(const std::shared_ptr< CommonTrackPanelCell > &pCell)
{
   mpBackground = pCell;
}

std::shared_ptr< CommonTrackPanelCell > TrackPanel::GetBackgroundCell()
{
   return mpBackground;
}

namespace {
std::vector<int> FindAdjustedChannelHeights( Track &t )
{
   auto channels = TrackList::Channels(&t);
   wxASSERT(!channels.empty());

   // Collect heights, and count affordances
   int nAffordances = 0;
   int totalHeight = 0;
   std::vector<int> oldHeights;
   for (auto channel : channels) {
      auto &view = TrackView::Get( *channel );
      const auto height = view.GetHeight();
      totalHeight += height;
      oldHeights.push_back( height );
      if (view.GetAffordanceControls())
         ++nAffordances;
   }

   // Allocate results
   auto nChannels = static_cast<int>(oldHeights.size());
   std::vector<int> results;
   results.reserve(nChannels);

   // Now reallocate the channel heights for the presence of affordances
   // and separators
   auto availableHeight = totalHeight
      - nAffordances * kAffordancesAreaHeight
      - (nChannels - 1) * kChannelSeparatorThickness
      - kTrackSeparatorThickness;
   int cumulativeOldHeight = 0;
   int cumulativeNewHeight = 0;
   for (const auto &oldHeight : oldHeights) {
      // Preserve the porportions among the stored heights
      cumulativeOldHeight += oldHeight;
      const auto newHeight =
         cumulativeOldHeight * availableHeight / totalHeight
            - cumulativeNewHeight;
      cumulativeNewHeight += newHeight;
      results.push_back(newHeight);
   }

   return results;
}
}

void TrackPanel::UpdateVRulers()
{
   for (auto t : GetTracks()->Any< WaveTrack >())
      UpdateTrackVRuler(t);

   UpdateVRulerSize();
}

void TrackPanel::UpdateVRuler(Track *t)
{
   if (t)
      UpdateTrackVRuler(t);

   UpdateVRulerSize();
}

void TrackPanel::UpdateTrackVRuler(Track *t)
{
   wxASSERT(t);
   if (!t)
      return;

   auto heights = FindAdjustedChannelHeights(*t);

   wxRect rect(mViewInfo->GetVRulerOffset(),
            0,
            mViewInfo->GetVRulerWidth(),
            0);

   auto pHeight = heights.begin();
   for (auto channel : TrackList::Channels(t)) {
      auto &view = TrackView::Get( *channel );
      const auto height = *pHeight++;
      rect.SetHeight( height );
      const auto subViews = view.GetSubViews( rect );
      if (subViews.empty())
         continue;
   
      auto iter = subViews.begin(), end = subViews.end(), next = iter;
      auto yy = iter->first;
      wxSize vRulerSize{ 0, 0 };
      for ( ; iter != end; iter = next ) {
         ++next;
         auto nextY = ( next == end )
            ? height
            : next->first;
         rect.SetHeight( nextY - yy );
         // This causes ruler size in the track to be reassigned:
         TrackVRulerControls::Get( *iter->second ).UpdateRuler( rect );
         // But we want to know the maximum width and height over all sub-views:
         vRulerSize.IncTo( t->vrulerSize );
         yy = nextY;
      }
      t->vrulerSize = vRulerSize;
   }
}

void TrackPanel::UpdateVRulerSize()
{
   auto trackRange = GetTracks()->Any();
   if (trackRange) {
      wxSize s { 0, 0 };
      for (auto t : trackRange)
         s.IncTo(t->vrulerSize);

      if (mViewInfo->GetVRulerWidth() != s.GetWidth()) {
         mViewInfo->SetVRulerWidth( s.GetWidth() );
         mRuler->SetLeftOffset(
            mViewInfo->GetLeftOffset());  // bevel on AdornedRuler
         mRuler->Refresh();
      }
   }
   Refresh(false);
}

void TrackPanel::OnTrackMenu(Track *t)
{
   CellularPanel::DoContextMenu( t ? &TrackView::Get( *t ) : nullptr );
}

// Tracks have been removed from the list.
void TrackPanel::OnEnsureVisible(TrackListEvent & e)
{
   e.Skip();
   bool modifyState = e.GetInt();

   auto pTrack = e.mpTrack.lock();
   auto t = pTrack.get();

   int trackTop = 0;
   int trackHeight =0;

   for (auto it : GetTracks()->Leaders()) {
      trackTop += trackHeight;

      auto channels = TrackList::Channels(it);
      trackHeight = channels.sum( TrackView::GetTrackHeight );

      //We have found the track we want to ensure is visible.
      if (channels.contains(t)) {

         //Get the size of the trackpanel.
         int width, height;
         GetSize(&width, &height);

         if (trackTop < mViewInfo->vpos) {
            height = mViewInfo->vpos - trackTop + mViewInfo->scrollStep;
            height /= mViewInfo->scrollStep;
            mListener->TP_ScrollUpDown(-height);
         }
         else if (trackTop + trackHeight > mViewInfo->vpos + height) {
            height = (trackTop + trackHeight) - (mViewInfo->vpos + height);
            height = (height + mViewInfo->scrollStep + 1) / mViewInfo->scrollStep;
            mListener->TP_ScrollUpDown(height);
         }

         break;
      }
   }
   Refresh(false);

   if ( modifyState )
      ProjectHistory::Get( *GetProject() ).ModifyState( false );
}

// 0.0 scrolls to top
// 1.0 scrolls to bottom.
void TrackPanel::VerticalScroll( float fracPosition){

   int trackTop = 0;
   int trackHeight = 0;

   auto tracks = GetTracks();

   auto range = tracks->Leaders();
   if (!range.empty()) {
      trackHeight = TrackView::GetChannelGroupHeight( *range.rbegin() );
      --range.second;
   }
   trackTop = range.sum( TrackView::GetChannelGroupHeight );

   int delta;
   
   //Get the size of the trackpanel.
   int width, height;
   GetSize(&width, &height);

   delta = (fracPosition * (trackTop + trackHeight - height)) - mViewInfo->vpos + mViewInfo->scrollStep;
   //wxLogDebug( "Scroll down by %i pixels", delta );
   delta /= mViewInfo->scrollStep;
   mListener->TP_ScrollUpDown(delta);
   Refresh(false);
}


namespace {
   // Drawing constants
   // DisplaceX and MarginX are large enough to avoid overwriting <- symbol
   // See TrackArt::DrawNegativeOffsetTrackArrows
   enum : int {
      // Displacement of the rectangle from upper left corner
      DisplaceX = 7, DisplaceY = 1,
      // Size of margins about the text extent that determine the rectangle size
      MarginX = 8, MarginY = 2,
      // Derived constants
      MarginsX = 2 * MarginX, MarginsY = 2 * MarginY,
   };

void GetTrackNameExtent(
   wxDC &dc, const Track *t, wxCoord *pW, wxCoord *pH )
{
   wxFont labelFont(12, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL);
   dc.SetFont(labelFont);
   dc.GetTextExtent( t->GetName(), pW, pH );
}

wxRect GetTrackNameRect(
   int leftOffset,
   const wxRect &trackRect, wxCoord textWidth, wxCoord textHeight )
{
   return {
      leftOffset + DisplaceX,
      trackRect.y + DisplaceY,
      textWidth + MarginsX,
      textHeight + MarginsY
   };
}

// Draws the track name on the track, if it is needed.
void DrawTrackName(
   int leftOffset,
   TrackPanelDrawingContext &context, const Track * t, const wxRect & rect )
{
   if( !TrackArtist::Get( context )->mbShowTrackNameInTrack )
      return;
   auto name = t->GetName();
   if( name.IsEmpty())
      return;
   if( !t->IsLeader())
      return;
   auto &dc = context.dc;
   wxBrush Brush;
   wxCoord textWidth, textHeight;
   GetTrackNameExtent( dc, t, &textWidth, &textHeight );

   // Logic for name background translucency (aka 'shields')
   // Tracks less than kOpaqueHeight high will have opaque shields.
   // Tracks more than kTranslucentHeight will have maximum translucency for shields.
   const int kOpaqueHeight = 44;
   const int kTranslucentHeight = 124;

   // PRL:  to do:  reexamine this strange use of TrackView::GetHeight,
   // ultimately to compute an opacity
   int h = TrackView::Get( *t ).GetHeight();

   // f codes the opacity as a number between 0.0 and 1.0
   float f = wxClip((h-kOpaqueHeight)/(float)(kTranslucentHeight-kOpaqueHeight),0.0,1.0);
   // kOpaque is the shield's alpha for tracks that are not tall
   // kTranslucent is the shield's alpha for tracks that are tall.
   const int kOpaque = 255;
   const int kTranslucent = 140;
   // 0.0 maps to full opacity, 1.0 maps to full translucency.
   int opacity = 255 - (255-140)*f;

   const auto nameRect =
      GetTrackNameRect( leftOffset, rect, textWidth, textHeight );

#ifdef __WXMAC__
   // Mac dc is a graphics dc already.
   AColor::UseThemeColour( &dc, clrTrackInfoSelected, clrTrackPanelText, opacity );
   dc.DrawRoundedRectangle( nameRect, 8.0 );
#else
   // This little dance with wxImage in order to draw to a graphic dc
   // which we can then paste as a translucent bitmap onto the real dc.
   enum : int {
      SecondMarginX = 1, SecondMarginY = 1,
      SecondMarginsX = 2 * SecondMarginX, SecondMarginsY = 2 * SecondMarginY,
   };
   wxImage image(
      textWidth + MarginsX + SecondMarginsX,
      textHeight + MarginsY + SecondMarginsY );
   image.InitAlpha();
   unsigned char *alpha=image.GetAlpha();
   memset(alpha, wxIMAGE_ALPHA_TRANSPARENT, image.GetWidth()*image.GetHeight());

   {
      std::unique_ptr< wxGraphicsContext >
         pGc{ wxGraphicsContext::Create(image) };
      auto &gc = *pGc;
      // This is to a gc, not a dc.
      AColor::UseThemeColour( &gc, clrTrackInfoSelected, clrTrackPanelText, opacity );
      // Draw at 1,1, not at 0,0 to avoid clipping of the antialiasing.
      gc.DrawRoundedRectangle(
         SecondMarginX, SecondMarginY,
         textWidth + MarginsX, textHeight + MarginsY, 8.0 );
      // destructor of gc updates the wxImage.
   }
   wxBitmap bitmap( image );
   dc.DrawBitmap( bitmap,
      nameRect.x - SecondMarginX, nameRect.y - SecondMarginY );
#endif
   dc.SetTextForeground(theTheme.Colour( clrTrackPanelText ));
   dc.DrawText(t->GetName(),
      nameRect.x + MarginX,
      nameRect.y + MarginY);
}

/*

  The following classes define the subdivision of the area of the TrackPanel
  into cells with differing responses to mouse, keyboard, and scroll wheel
  events.
  
  The classes defining the less inclusive areas are earlier, while those
  defining ever larger hierarchical groupings of cells are later.

  To describe that subdivision again, informally, and top-down:

  Firstly subtract margin areas, on the left and right, that do not interact.

  Secondly subtract a noninterative margin above the first track, and an area
  below all tracks that causes deselection of all tracks if you click it.
  (One or both of those areas might be vertically scrolled off-screen, though.)
  Divide what remains into areas corresponding to the several tracks.

  Thirdly, for each track, subtract an area below, which you can click and drag
  to resize the track vertically.

  Fourthly, subtract an area at the left, which contains the track controls,
  such as the menu and delete and minimize buttons, and others appropriate
  to the track subtype.

  Fifthly, divide what remains into the vertically stacked channels, if there
  are more than one, alternating with separators, which can be clicked to
  resize the channel views.

  Sixthly, divide each channel into one or more vertically stacked sub-views.
  
  Lastly, split the area for each sub-view into a vertical ruler, and an area
  that displays the channel's own contents.

*/

struct EmptyCell final : CommonTrackPanelCell {
   std::vector< UIHandlePtr > HitTest(
      const TrackPanelMouseState &, const AudacityProject *) override
   { return {}; }
   virtual std::shared_ptr< Track > DoFindTrack() override { return {}; }
   static std::shared_ptr<EmptyCell> Instance()
   {
      static auto instance = std::make_shared< EmptyCell >();
      return instance;
   }

   // TrackPanelDrawable implementation
   void Draw(
      TrackPanelDrawingContext &context,
      const wxRect &rect, unsigned iPass ) override
   {
      if ( iPass == TrackArtist::PassMargins ) {
         // Draw a margin area of TrackPanel
         auto dc = &context.dc;
         
         AColor::TrackPanelBackground( dc, false );
         dc->DrawRectangle( rect );
      }
   }
};

// A vertical ruler left of a channel
struct VRulerAndChannel final : TrackPanelGroup {
   VRulerAndChannel(
      const std::shared_ptr< TrackView > &pView, wxCoord leftOffset )
         : mpView{ pView }, mLeftOffset{ leftOffset } {}
   Subdivision Children( const wxRect &rect ) override
   {
      return { Axis::X, Refinement{
         { rect.GetLeft(),
           TrackVRulerControls::Get( *mpView ).shared_from_this() },
         { mLeftOffset, mpView }
      } };
   }
   std::shared_ptr< TrackView > mpView;
   wxCoord mLeftOffset;
};

// One or more sub-views of one channel, stacked vertically, each containing
// a vertical ruler and a channel
struct VRulersAndChannels final : TrackPanelGroup {
   VRulersAndChannels(
      const std::shared_ptr<Track> &pTrack,
      TrackView::Refinement refinement, wxCoord leftOffset )
         : mpTrack{ pTrack }
         , mRefinement{ std::move( refinement ) }
         , mLeftOffset{ leftOffset } {}
   Subdivision Children( const wxRect &rect ) override
   {
      Refinement refinement;
      auto y1 = rect.GetTop();
      for ( const auto &subView : mRefinement ) {
         y1 = std::max( y1, subView.first );
         refinement.emplace_back( y1,
            std::make_shared< VRulerAndChannel >(
               subView.second, mLeftOffset ) );
      }
      return { Axis::Y, std::move( refinement ) };
   }

   // TrackPanelDrawable implementation
   void Draw(
      TrackPanelDrawingContext &context,
      const wxRect &rect, unsigned iPass ) override
   {
      // This overpaints the track area, but sometimes too the stereo channel
      // separator, so draw at least later than that
      if ( iPass == TrackArtist::PassBorders ) {
         DrawTrackName( mLeftOffset,
            context, mpTrack->SubstitutePendingChangedTrack().get(), rect );
      }
      if ( iPass == TrackArtist::PassControls ) {
         if (mRefinement.size() > 1) {
            // Draw lines separating sub-views
            auto &dc = context.dc;
            AColor::CursorColor( &dc );
            auto iter = mRefinement.begin() + 1, end = mRefinement.end();
            for ( ; iter != end; ++iter ) {
               auto yy = iter->first;
               AColor::Line( dc, mLeftOffset, yy, rect.GetRight(), yy );
            }
         }
      }
   }

   wxRect DrawingArea(
      TrackPanelDrawingContext &context,
      const wxRect &rect, const wxRect &panelRect, unsigned iPass ) override
   {
      auto result = rect;
      if ( iPass == TrackArtist::PassBorders ) {
         if ( true ) {
            wxCoord textWidth, textHeight;
            GetTrackNameExtent( context.dc, mpTrack.get(),
               &textWidth, &textHeight );
            result =
               GetTrackNameRect( mLeftOffset, rect, textWidth, textHeight );
         }
      }
      return result;
   }

   std::shared_ptr< Track > mpTrack;
   TrackView::Refinement mRefinement;
   wxCoord mLeftOffset;
};

//Simply fills area using specified brush and outlines borders
class EmptyPanelRect final : public CommonTrackPanelCell
{
   //Required to keep selection behaviour similar to others
   std::shared_ptr<Track> mTrack;
   int mFillBrushName;
public:
   explicit EmptyPanelRect(const std::shared_ptr<Track>& track, int fillBrushName)
      : mTrack(track), mFillBrushName(fillBrushName)
   {
   }

   ~EmptyPanelRect() { }

   void Draw(TrackPanelDrawingContext& context,
      const wxRect& rect, unsigned iPass) override
   {
      if (iPass == TrackArtist::PassBackground)
      {
         context.dc.SetPen(*wxTRANSPARENT_PEN);
         AColor::UseThemeColour(&context.dc, mFillBrushName);
         context.dc.DrawRectangle(rect);
         wxRect bevel(rect.x, rect.y, rect.width - 1, rect.height - 1);
         AColor::BevelTrackInfo(context.dc, true, bevel, false);
      }
   }

   std::shared_ptr<Track> DoFindTrack() override
   {
       return mTrack;
   }

   std::vector<UIHandlePtr> HitTest(const TrackPanelMouseState& state,
      const AudacityProject* pProject) override
   {
      return {};
   }
};

//Simply place children one after another horizontally, without any specific logic
struct HorizontalGroup final : TrackPanelGroup {

   Refinement mRefinement;

   HorizontalGroup(Refinement refinement) 
      : mRefinement(std::move(refinement)) 
   { 
   }

   Subdivision Children(const wxRect& /*rect*/) override
   {
      return { Axis::X, mRefinement };
   }

};


// optional affordance areas, and n channels with vertical rulers,
// alternating with n - 1 resizers;
// each channel-ruler pair might be divided into multiple views
struct ChannelGroup final : TrackPanelGroup {
   ChannelGroup( const std::shared_ptr< Track > &pTrack, wxCoord leftOffset )
      : mpTrack{ pTrack }, mLeftOffset{ leftOffset } {}
   Subdivision Children( const wxRect &rect_ ) override
   {
      auto rect = rect_;
      Refinement refinement;

      const auto channels = TrackList::Channels( mpTrack.get() );
      const auto pLast = *channels.rbegin();
      wxCoord yy = rect.GetTop();
      auto heights = FindAdjustedChannelHeights(*mpTrack);
      auto pHeight = heights.begin();
      for ( auto channel : channels )
      {
         auto &view = TrackView::Get( *channel );
         if (auto affordance = view.GetAffordanceControls())
         {
            auto panelRect = std::make_shared<EmptyPanelRect>(
               channel->shared_from_this(),
               channel->GetSelected() ? clrTrackInfoSelected : clrTrackInfo);
            Refinement hgroup {
               std::make_pair(rect.GetLeft() + 1, panelRect),
               std::make_pair(mLeftOffset, affordance)
            };
            refinement.emplace_back(yy, std::make_shared<HorizontalGroup>(hgroup));
            yy += kAffordancesAreaHeight;
         }

         auto height = *pHeight++;
         rect.SetTop( yy );
         rect.SetHeight( height - kChannelSeparatorThickness );
         refinement.emplace_back( yy,
            std::make_shared< VRulersAndChannels >(
               channel->shared_from_this(),
               TrackView::Get( *channel ).GetSubViews( rect ),
               mLeftOffset ) );
         if ( channel != pLast ) {
            yy += height;
            refinement.emplace_back(
               yy - kChannelSeparatorThickness,
               TrackPanelResizerCell::Get( *channel ).shared_from_this() );
         }
      }

      return { Axis::Y, std::move( refinement ) };
   }

   void Draw(TrackPanelDrawingContext& context, const wxRect& rect, unsigned iPass) override
   {
      TrackPanelGroup::Draw(context, rect, iPass);
      if (iPass == TrackArtist::PassFocus && mpTrack->IsSelected())
      {
         const auto channels = TrackList::Channels(mpTrack.get());
         const auto pLast = *channels.rbegin();
         wxCoord yy = rect.GetTop();
         auto heights = FindAdjustedChannelHeights(*mpTrack);
         auto pHeight = heights.begin();
         for (auto channel : channels)
         {
            auto& view = TrackView::Get(*channel);
            auto height = *pHeight++;
            if (auto affordance = view.GetAffordanceControls())
            {
               height += kAffordancesAreaHeight;
            }
            auto trackRect = wxRect(
               mLeftOffset,
               yy,
               rect.GetRight() - mLeftOffset,
               height - kChannelSeparatorThickness);
            TrackArt::DrawCursor(context, trackRect, mpTrack.get());
            yy += height;
         }
      }
   }

   std::shared_ptr< Track > mpTrack;
   wxCoord mLeftOffset;
};

// A track control panel, left of n vertical rulers and n channels
// alternating with n - 1 resizers
struct LabeledChannelGroup final : TrackPanelGroup {
   LabeledChannelGroup(
      const std::shared_ptr< Track > &pTrack, wxCoord leftOffset )
         : mpTrack{ pTrack }, mLeftOffset{ leftOffset } {}
   Subdivision Children( const wxRect &rect ) override
   { return { Axis::X, Refinement{
      { rect.GetLeft(),
         TrackControls::Get( *mpTrack ).shared_from_this() },
      { rect.GetLeft() + kTrackInfoWidth,
        std::make_shared< ChannelGroup >( mpTrack, mLeftOffset ) }
   } }; }

   // TrackPanelDrawable implementation
   void Draw( TrackPanelDrawingContext &context,
      const wxRect &rect, unsigned iPass ) override
   {
      if ( iPass == TrackArtist::PassBorders ) {
         auto &dc = context.dc;
         dc.SetBrush(*wxTRANSPARENT_BRUSH);
         dc.SetPen(*wxBLACK_PEN);

         // border
         dc.DrawRectangle(
            rect.x, rect.y,
            rect.width - kShadowThickness, rect.height - kShadowThickness
         );

         // shadow
         // Stroke lines along bottom and right, which are slightly short at
         // bottom-left and top-right
         const auto right = rect.GetRight();
         const auto bottom = rect.GetBottom();

         // bottom
         AColor::Line(dc, rect.x + 2, bottom, right, bottom);
         // right
         AColor::Line(dc, right, rect.y + 2, right, bottom);
      }
      if ( iPass == TrackArtist::PassFocus ) {
         // Sometimes highlight is not drawn on backing bitmap. I thought
         // it was because FindFocus did not return the TrackPanel on Mac, but
         // when I removed that test, yielding this condition:
         //     if (GetFocusedTrack() != NULL) {
         // the highlight was reportedly drawn even when something else
         // was the focus and no highlight should be drawn. -RBD
         const auto artist = TrackArtist::Get( context );
         auto &trackPanel = *artist->parent;
         auto &trackFocus = TrackFocus::Get( *trackPanel.GetProject() );
         if (trackFocus.Get() == mpTrack.get() &&
             wxWindow::FindFocus() == &trackPanel ) {
            /// Draw a three-level highlight gradient around the focused track.
            wxRect theRect = rect;
            auto &dc = context.dc;
            dc.SetBrush(*wxTRANSPARENT_BRUSH);
            
            AColor::TrackFocusPen( &dc, 2 );
            dc.DrawRectangle(theRect);
            theRect.Deflate(1);
            
            AColor::TrackFocusPen( &dc, 1 );
            dc.DrawRectangle(theRect);
            theRect.Deflate(1);
            
            AColor::TrackFocusPen( &dc, 0 );
            dc.DrawRectangle(theRect);
         }
      }
   }

   wxRect DrawingArea(
      TrackPanelDrawingContext &,
      const wxRect &rect, const wxRect &, unsigned iPass ) override
   {
      if ( iPass == TrackArtist::PassBorders )
         return {
            rect.x - kBorderThickness,
            rect.y - kBorderThickness,
            rect.width + 2 * kBorderThickness + kShadowThickness,
            rect.height + 2 * kBorderThickness + kShadowThickness
         };
      else if ( iPass == TrackArtist::PassFocus ) {
         constexpr auto extra = kBorderThickness + 3;
         return {
            rect.x - extra,
            rect.y - extra,
            rect.width + 2 * extra + kShadowThickness,
            rect.height + 2 * extra + kShadowThickness
         };
      }
      else
         return rect;
   }

   std::shared_ptr< Track > mpTrack;
   wxCoord mLeftOffset;
};

// Stacks a label and a single or multi-channel track on a resizer below,
// which is associated with the last channel
struct ResizingChannelGroup final : TrackPanelGroup {
   ResizingChannelGroup(
      const std::shared_ptr< Track > &pTrack, wxCoord leftOffset )
         : mpTrack{ pTrack }, mLeftOffset{ leftOffset } {}
   Subdivision Children( const wxRect &rect ) override
   { return { Axis::Y, Refinement{
      { rect.GetTop(),
         std::make_shared< LabeledChannelGroup >( mpTrack, mLeftOffset ) },
      { rect.GetTop() + rect.GetHeight() - kTrackSeparatorThickness,
         TrackPanelResizerCell::Get(
            **TrackList::Channels( mpTrack.get() ).rbegin() ).shared_from_this()
      }
   } }; }
   std::shared_ptr< Track > mpTrack;
   wxCoord mLeftOffset;
};

// Stacks a dead area at top, the tracks, and the click-to-deselect area below
struct Subgroup final : TrackPanelGroup {
   explicit Subgroup( TrackPanel &panel ) : mPanel{ panel } {}
   Subdivision Children( const wxRect &rect ) override
   {
      const auto &viewInfo = *mPanel.GetViewInfo();
      wxCoord yy = -viewInfo.vpos;
      Refinement refinement;

      auto &tracks = *mPanel.GetTracks();
      if ( tracks.Any() )
         refinement.emplace_back( yy, EmptyCell::Instance() ),
         yy += kTopMargin;

      for ( const auto leader : tracks.Leaders() ) {
         wxCoord height = 0;
         for ( auto channel : TrackList::Channels( leader ) ) {
            auto &view = TrackView::Get( *channel );
            height += view.GetHeight();
         }
         refinement.emplace_back( yy,
            std::make_shared< ResizingChannelGroup >(
               leader->SharedPointer(), viewInfo.GetLeftOffset() )
         );
         yy += height;
      }

      refinement.emplace_back( std::max( 0, yy ), mPanel.GetBackgroundCell() );

      return { Axis::Y, std::move( refinement ) };
   }
   TrackPanel &mPanel;
};

// Main group shaves off the left and right margins
struct MainGroup final : TrackPanelGroup {
   explicit MainGroup( TrackPanel &panel ) : mPanel{ panel } {}
   Subdivision Children( const wxRect &rect ) override
   { return { Axis::X, Refinement{
      { 0, EmptyCell::Instance() },
      { kLeftMargin, std::make_shared< Subgroup >( mPanel ) },
      { rect.GetRight() + 1 - kRightMargin, EmptyCell::Instance() }
   } }; }
   TrackPanel &mPanel;
};

}

std::shared_ptr<TrackPanelNode> TrackPanel::Root()
{
   // Root and other subgroup objects are throwaways.
   // They might instead be cached to avoid repeated allocation.
   // That cache would need invalidation when there is addition, deletion, or
   // permutation of tracks, or change of width of the vertical rulers.
   return std::make_shared< MainGroup >( *this );
}

// This finds the rectangle of a given track (including all channels),
// either that of the label 'adornment' or the track itself
// The given track is assumed to be the first channel
wxRect TrackPanel::FindTrackRect( const Track * target )
{
   auto leader = *GetTracks()->FindLeader( target );
   if (!leader) {
      return {};
   }

   return CellularPanel::FindRect( [&] ( TrackPanelNode &node ) {
      if (auto pGroup = dynamic_cast<const LabeledChannelGroup*>( &node ))
         return pGroup->mpTrack.get() == leader;
      return false;
   } );
}

wxRect TrackPanel::FindFocusedTrackRect( const Track * target )
{
   auto rect = FindTrackRect(target);
   if (rect != wxRect{}) {
      // Enlarge horizontally.
      // PRL:  perhaps it's one pixel too much each side, including some gray
      // beyond the yellow?
      rect.x = 0;
      GetClientSize(&rect.width, nullptr);

      // Enlarge vertically, enough to enclose the yellow focus border pixels
      // The the outermost ring of gray pixels is included on three sides
      // but not the top (should that be fixed?)

      // (Note that TrackPanel paints its focus over the "top margin" of the
      // rectangle allotted to the track, according to TrackView::GetY() and
      // TrackView::GetHeight(), but also over the margin of the next track.)

      rect.height += kBottomMargin;
      int dy = kTopMargin - 1;
      rect.Inflate( 0, dy );

      // Note that this rectangle does not coincide with any one of
      // the nodes in the subdivision.
   }
   return rect;
}

std::vector<wxRect> TrackPanel::FindRulerRects( const Track *target )
{
   std::vector<wxRect> results;
   if (target)
      VisitCells( [&]( const wxRect &rect, TrackPanelCell &visited ) {
         if (auto pRuler = dynamic_cast<const TrackVRulerControls*>(&visited);
             pRuler && pRuler->FindTrack().get() == target)
            results.push_back(rect);
      } );
   return results;
}

TrackPanelCell *TrackPanel::GetFocusedCell()
{
   auto pTrack = TrackFocus::Get( *GetProject() ).Get();
   return pTrack ? &TrackView::Get( *pTrack ) : GetBackgroundCell().get();
}

void TrackPanel::SetFocusedCell()
{
   // This may have a side-effect of assigning a focus if there was none
   auto& trackFocus = TrackFocus::Get(*GetProject());
   trackFocus.Set(trackFocus.Get());
   KeyboardCapture::Capture(this);
}

void TrackPanel::OnTrackFocusChange( wxCommandEvent &event )
{
   event.Skip();
   auto cell = GetFocusedCell();

   if (cell) {
      Refresh( false );
   }
}
