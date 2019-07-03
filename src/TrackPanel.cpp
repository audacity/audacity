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

  Note that in some of the older code here, e.g., GetLabelWidth(),
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

#include "Audacity.h" // for USE_* macros
#include "TrackPanel.h"

#include "Experimental.h"

#include <wx/setup.h> // for wxUSE_* macros

#include "AdornedRulerPanel.h"
#include "KeyboardCapture.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "ProjectHistory.h"
#include "ProjectSettings.h"
#include "ProjectStatus.h"
#include "ProjectWindow.h"
#include "TrackPanelMouseEvent.h"
#include "TrackPanelResizeHandle.h"
//#define DEBUG_DRAW_TIMING 1

#include "UndoManager.h"

#include "AColor.h"
#include "AudioIO.h"
#include "float_cast.h"

#include "Prefs.h"
#include "RefreshCode.h"
#include "TrackArtist.h"
#include "TrackPanelAx.h"
#include "WaveTrack.h"
#ifdef EXPERIMENTAL_MIDI_OUT
#include "NoteTrack.h"
#endif

#include "ondemand/ODManager.h"
#include "ondemand/ODTask.h"

#include "toolbars/ControlToolBar.h"

#include "tracks/ui/TrackControls.h"
#include "tracks/ui/TrackView.h"
#include "tracks/ui/TrackVRulerControls.h"

//This loads the appropriate set of cursors, depending on platform.
#include "../images/Cursors.h"

#include <algorithm>

#include <wx/dcclient.h>

/**

\class TrackPanel

This is a diagram of TrackPanel's division of one (non-stereo) track rectangle.
Total height equals TrackView::GetHeight()'s value.  Total width is the wxWindow's
width.  Each charater that is not . represents one pixel.

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

GetLabelWidth() counts columns up to and including the VRuler.
GetLeftOffset() is yet one more -- it counts the "one pixel" column.

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

AudacityProject::AttachedWindows::RegisteredFactory sKey{
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
      project.SetPanel( result );
      return result;
   }
};

}

TrackPanel &TrackPanel::Get( AudacityProject &project )
{
   return project.AttachedWindows::Get< TrackPanel >( sKey );
}

const TrackPanel &TrackPanel::Get( const AudacityProject &project )
{
   return Get( const_cast< AudacityProject & >( project ) );
}

void TrackPanel::Destroy( AudacityProject &project )
{
   auto *pPanel = project.AttachedWindows::Find( sKey );
   if (pPanel) {
      pPanel->wxWindow::Destroy();
      project.AttachedWindows::Assign( sKey, nullptr );
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
   SetLabel(_("Track Panel"));
   SetName(_("Track Panel"));
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
#if wxUSE_ACCESSIBILITY
      // wxWidgets owns the accessible object
      SetAccessible(mAx = pAx.release());
#else
      // wxWidgets does not own the object, but we need to retain it
      mAx = std::move(pAx);
#endif
   }

   mRedrawAfterStop = false;

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
   theProject->Bind(EVT_ODTASK_UPDATE, &TrackPanel::OnODTask, this);
   theProject->Bind(EVT_ODTASK_COMPLETE, &TrackPanel::OnODTask, this);
   theProject->Bind(
      EVT_PROJECT_SETTINGS_CHANGE, &TrackPanel::OnProjectSettingsChange, this);
   theProject->Bind(
      EVT_TRACK_FOCUS_CHANGE, &TrackPanel::OnTrackFocusChange, this );

   theProject->Bind(EVT_UNDO_RESET, &TrackPanel::OnUndoReset, this);

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
   // frequences may have been changed.
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
#ifdef __WXMAC__
   // Unfortunate part of fix for bug 1431
   // Without this, the toolbars hide only every other time that you press
   // the yellow title bar button.  For some reason, not every press sends
   // us a deactivate event for the application.
   {
      auto project = GetProject();
      auto &window = ProjectWindow::Get( *project );
      if (window.IsIconized())
         window.MacShowUndockedToolbars(false);
   }
#endif

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
      auto &bar = ControlToolBar::Get( *p );
      bar.StopPlaying(!gAudioIO->IsStreamActive());
   }

   // Next, check to see if we were playing or recording
   // audio, but now Audio I/O is completely finished.
   if (projectAudioIO.GetAudioIOToken()>0 &&
         !gAudioIO->IsAudioTokenActive(projectAudioIO.GetAudioIOToken()))
   {
      projectAudioIO.SetAudioIOToken(0);
      window.RedrawProject();

      mRedrawAfterStop = false;

      //ANSWER-ME: Was DisplaySelection added to solve a repaint problem?
      DisplaySelection();
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

      if (!mRedrawAfterStop) {
         mRedrawAfterStop = true;
         MakeParentRedrawScrollbars();
         mListener->TP_ScrollUpDown( 99999999 );
         Refresh( false );
      }
      else {
         if ((mTimeCount % 5) == 0) {
            // Must tell OnPaint() to recreate the backing bitmap
            // since we've not done a full refresh.
            mRefreshBacking = true;
            Refresh( false );
         }
      }
   }
   if(mTimeCount > 1000)
      mTimeCount = 0;
}

///Handles the redrawing necessary for tasks as they partially update in the
///background, or finish.
void TrackPanel::OnODTask(wxCommandEvent & WXUNUSED(event))
{
   //todo: add track data to the event - check to see if the project contains it before redrawing.
   Refresh(false);
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
   SetFocusedTrack( nullptr );
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

void TrackPanel::MakeParentModifyState(bool bWantsAutoSave)
{
   ProjectHistory::Get( *GetProject() ).ModifyState(bWantsAutoSave);
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

   // This flag is superfluous if you do full refresh,
   // because TrackPanel::Refresh() does this too
   if (refreshResult & UpdateSelection) {
      panel->DisplaySelection();

      {
         // Formerly in TrackPanel::UpdateSelectionDisplay():

         // Make sure the ruler follows suit.
         // mRuler->DrawSelection();

         // ... but that too is superfluous it does nothing but refresh
         // the ruler, while DisplaySelection calls TP_DisplaySelection which
         // also always refreshes the ruler.
      }
   }

   if ((refreshResult & RefreshCode::EnsureVisible) && pClickedTrack)
      pClickedTrack->EnsureVisible();
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

void TrackPanel::UpdateStatusMessage( const wxString &st )
{
   auto status = st;
   if (HasEscape())
   /* i18n-hint Esc is a key on the keyboard */
      status += wxT(" "), status += _("(Esc to cancel)");
   ProjectStatus::Get( *GetProject() ).Set( status );
}

void TrackPanel::UpdateSelectionDisplay()
{
   // Full refresh since the label area may need to indicate
   // newly selected tracks.
   Refresh(false);

   // Make sure the ruler follows suit.
   mRuler->DrawSelection();

   // As well as the SelectionBar.
   DisplaySelection();
}

void TrackPanel::UpdateAccessibility()
{
   if (mAx)
      mAx->Updated();
}

// Counts selected tracks, counting stereo tracks as one track.
size_t TrackPanel::GetSelectedTrackCount() const
{
   return GetTracks()->SelectedLeaders().size();
}

void TrackPanel::MessageForScreenReader(const wxString& message)
{
   if (mAx)
      mAx->MessageForScreenReader(message);
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
      ProjectStatus::Get( *GetProject() ).Set(wxT(""));
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
   GetFocusedTrack();

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
         if ( t )
            t->EnsureVisible();
      } );
   }

   // Must also fall through to base class handler
   event.Skip();
}

double TrackPanel::GetMostRecentXPos()
{
   return mViewInfo->PositionToTime(
      MostRecentXCoord(), mViewInfo->GetLabelWidth());
}

void TrackPanel::RefreshTrack(Track *trk, bool refreshbacking)
{
   if (!trk)
      return;

   trk = *GetTracks()->FindLeader(trk);
   auto &view = TrackView::Get( *trk );
   auto height =
      TrackList::Channels(trk).sum( TrackView::GetTrackHeight )
      - kTopInset - kShadowThickness;

   // subtract insets and shadows from the rectangle, but not border
   // This matters because some separators do paint over the border
   wxRect rect(kLeftInset,
            -mViewInfo->vpos + view.GetY() + kTopInset,
            GetRect().GetWidth() - kLeftInset - kRightInset - kShadowThickness,
            height);

   if( refreshbacking )
   {
      mRefreshBacking = true;
   }

   Refresh( false, &rect );
}


/// This method overrides Refresh() of wxWindow so that the
/// boolean play indictaor can be set to false, so that an old play indicator that is
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
   DisplaySelection();
}

#include "TrackPanelDrawingContext.h"

/// Draw the actual track areas.  We only draw the borders
/// and the little buttons and menues and whatnot here, the
/// actual contents of each track are drawn by the TrackArtist.
void TrackPanel::DrawTracks(wxDC * dc)
{
   wxRegion region = GetUpdateRegion();

   const wxRect clip = GetRect();

   mTrackArtist->pSelectedRegion = &mViewInfo->selectedRegion;
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

   const bool hasSolo = GetTracks()->Any< PlayableTrack >()
      .any_of( []( const PlayableTrack *pt ) {
         pt = static_cast< const PlayableTrack * >(
            pt->SubstitutePendingChangedTrack().get() );
         return (pt && pt->GetSolo());
      } );

   mTrackArtist->drawEnvelope = envelopeFlag;
   mTrackArtist->bigPoints = bigPointsFlag;
   mTrackArtist->drawSliders = sliderFlag;
   mTrackArtist->hasSolo = hasSolo;

   this->CellularPanel::Draw( context, TrackArtist::NPasses );
}

void TrackPanel::SetBackgroundCell
(const std::shared_ptr< TrackPanelCell > &pCell)
{
   mpBackground = pCell;
}

std::shared_ptr< TrackPanelCell > TrackPanel::GetBackgroundCell()
{
   return mpBackground;
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

   wxRect rect(mViewInfo->GetVRulerOffset(),
            kTopMargin,
            mViewInfo->GetVRulerWidth(),
            0);


   for (auto channel : TrackList::Channels(t)) {
      auto &view = TrackView::Get( *channel );
      rect.height = view.GetHeight() - (kTopMargin + kBottomMargin);
      TrackVRulerControls::Get( *channel ).UpdateRuler( rect );
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

   SetFocusedTrack(t);

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

/*

  The following classes define the subdivision of the area of the TrackPanel
  into cells with differing reponses to mouse, keyboard, and scroll wheel
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
  
  Lastly, split the area for each channel into a vertical ruler, and an area
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
      const std::shared_ptr< Track > &pChannel, wxCoord leftOffset )
         : mpChannel{ pChannel }, mLeftOffset{ leftOffset } {}
   Subdivision Children( const wxRect &rect ) override
   {
      return { Axis::X, Refinement{
         { rect.GetLeft(),
           TrackVRulerControls::Get( *mpChannel ).shared_from_this() },
         { mLeftOffset,
           TrackView::Get( *mpChannel ).shared_from_this() }
      } };
   }
   std::shared_ptr< Track > mpChannel;
   wxCoord mLeftOffset;
};

// n channels with vertical rulers, alternating with n - 1 resizers
struct ChannelGroup final : TrackPanelGroup {
   ChannelGroup( const std::shared_ptr< Track > &pTrack, wxCoord leftOffset )
      : mpTrack{ pTrack }, mLeftOffset{ leftOffset } {}
   Subdivision Children( const wxRect &rect ) override
   {
      Refinement refinement;

      const auto channels = TrackList::Channels( mpTrack.get() );
      const auto pLast = *channels.rbegin();
      wxCoord yy = rect.GetTop();
      for ( auto channel : channels ) {
         refinement.emplace_back( yy,
            std::make_shared< VRulerAndChannel >(
               channel->SharedPointer(), mLeftOffset ) );
         if ( channel != pLast ) {
            auto &view = TrackView::Get( *channel );
            yy += view.GetHeight();
            refinement.emplace_back(
               yy - kSeparatorThickness,
               TrackView::Get( *channel ).GetResizer() );
         }
      }

      return { Axis::Y, std::move( refinement ) };
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

   // TrackPanelDrawable impementation
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
         if (trackPanel.GetFocusedTrack() == mpTrack.get() &&
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
      { rect.GetTop() + rect.GetHeight() - kSeparatorThickness,
         TrackView::Get( **TrackList::Channels( mpTrack.get() ).rbegin() )
            .GetResizer() }
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
      return { 0, 0, 0, 0 };
   }

   return CellularPanel::FindRect( [&] ( TrackPanelNode &node ) {
      if (auto pGroup = dynamic_cast<const LabeledChannelGroup*>( &node ))
         return pGroup->mpTrack.get() == leader;
      return false;
   } );
}

/// Displays the bounds of the selection in the status bar.
void TrackPanel::DisplaySelection()
{
   if (!mListener)
      return;

   // DM: Note that the Selection Bar can actually MODIFY the selection
   // if snap-to mode is on!!!
   mListener->TP_DisplaySelection();
}

TrackPanelCell *TrackPanel::GetFocusedCell()
{
   auto pTrack = mAx->GetFocus().get();
   if (pTrack)
      return &TrackView::Get( *pTrack );
   return nullptr;
}

Track *TrackPanel::GetFocusedTrack()
{
   auto pView = dynamic_cast<TrackView *>( GetFocusedCell() );
   if (pView)
      return pView->FindTrack().get();
   return nullptr;
}

void TrackPanel::SetFocusedCell()
{
   SetFocusedTrack( GetFocusedTrack() );
}

void TrackPanel::SetFocusedTrack( Track *t )
{
   // Make sure we always have the first linked track of a stereo track
   t = *GetTracks()->FindLeader(t);

   // This will cause callback to the handler function, defined next
   mAx->SetFocus( Track::SharedPointer( t ) );
}

void TrackPanel::OnTrackFocusChange( wxCommandEvent &event )
{
   event.Skip();
   auto cell = mAx->GetFocus().get();

   if (cell) {
      KeyboardCapture::Capture(this);
      Refresh( false );
   }
}

TrackPanelDrawable::~TrackPanelDrawable()
{
}

void TrackPanelDrawable::Draw(
   TrackPanelDrawingContext &, const wxRect &, unsigned )
{
}

wxRect TrackPanelDrawable::DrawingArea(
   const wxRect &rect, const wxRect &, unsigned )
{
   return rect;
}

TrackPanelNode::TrackPanelNode()
{
}

TrackPanelNode::~TrackPanelNode()
{
}

TrackPanelGroup::TrackPanelGroup()
{
}

TrackPanelGroup::~TrackPanelGroup()
{
}

TrackPanelCell::~TrackPanelCell()
{
}

HitTestPreview TrackPanelCell::DefaultPreview
(const TrackPanelMouseState &, const AudacityProject *)
{
   return {};
}

unsigned TrackPanelCell::HandleWheelRotation
(const TrackPanelMouseEvent &, AudacityProject *)
{
   return RefreshCode::Cancelled;
}

unsigned TrackPanelCell::DoContextMenu
   (const wxRect &, wxWindow*, wxPoint *)
{
   return RefreshCode::RefreshNone;
}

unsigned TrackPanelCell::CaptureKey(
   wxKeyEvent &event, ViewInfo &, wxWindow *, AudacityProject *)
{
   event.Skip();
   return RefreshCode::RefreshNone;
}

unsigned TrackPanelCell::KeyDown(
   wxKeyEvent &event, ViewInfo &, wxWindow *, AudacityProject *)
{
   event.Skip();
   return RefreshCode::RefreshNone;
}

unsigned TrackPanelCell::KeyUp(
   wxKeyEvent &event, ViewInfo &, wxWindow *, AudacityProject *)
{
   event.Skip();
   return RefreshCode::RefreshNone;
}

unsigned TrackPanelCell::Char(
   wxKeyEvent &event, ViewInfo &, wxWindow *, AudacityProject *)
{
   event.Skip();
   return RefreshCode::RefreshNone;
}

IsVisibleTrack::IsVisibleTrack(AudacityProject *project)
   : mPanelRect {
        wxPoint{ 0, ViewInfo::Get( *project ).vpos },
        wxSize{
           ViewInfo::Get( *project ).GetTracksUsableWidth(),
           ViewInfo::Get( *project ).GetHeight()
        }
     }
{}

bool IsVisibleTrack::operator () (const Track *pTrack) const
{
   // Need to return true if this track or a later channel intersects
   // the view
   return
   TrackList::Channels(pTrack).StartingWith(pTrack).any_of(
      [this]( const Track *pT ) {
         auto &view = TrackView::Get( *pT );
         wxRect r(0, view.GetY(), 1, view.GetHeight());
         return r.Intersects(mPanelRect);
      }
   );
}
