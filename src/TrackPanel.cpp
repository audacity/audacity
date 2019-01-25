/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackPanel.cpp

  Dominic Mazzoni
  and lots of other contributors

  Implements TrackPanel and TrackInfo.

********************************************************************//*!

\file TrackPanel.cpp
\brief
  Implements TrackPanel and TrackInfo.

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

\namespace TrackInfo
\brief
  Functions for drawing the track control panel, which is shown to the side
  of a track
  It has the menus, pan and gain controls displayed in it.
  So "Info" is somewhat a misnomer. Should possibly be "TrackControls".

  It maintains global slider widget instances that are reparented and
  repositioned as needed for drawing and interaction with the user,
  interoperating with the custom panel subdivision implemented in CellularPanel
  and avoiding wxWidgets sizers

  If we'd instead coded it as a wxWindow, we would have an instance
  of this class for each track displayed.

*//**************************************************************//**

\class TrackPanel::AudacityTimer
\brief Timer class dedicated to informing the TrackPanel that it
is time to refresh some aspect of the screen.

*//*****************************************************************/

#include "Audacity.h" // for USE_* macros
#include "TrackPanel.h"

#include <wx/setup.h> // for wxUSE_* macros

#include "Experimental.h"

#include "AdornedRulerPanel.h"
#include "Project.h"
#include "TrackPanelMouseEvent.h"
#include "TrackPanelResizeHandle.h"
//#define DEBUG_DRAW_TIMING 1

#include "AColor.h"
#include "AllThemeResources.h"
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

#include "toolbars/ControlToolBar.h"
#include "toolbars/ToolsToolBar.h"

#include "tracks/ui/TrackControls.h" // for inheritance relation
#include "tracks/ui/TrackVRulerControls.h" // for inheritance relation

//This loads the appropriate set of cursors, depending on platform.
#include "../images/Cursors.h"

#include "widgets/ASlider.h"
#include <algorithm>

wxDEFINE_EVENT(EVT_TRACK_PANEL_TIMER, wxCommandEvent);

/**

\class TrackPanel

This is a diagram of TrackPanel's division of one (non-stereo) track rectangle.
Total height equals Track::GetHeight()'s value.  Total width is the wxWindow's
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



// Don't warn us about using 'this' in the base member initializer list.
#ifndef __WXGTK__ //Get rid if this pragma for gtk
#pragma warning( disable: 4355 )
#endif
TrackPanel::TrackPanel(wxWindow * parent, wxWindowID id,
                       const wxPoint & pos,
                       const wxSize & size,
                       const std::shared_ptr<TrackList> &tracks,
                       ViewInfo * viewInfo,
                       TrackPanelListener * listener,
                       AdornedRulerPanel * ruler)
   : CellularPanel(parent, id, pos, size, viewInfo,
                   wxWANTS_CHARS | wxNO_BORDER),
     mListener(listener),
     mTracks(tracks),
     mRuler(ruler),
     mTrackArtist(nullptr),
     mRefreshBacking(false),
     vrulerSize(36,0)
#ifndef __WXGTK__   //Get rid if this pragma for gtk
#pragma warning( default: 4355 )
#endif
{
   TrackInfo::ReCreateSliders( this );
   TrackInfo::UpdatePrefs( this );

   SetLayoutDirection(wxLayout_LeftToRight);
   SetLabel(_("Track Panel"));
   SetName(_("Track Panel"));
   SetBackgroundStyle(wxBG_STYLE_PAINT);

   {
      auto pAx = std::make_unique <TrackPanelAx>( this );
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
   GetProject()->Bind(wxEVT_IDLE, &TrackPanel::OnIdle, this);

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
   wxTheApp->Bind(EVT_AUDIOIO_PLAYBACK,
                     &TrackPanel::OnPlayback,
                     this);
}


TrackPanel::~TrackPanel()
{
   mTimer.Stop();

   // This can happen if a label is being edited and the user presses
   // ALT+F4 or Command+Q
   if (HasCapture())
      ReleaseMouse();
}

LWSlider *TrackPanel::GainSlider( const WaveTrack *wt )
{
   auto pControls = wt->GetTrackControl();
   auto rect = FindRect( *pControls );
   wxRect sliderRect;
   TrackInfo::GetGainRect( rect.GetTopLeft(), sliderRect );
   return TrackInfo::GainSlider(sliderRect, wt, false, this);
}

LWSlider *TrackPanel::PanSlider( const WaveTrack *wt )
{
   auto pControls = wt->GetTrackControl();
   auto rect = FindRect( *pControls );
   wxRect sliderRect;
   TrackInfo::GetPanRect( rect.GetTopLeft(), sliderRect );
   return TrackInfo::PanSlider(sliderRect, wt, false, this);
}

#ifdef EXPERIMENTAL_MIDI_OUT
LWSlider *TrackPanel::VelocitySlider( const NoteTrack *nt )
{
   auto pControls = nt->GetTrackControl();
   auto rect = FindRect( *pControls );
   wxRect sliderRect;
   TrackInfo::GetVelocityRect( rect.GetTopLeft(), sliderRect );
   return TrackInfo::VelocitySlider(sliderRect, nt, false, this);
}
#endif

wxString TrackPanel::gSoloPref;

void TrackPanel::UpdatePrefs()
{
   gPrefs->Read(wxT("/GUI/AutoScroll"), &mViewInfo->bUpdateTrackIndicator,
      true);
   gPrefs->Read(wxT("/GUI/Solo"), &gSoloPref, wxT("Simple"));

   mViewInfo->UpdatePrefs();

   if (mTrackArtist) {
      mTrackArtist->UpdatePrefs();
   }

   // All vertical rulers must be recalculated since the minimum and maximum
   // frequences may have been changed.
   UpdateVRulers();

   TrackInfo::UpdatePrefs( this );

   Refresh();
}

void TrackPanel::ApplyUpdatedTheme()
{
   TrackInfo::ReCreateSliders( this );
}


void TrackPanel::GetTracksUsableArea(int *width, int *height) const
{
   GetSize(width, height);
   if (width) {
      *width -= GetLeftOffset();
      *width -= kRightMargin;
      *width = std::max(0, *width);
   }
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
   pWind = pWind->GetParent(); //Project
   wxASSERT( pWind );
   return (AudacityProject*)pWind;
}

void TrackPanel::OnIdle(wxIdleEvent& event)
{
   // The window must be ready when the timer fires (#1401)
   if (IsShownOnScreen())
   {
      mTimer.Start(kTimerInterval, FALSE);

      // Timer is started, we don't need the event anymore
      GetProject()->Unbind(wxEVT_IDLE, &TrackPanel::OnIdle, this);
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
      if (project->IsIconized())
         project->MacShowUndockedToolbars(false);
   }
#endif

   mTimeCount++;

   AudacityProject *const p = GetProject();

   // Check whether we were playing or recording, but the stream has stopped.
   if (p->GetAudioIOToken()>0 && !IsAudioActive())
   {
      //the stream may have been started up after this one finished (by some other project)
      //in that case reset the buttons don't stop the stream
      p->GetControlToolBar()->StopPlaying(!gAudioIO->IsStreamActive());
   }

   // Next, check to see if we were playing or recording
   // audio, but now Audio I/O is completely finished.
   if (p->GetAudioIOToken()>0 &&
         !gAudioIO->IsAudioTokenActive(p->GetAudioIOToken()))
   {
      p->FixScrollbars();
      p->SetAudioIOToken(0);
      p->RedrawProject();

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
      p->GetEventHandler()->ProcessEvent(e);
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

double TrackPanel::GetScreenEndTime() const
{
   int width;
   GetTracksUsableArea(&width, NULL);
   return mViewInfo->PositionToTime(width, 0, true);
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
   GetProject()->ModifyState(bWantsAutoSave);
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
   panel->GetProject()->GetTracks()->UpdatePendingTracks();

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
      panel->EnsureVisible(pClickedTrack);
}

void TrackPanel::HandlePageUpKey()
{
   mListener->TP_ScrollWindow(2 * mViewInfo->h - GetScreenEndTime());
}

void TrackPanel::HandlePageDownKey()
{
   mListener->TP_ScrollWindow(GetScreenEndTime());
}

bool TrackPanel::IsAudioActive()
{
   AudacityProject *p = GetProject();
   return p->IsAudioActive();
}

void TrackPanel::UpdateStatusMessage( const wxString &st )
{
   auto status = st;
   if (HasEscape())
   /* i18n-hint Esc is a key on the keyboard */
      status += wxT(" "), status += _("(Esc to cancel)");
   mListener->TP_DisplayStatusMessage(status);
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

// Counts tracks, counting stereo tracks as one track.
size_t TrackPanel::GetTrackCount() const
{
  return GetTracks()->Leaders().size();
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

      mListener->TP_RedrawScrollbars();
      mListener->TP_HandleResize();
      mListener->TP_DisplayStatusMessage(wxT("")); //STM: Clear message if all tracks are removed
   }
}

void TrackPanel::OnPlayback(wxEvent &e)
{
   e.Skip();
   // Starting or stopping of play or record affects some cursors.
   // Start or stop is in progress now, not completed; so delay the cursor
   // change until next idle time.
   CallAfter( [this] { HandleCursorForPresentMouseState(); } );
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

struct TrackInfo::TCPLine {
   using DrawFunction = void (*)(
      TrackPanelDrawingContext &context,
      const wxRect &rect,
      const Track *maybeNULL
   );

   unsigned items; // a bitwise OR of values of the enum above
   int height;
   int extraSpace;
   DrawFunction drawFunction;
};

namespace {

#define RANGE(array) (array), (array) + sizeof(array)/sizeof(*(array))
using TCPLines = std::vector< TrackInfo::TCPLine >;

enum : unsigned {
   // The sequence is not significant, just keep bits distinct
   kItemBarButtons       = 1 << 0,
   kItemStatusInfo1      = 1 << 1,
   kItemMute             = 1 << 2,
   kItemSolo             = 1 << 3,
   kItemGain             = 1 << 4,
   kItemPan              = 1 << 5,
   kItemVelocity         = 1 << 6,
   kItemMidiControlsRect = 1 << 7,
   kItemMinimize         = 1 << 8,
   kItemSyncLock         = 1 << 9,
   kItemStatusInfo2      = 1 << 10,

   kHighestBottomItem = kItemMinimize,
};


#ifdef EXPERIMENTAL_DA

   #define TITLE_ITEMS \
      { kItemBarButtons, kTrackInfoBtnSize, 4, \
        &TrackInfo::CloseTitleDrawFunction },
   // DA: Has Mute and Solo on separate lines.
   #define MUTE_SOLO_ITEMS(extra) \
      { kItemMute, kTrackInfoBtnSize + 1, 1, \
        &TrackInfo::WideMuteDrawFunction }, \
      { kItemSolo, kTrackInfoBtnSize + 1, extra, \
        &TrackInfo::WideSoloDrawFunction },
   // DA: Does not have status information for a track.
   #define STATUS_ITEMS

#else

   #define TITLE_ITEMS \
      { kItemBarButtons, kTrackInfoBtnSize, 0, \
        &TrackInfo::CloseTitleDrawFunction },
   #define MUTE_SOLO_ITEMS(extra) \
      { kItemMute | kItemSolo, kTrackInfoBtnSize + 1, extra, \
        &TrackInfo::MuteAndSoloDrawFunction },
   #define STATUS_ITEMS \
      { kItemStatusInfo1, 12, 0, \
        &TrackInfo::Status1DrawFunction }, \
      { kItemStatusInfo2, 12, 0, \
        &TrackInfo::Status2DrawFunction },

#endif

#define COMMON_ITEMS \
   TITLE_ITEMS

const TrackInfo::TCPLine defaultCommonTrackTCPLines[] = {
   COMMON_ITEMS
};
TCPLines commonTrackTCPLines{ RANGE(defaultCommonTrackTCPLines) };

const TrackInfo::TCPLine defaultWaveTrackTCPLines[] = {
   COMMON_ITEMS
   MUTE_SOLO_ITEMS(2)
   { kItemGain, kTrackInfoSliderHeight, kTrackInfoSliderExtra,
     &TrackInfo::GainSliderDrawFunction },
   { kItemPan, kTrackInfoSliderHeight, kTrackInfoSliderExtra,
     &TrackInfo::PanSliderDrawFunction },
   STATUS_ITEMS
};
TCPLines waveTrackTCPLines{ RANGE(defaultWaveTrackTCPLines) };

const TrackInfo::TCPLine defaultNoteTrackTCPLines[] = {
   COMMON_ITEMS
#ifdef EXPERIMENTAL_MIDI_OUT
   MUTE_SOLO_ITEMS(0)
   { kItemMidiControlsRect, kMidiCellHeight * 4, 0,
     &TrackInfo::MidiControlsDrawFunction },
   { kItemVelocity, kTrackInfoSliderHeight, kTrackInfoSliderExtra,
     &TrackInfo::VelocitySliderDrawFunction },
#endif
};
TCPLines noteTrackTCPLines{ RANGE(defaultNoteTrackTCPLines) };

int totalTCPLines( const TCPLines &lines, bool omitLastExtra )
{
   int total = 0;
   int lastExtra = 0;
   for ( const auto line : lines ) {
      lastExtra = line.extraSpace;
      total += line.height + lastExtra;
   }
   if (omitLastExtra)
      total -= lastExtra;
   return total;
}

const TCPLines &getTCPLines( const Track &track )
{
   auto lines = track.TypeSwitch< TCPLines * >(
#ifdef USE_MIDI
      [](const NoteTrack*){
         return &noteTrackTCPLines;
      },
#endif
      [](const WaveTrack*){
         return &waveTrackTCPLines;
      },
      [](const Track*){
         return &commonTrackTCPLines;
      }
   );

   if (lines)
      return *lines;

   return commonTrackTCPLines;
}

// return y value and height
std::pair< int, int > CalcItemY( const TCPLines &lines, unsigned iItem )
{
   int y = 0;
   auto pLines = lines.begin();
   while ( pLines != lines.end() &&
           0 == (pLines->items & iItem) ) {
      y += pLines->height + pLines->extraSpace;
      ++pLines;
   }
   int height = 0;
   if ( pLines != lines.end() )
      height = pLines->height;
   return { y, height };
}

// Items for the bottom of the panel, listed bottom-upwards
// As also with the top items, the extra space is below the item
const TrackInfo::TCPLine defaultCommonTrackTCPBottomLines[] = {
   // The '0' avoids impinging on bottom line of TCP
   // Use -1 if you do want to do so.
   { kItemSyncLock | kItemMinimize, kTrackInfoBtnSize, 0,
     &TrackInfo::MinimizeSyncLockDrawFunction },
};
TCPLines commonTrackTCPBottomLines{ RANGE(defaultCommonTrackTCPBottomLines) };

// return y value and height
std::pair< int, int > CalcBottomItemY
   ( const TCPLines &lines, unsigned iItem, int height )
{
   int y = height;
   auto pLines = lines.begin();
   while ( pLines != lines.end() &&
           0 == (pLines->items & iItem) ) {
      y -= pLines->height + pLines->extraSpace;
      ++pLines;
   }
   if (pLines != lines.end())
      y -= (pLines->height + pLines->extraSpace );
   return { y, pLines->height };
}

}

unsigned TrackInfo::MinimumTrackHeight()
{
   unsigned height = 0;
   if (!commonTrackTCPLines.empty())
      height += commonTrackTCPLines.front().height;
   if (!commonTrackTCPBottomLines.empty())
      height += commonTrackTCPBottomLines.front().height;
   // + 1 prevents the top item from disappearing for want of enough space,
   // according to the rules in HideTopItem.
   return height + kTopMargin + kBottomMargin + 1;
}

bool TrackInfo::HideTopItem( const wxRect &rect, const wxRect &subRect,
                 int allowance ) {
   auto limit = CalcBottomItemY
   ( commonTrackTCPBottomLines, kHighestBottomItem, rect.height).first;
   // Return true if the rectangle is even touching the limit
   // without an overlap.  That was the behavior as of 2.1.3.
   return subRect.y + subRect.height - allowance >= rect.y + limit;
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
            EnsureVisible(t.get());
      } );
   }

   // Must also fall through to base class handler
   event.Skip();
}

double TrackPanel::GetMostRecentXPos()
{
   return mViewInfo->PositionToTime(MostRecentXCoord(), GetLabelWidth());
}

void TrackPanel::RefreshTrack(Track *trk, bool refreshbacking)
{
   if (!trk)
      return;

   trk = *GetTracks()->FindLeader(trk);
   auto height =
      TrackList::Channels(trk).sum( &Track::GetHeight )
      - kTopInset - kShadowThickness;

   // subtract insets and shadows from the rectangle, but not border
   // This matters because some separators do paint over the border
   wxRect rect(kLeftInset,
            -mViewInfo->vpos + trk->GetY() + kTopInset,
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

   // Draw margins on two or three sides.
   ClearLeftAndRightMargins(context, clip);
   if ( GetTracks()->Any() )
      // This margin may may scrolled up out of view
      ClearTopMargin( context, clip );

   // Don't draw a bottom margin here.

   ToolsToolBar *pTtb = GetProject()->GetToolsToolBar();
   bool bMultiToolDown = pTtb->IsDown(multiTool);
   bool envelopeFlag   = pTtb->IsDown(envelopeTool) || bMultiToolDown;
   bool bigPointsFlag  = pTtb->IsDown(drawTool) || bMultiToolDown;
   bool sliderFlag     = bMultiToolDown;

   const bool hasSolo = GetTracks()->Any< PlayableTrack >()
      .any_of( []( const PlayableTrack *pt ) {
         pt = static_cast< const PlayableTrack * >(
            pt->SubstitutePendingChangedTrack().get() );
         return (pt && pt->GetSolo());
      } );

   mTrackArtist->leftOffset = GetLeftOffset();
   mTrackArtist->drawEnvelope = envelopeFlag;
   mTrackArtist->bigPoints = bigPointsFlag;
   mTrackArtist->drawSliders = sliderFlag;
   mTrackArtist->hasSolo = hasSolo;
   TrackArt::DrawTracks( context, GetTracks(), region, clip );

   // Draw the rest, including the click-to-deselect blank area below all
   // tracks
   DrawEverythingElse(context, region, clip);
}

/// Draws 'Everything else'.  In particular it draws:
///  - Drop shadow for tracks and vertical rulers.
///  - Zooming Indicators.
///  - Fills in space below the tracks.
void TrackPanel::DrawEverythingElse(TrackPanelDrawingContext &context,
                                    const wxRegion &region,
                                    const wxRect & clip)
{
   // We draw everything else
   auto dc = &context.dc;

   // Fix the horizontal extent, will vary the vertical:
   wxRect trackRect{
      kLeftMargin, 0, clip.width - (kLeftMargin + kRightMargin), 0
   };
   wxRect focusRect{};

   // The loop below now groups each track with the margin BELOW it, to
   // correspond better with the subdivision of panel area used in hit testing.

   for ( auto leaderTrack : GetTracks()->Leaders< const Track >()
         // Predicate is true iff any channel in the group is wholly or partly
         // visible:
         + IsVisibleTrack{ GetProject() } ) {
      const auto channels = TrackList::Channels(leaderTrack);
      bool focused = false;
      wxRect teamRect = trackRect;
      teamRect.height = 0;
      bool first = true;
      for (auto channel : channels) {
         focused = focused || mAx->IsFocused(channel);
         channel = channel->SubstitutePendingChangedTrack().get();
         if (first)
            first = false,
            teamRect.y = channel->GetY() - mViewInfo->vpos + kTopMargin;
         teamRect.height += channel->GetHeight();
      }

      if (focused) {
         focusRect = teamRect;
         focusRect.height -= kSeparatorThickness;
      }
      DrawOutside(context, leaderTrack, teamRect);

      // Believe it or not, we can speed up redrawing if we don't
      // redraw the vertical ruler when only the waveform data has
      // changed.  An example is during recording.

#if DEBUG_DRAW_TIMING
//      wxRect rbox = region.GetBox();
//      wxPrintf(wxT("Update Region: %d %d %d %d\n"),
//             rbox.x, rbox.y, rbox.width, rbox.height);
#endif

      for (auto channel : channels) {
         bool bSelected = channel->GetSelected();
         channel = channel->SubstitutePendingChangedTrack().get();
         trackRect.y = channel->GetY() - mViewInfo->vpos + kTopMargin;
         trackRect.height = channel->GetHeight();
         if (region.Contains(
            0, trackRect.y, GetLeftOffset(), trackRect.height)) {
            wxRect rect{
               GetVRulerOffset(),
               trackRect.y,
               GetVRulerWidth() + 1,
               trackRect.height - kSeparatorThickness
            };
            TrackArt::DrawVRuler(context, channel, rect, bSelected);
         }
      }
   }

   auto target = Target();
   if (target)
      target->DrawExtras(UIHandle::Cells, dc, region, clip);

   // Paint over the part below the tracks
   trackRect.y += trackRect.height;
   if (trackRect.y < clip.GetBottom()) {
      AColor::TrackPanelBackground(dc, false);
      dc->DrawRectangle(trackRect.x,
                        trackRect.y,
                        trackRect.width,
                        clip.height - trackRect.y);
   }

   // Sometimes highlight is not drawn on backing bitmap. I thought
   // it was because FindFocus did not return "this" on Mac, but
   // when I removed that test, yielding this condition:
   //     if (GetFocusedTrack() != NULL) {
   // the highlight was reportedly drawn even when something else
   // was the focus and no highlight should be drawn. -RBD
   if (GetFocusedTrack() != NULL &&
      wxWindow::FindFocus() == this ) {
      HighlightFocusedTrack(dc, focusRect);
   }

   if (target)
      target->DrawExtras(UIHandle::Panel, dc, region, clip);
}

// Make this #include go away!
#include "tracks/ui/TrackControls.h"

void TrackInfo::DrawItems
( TrackPanelDrawingContext &context,
  const wxRect &rect, const Track &track  )
{
   const auto topLines = getTCPLines( track );
   const auto bottomLines = commonTrackTCPBottomLines;
   DrawItems
      ( context, rect, &track, topLines, bottomLines );
}

void TrackInfo::DrawItems
( TrackPanelDrawingContext &context,
  const wxRect &rect, const Track *pTrack,
  const std::vector<TCPLine> &topLines, const std::vector<TCPLine> &bottomLines )
{
   auto dc = &context.dc;
   TrackInfo::SetTrackInfoFont(dc);
   dc->SetTextForeground(theTheme.Colour(clrTrackPanelText));

   {
      int yy = 0;
      for ( const auto &line : topLines ) {
         wxRect itemRect{
            rect.x, rect.y + yy,
            rect.width, line.height
         };
         if ( !TrackInfo::HideTopItem( rect, itemRect ) &&
              line.drawFunction )
            line.drawFunction( context, itemRect, pTrack );
         yy += line.height + line.extraSpace;
      }
   }
   {
      int yy = rect.height;
      for ( const auto &line : bottomLines ) {
         yy -= line.height + line.extraSpace;
         if ( line.drawFunction ) {
            wxRect itemRect{
               rect.x, rect.y + yy,
               rect.width, line.height
            };
            line.drawFunction( context, itemRect, pTrack );
         }
      }
   }
}

#include "tracks/ui/TrackButtonHandles.h"
void TrackInfo::CloseTitleDrawFunction
( TrackPanelDrawingContext &context,
  const wxRect &rect, const Track *pTrack )
{
   auto dc = &context.dc;
   bool selected = pTrack ? pTrack->GetSelected() : true;
   {
      wxRect bev = rect;
      GetCloseBoxHorizontalBounds( rect, bev );
      auto target = dynamic_cast<CloseButtonHandle*>( context.target.get() );
      bool hit = target && target->GetTrack().get() == pTrack;
      bool captured = hit && target->IsClicked();
      bool down = captured && bev.Contains( context.lastState.GetPosition());
      AColor::Bevel2(*dc, !down, bev, selected, hit );

#ifdef EXPERIMENTAL_THEMING
      wxPen pen( theTheme.Colour( clrTrackPanelText ));
      dc->SetPen( pen );
#else
      dc->SetPen(*wxBLACK_PEN);
#endif
      bev.Inflate( -1, -1 );
      // Draw the "X"
      const int s = 6;

      int ls = bev.x + ((bev.width - s) / 2);
      int ts = bev.y + ((bev.height - s) / 2);
      int rs = ls + s;
      int bs = ts + s;

      AColor::Line(*dc, ls,     ts, rs,     bs);
      AColor::Line(*dc, ls + 1, ts, rs + 1, bs);
      AColor::Line(*dc, rs,     ts, ls,     bs);
      AColor::Line(*dc, rs + 1, ts, ls + 1, bs);

      //   bev.Inflate(-1, -1);
   }

   {
      wxRect bev = rect;
      GetTitleBarHorizontalBounds( rect, bev );
      auto target = dynamic_cast<MenuButtonHandle*>( context.target.get() );
      bool hit = target && target->GetTrack().get() == pTrack;
      bool captured = hit && target->IsClicked();
      bool down = captured && bev.Contains( context.lastState.GetPosition());
      wxString titleStr =
         pTrack ? pTrack->GetName() : _("Name");

      //bev.Inflate(-1, -1);
      AColor::Bevel2(*dc, !down, bev, selected, hit);

      // Draw title text
      SetTrackInfoFont(dc);

      // Bug 1660 The 'k' of 'Audio Track' was being truncated.
      // Constant of 32 found by counting pixels on a windows machine.
      // I believe it's the size of the X close button + the size of the 
      // drop down arrow.
      int allowableWidth = rect.width - 32;

      wxCoord textWidth, textHeight;
      dc->GetTextExtent(titleStr, &textWidth, &textHeight);
      while (textWidth > allowableWidth) {
         titleStr = titleStr.Left(titleStr.length() - 1);
         dc->GetTextExtent(titleStr, &textWidth, &textHeight);
      }

      // Pop-up triangle
   #ifdef EXPERIMENTAL_THEMING
      wxColour c = theTheme.Colour( clrTrackPanelText );
   #else
      wxColour c = *wxBLACK;
   #endif

      // wxGTK leaves little scraps (antialiasing?) of the
      // characters if they are repeatedly drawn.  This
      // happens when holding down mouse button and moving
      // in and out of the title bar.  So clear it first.
   //   AColor::MediumTrackInfo(dc, t->GetSelected());
   //   dc->DrawRectangle(bev);

      dc->SetTextForeground( c );
      dc->SetTextBackground( wxTRANSPARENT );
      dc->DrawText(titleStr, bev.x + 2, bev.y + (bev.height - textHeight) / 2);



      dc->SetPen(c);
      dc->SetBrush(c);

      int s = 10; // Width of dropdown arrow...height is half of width
      AColor::Arrow(*dc,
                    bev.GetRight() - s - 3, // 3 to offset from right border
                    bev.y + ((bev.height - (s / 2)) / 2),
                    s);

   }
}

void TrackInfo::MinimizeSyncLockDrawFunction
( TrackPanelDrawingContext &context,
  const wxRect &rect, const Track *pTrack )
{
   auto dc = &context.dc;
   bool selected = pTrack ? pTrack->GetSelected() : true;
   bool syncLockSelected = pTrack ? pTrack->IsSyncLockSelected() : true;
   bool minimized = pTrack ? pTrack->GetMinimized() : false;
   {
      wxRect bev = rect;
      GetMinimizeHorizontalBounds(rect, bev);
      auto target = dynamic_cast<MinimizeButtonHandle*>( context.target.get() );
      bool hit = target && target->GetTrack().get() == pTrack;
      bool captured = hit && target->IsClicked();
      bool down = captured && bev.Contains( context.lastState.GetPosition());

      // Clear background to get rid of previous arrow
      //AColor::MediumTrackInfo(dc, t->GetSelected());
      //dc->DrawRectangle(bev);

      AColor::Bevel2(*dc, !down, bev, selected, hit);

#ifdef EXPERIMENTAL_THEMING
      wxColour c = theTheme.Colour(clrTrackPanelText);
      dc->SetBrush(c);
      dc->SetPen(c);
#else
      AColor::Dark(dc, selected);
#endif

      AColor::Arrow(*dc,
                    bev.x - 5 + bev.width / 2,
                    bev.y - 2 + bev.height / 2,
                    10,
                    minimized);
   }

   {
      wxRect bev = rect;
      GetSelectButtonHorizontalBounds(rect, bev);
      auto target = dynamic_cast<SelectButtonHandle*>( context.target.get() );
      bool hit = target && target->GetTrack().get() == pTrack;
      bool captured = hit && target->IsClicked();
      bool down = captured && bev.Contains( context.lastState.GetPosition());

      AColor::Bevel2(*dc, !down, bev, selected, hit);

#ifdef EXPERIMENTAL_THEMING
      wxColour c = theTheme.Colour(clrTrackPanelText);
      dc->SetBrush(c);
      dc->SetPen(c);
#else
      AColor::Dark(dc, selected);
#endif

      wxString str = _("Select");
      wxCoord textWidth;
      wxCoord textHeight;
      SetTrackInfoFont(dc);
      dc->GetTextExtent(str, &textWidth, &textHeight);

      dc->SetTextForeground( c );
      dc->SetTextBackground( wxTRANSPARENT );
      dc->DrawText(str, bev.x + 2 + (bev.width-textWidth)/2, bev.y + (bev.height - textHeight) / 2);
   }


   // Draw the sync-lock indicator if this track is in a sync-lock selected group.
   if (syncLockSelected)
   {
      wxRect syncLockIconRect = rect;
	
      GetSyncLockHorizontalBounds( rect, syncLockIconRect );
      wxBitmap syncLockBitmap(theTheme.Image(bmpSyncLockIcon));
      // Icon is 12x12 and syncLockIconRect is 16x16.
      dc->DrawBitmap(syncLockBitmap,
                     syncLockIconRect.x + 3,
                     syncLockIconRect.y + 2,
                     true);
   }
}

#include "tracks/playabletrack/notetrack/ui/NoteTrackButtonHandle.h"
void TrackInfo::MidiControlsDrawFunction
( TrackPanelDrawingContext &context,
  const wxRect &rect, const Track *pTrack )
{
#ifdef EXPERIMENTAL_MIDI_OUT
   auto target = dynamic_cast<NoteTrackButtonHandle*>( context.target.get() );
   bool hit = target && target->GetTrack().get() == pTrack;
   auto channel = hit ? target->GetChannel() : -1;
   auto &dc = context.dc;
   wxRect midiRect = rect;
   GetMidiControlsHorizontalBounds(rect, midiRect);
   NoteTrack::DrawLabelControls
      ( static_cast<const NoteTrack *>(pTrack), dc, midiRect, channel );
#endif // EXPERIMENTAL_MIDI_OUT
}

template<typename TrackClass>
void TrackInfo::SliderDrawFunction
( LWSlider *(*Selector)
    (const wxRect &sliderRect, const TrackClass *t, bool captured, wxWindow*),
  wxDC *dc, const wxRect &rect, const Track *pTrack,
  bool captured, bool highlight )
{
   wxRect sliderRect = rect;
   TrackInfo::GetSliderHorizontalBounds( rect.GetTopLeft(), sliderRect );
   auto wt = static_cast<const TrackClass*>( pTrack );
   Selector( sliderRect, wt, captured, nullptr )->OnPaint(*dc, highlight);
}

#include "tracks/playabletrack/wavetrack/ui/WaveTrackSliderHandles.h"
void TrackInfo::PanSliderDrawFunction
( TrackPanelDrawingContext &context,
  const wxRect &rect, const Track *pTrack )
{
   auto target = dynamic_cast<PanSliderHandle*>( context.target.get() );
   auto dc = &context.dc;
   bool hit = target && target->GetTrack().get() == pTrack;
   bool captured = hit && target->IsClicked();
   SliderDrawFunction<WaveTrack>
      ( &TrackInfo::PanSlider, dc, rect, pTrack, captured, hit);
}

void TrackInfo::GainSliderDrawFunction
( TrackPanelDrawingContext &context,
  const wxRect &rect, const Track *pTrack )
{
   auto target = dynamic_cast<GainSliderHandle*>( context.target.get() );
   auto dc = &context.dc;
   bool hit = target && target->GetTrack().get() == pTrack;
   if( hit )
      hit=hit;
   bool captured = hit && target->IsClicked();
   SliderDrawFunction<WaveTrack>
      ( &TrackInfo::GainSlider, dc, rect, pTrack, captured, hit);
}

#ifdef EXPERIMENTAL_MIDI_OUT
#include "tracks/playabletrack/notetrack/ui/NoteTrackSliderHandles.h"
void TrackInfo::VelocitySliderDrawFunction
( TrackPanelDrawingContext &context,
  const wxRect &rect, const Track *pTrack )
{
   auto dc = &context.dc;
   auto target = dynamic_cast<VelocitySliderHandle*>( context.target.get() );
   bool hit = target && target->GetTrack().get() == pTrack;
   bool captured = hit && target->IsClicked();
   SliderDrawFunction<NoteTrack>
      ( &TrackInfo::VelocitySlider, dc, rect, pTrack, captured, hit);
}
#endif

void TrackInfo::MuteOrSoloDrawFunction
( wxDC *dc, const wxRect &bev, const Track *pTrack, bool down, 
  bool WXUNUSED(captured),
  bool solo, bool hit )
{
   //bev.Inflate(-1, -1);
   bool selected = pTrack ? pTrack->GetSelected() : true;
   auto pt = dynamic_cast<const PlayableTrack *>(pTrack);
   bool value = pt ? (solo ? pt->GetSolo() : pt->GetMute()) : false;

#if 0
   AColor::MediumTrackInfo( dc, t->GetSelected());
   if( solo )
   {
      if( pt && pt->GetSolo() )
      {
         AColor::Solo(dc, pt->GetSolo(), t->GetSelected());
      }
   }
   else
   {
      if( pt && pt->GetMute() )
      {
         AColor::Mute(dc, pt->GetMute(), t->GetSelected(), pt->GetSolo());
      }
   }
   //(solo) ? AColor::Solo(dc, t->GetSolo(), t->GetSelected()) :
   //    AColor::Mute(dc, t->GetMute(), t->GetSelected(), t->GetSolo());
   dc->SetPen( *wxTRANSPARENT_PEN );//No border!
   dc->DrawRectangle(bev);
#endif

   wxCoord textWidth, textHeight;
   wxString str = (solo) ?
      /* i18n-hint: This is on a button that will silence all the other tracks.*/
      _("Solo") :
      /* i18n-hint: This is on a button that will silence this track.*/
      _("Mute");

   AColor::Bevel2(
      *dc,
      value == down,
      bev,
      selected, hit
   );

   SetTrackInfoFont(dc);
   dc->GetTextExtent(str, &textWidth, &textHeight);
   dc->DrawText(str, bev.x + (bev.width - textWidth) / 2, bev.y + (bev.height - textHeight) / 2);
}

#include "tracks/playabletrack/ui/PlayableTrackButtonHandles.h"
void TrackInfo::WideMuteDrawFunction
( TrackPanelDrawingContext &context,
  const wxRect &rect, const Track *pTrack )
{
   auto dc = &context.dc;
   wxRect bev = rect;
   GetWideMuteSoloHorizontalBounds( rect, bev );
   auto target = dynamic_cast<MuteButtonHandle*>( context.target.get() );
   bool hit = target && target->GetTrack().get() == pTrack;
   bool captured = hit && target->IsClicked();
   bool down = captured && bev.Contains( context.lastState.GetPosition());
   MuteOrSoloDrawFunction( dc, bev, pTrack, down, captured, false, hit );
}

void TrackInfo::WideSoloDrawFunction
( TrackPanelDrawingContext &context,
  const wxRect &rect, const Track *pTrack )
{
   auto dc = &context.dc;
   wxRect bev = rect;
   GetWideMuteSoloHorizontalBounds( rect, bev );
   auto target = dynamic_cast<SoloButtonHandle*>( context.target.get() );
   bool hit = target && target->GetTrack().get() == pTrack;
   bool captured = hit && target->IsClicked();
   bool down = captured && bev.Contains( context.lastState.GetPosition());
   MuteOrSoloDrawFunction( dc, bev, pTrack, down, captured, true, hit );
}

void TrackInfo::MuteAndSoloDrawFunction
( TrackPanelDrawingContext &context,
  const wxRect &rect, const Track *pTrack )
{
   auto dc = &context.dc;
   bool bHasSoloButton = TrackPanel::HasSoloButton();

   wxRect bev = rect;
   if ( bHasSoloButton )
      GetNarrowMuteHorizontalBounds( rect, bev );
   else
      GetWideMuteSoloHorizontalBounds( rect, bev );
   {
      auto target = dynamic_cast<MuteButtonHandle*>( context.target.get() );
      bool hit = target && target->GetTrack().get() == pTrack;
      bool captured = hit && target->IsClicked();
      bool down = captured && bev.Contains( context.lastState.GetPosition());
      MuteOrSoloDrawFunction( dc, bev, pTrack, down, captured, false, hit );
   }

   if( !bHasSoloButton )
      return;

   GetNarrowSoloHorizontalBounds( rect, bev );
   {
      auto target = dynamic_cast<SoloButtonHandle*>( context.target.get() );
      bool hit = target && target->GetTrack().get() == pTrack;
      bool captured = hit && target->IsClicked();
      bool down = captured && bev.Contains( context.lastState.GetPosition());
      MuteOrSoloDrawFunction( dc, bev, pTrack, down, captured, true, hit );
   }
}

void TrackInfo::StatusDrawFunction
   ( const wxString &string, wxDC *dc, const wxRect &rect )
{
   static const int offset = 3;
   dc->DrawText(string, rect.x + offset, rect.y);
}

void TrackInfo::Status1DrawFunction
( TrackPanelDrawingContext &context,
  const wxRect &rect, const Track *pTrack )
{
   auto dc = &context.dc;
   auto wt = static_cast<const WaveTrack*>(pTrack);

   /// Returns the string to be displayed in the track label
   /// indicating whether the track is mono, left, right, or
   /// stereo and what sample rate it's using.
   auto rate = wt ? wt->GetRate() : 44100.0;
   wxString s;
   if (!pTrack || TrackList::Channels(pTrack).size() > 1)
      // TODO: more-than-two-channels-message
      // more appropriate strings
      s = _("Stereo, %dHz");
   else {
      if (wt->GetChannel() == Track::MonoChannel)
         s = _("Mono, %dHz");
      else if (wt->GetChannel() == Track::LeftChannel)
         s = _("Left, %dHz");
      else if (wt->GetChannel() == Track::RightChannel)
         s = _("Right, %dHz");
   }
   s = wxString::Format( s, (int) (rate + 0.5) );

   StatusDrawFunction( s, dc, rect );
}

void TrackInfo::Status2DrawFunction
( TrackPanelDrawingContext &context,
  const wxRect &rect, const Track *pTrack )
{
   auto dc = &context.dc;
   auto wt = static_cast<const WaveTrack*>(pTrack);
   auto format = wt ? wt->GetSampleFormat() : floatSample;
   auto s = GetSampleFormatStr(format);
   StatusDrawFunction( s, dc, rect );
}

void TrackPanel::DrawOutside
(TrackPanelDrawingContext &context,
 const Track * t, const wxRect & rec)
{
   // Given rectangle excludes left and right margins, and encompasses a
   // channel group of tracks, plus the resizer area below

   auto dc = &context.dc;

   // Start with whole track rect
   wxRect rect = rec;

   {
      ClearSeparator(context, rect);

      // Now exclude the resizer below
      rect.height -= kSeparatorThickness;

      int labelw = GetLabelWidth();
      int vrul = GetVRulerOffset();

      TrackInfo::DrawBackground( dc, rect, t->GetSelected(), vrul );

      // Vaughan, 2010-08-24: No longer doing this.
      // Draw sync-lock tiles in ruler area.
      //if (t->IsSyncLockSelected()) {
      //   wxRect tileFill = rect;
      //   tileFill.x = GetVRulerOffset();
      //   tileFill.width = GetVRulerWidth();
      //   TrackArt::DrawSyncLockTiles(dc, tileFill);
      //}

      DrawBordersAroundTrack( dc, rect );
      {
         auto channels = TrackList::Channels(t);
         // omit last (perhaps, only) channel
         --channels.second;
         for (auto channel : channels) {
            // draw the sash below this channel
            channel = channel->SubstitutePendingChangedTrack().get();
            auto yy =
               channel->GetY() - mViewInfo->vpos + channel->GetHeight()
                  - kBottomMargin;
            wxRect sashRect{
               vrul, yy, rect.GetRight() - vrul, kSeparatorThickness
            };
            DrawSash( dc, sashRect, labelw, t->GetSelected() );
         }
      }

      DrawShadow( dc, rect );
   }

   // Draw things within the track control panel
   rect.width = kTrackInfoWidth;
   TrackInfo::DrawItems( context, rect, *t );

   //mTrackInfo.DrawBordersWithin( dc, rect, *t );
}

void TrackPanel::ClearTopMargin
(TrackPanelDrawingContext &context, const wxRect &clip)
{
   auto dc = &context.dc;

   // Area above the first track if there is one
   AColor::TrackPanelBackground(dc, false);
   wxRect side{
      clip.x + kLeftMargin,
      -mViewInfo->vpos,
      clip.width - ( kLeftMargin + kRightMargin ),
      kTopMargin
   };

   if (side.Intersects(clip))
      dc->DrawRectangle(side);
}

// Paint the inset areas of the whole panel, left and right, in a background
// color
void TrackPanel::ClearLeftAndRightMargins
(TrackPanelDrawingContext &context, const wxRect & clip)
{
   auto dc = &context.dc;

   // Fill in area outside of tracks
   AColor::TrackPanelBackground(dc, false);
   wxRect side;

   // Area between panel border and left track border
   side = clip;
   side.width = kLeftMargin;
   dc->DrawRectangle(side);

   // Area between panel border and right track border
   side = clip;
   side.x += side.width - kRightMargin;
   side.width = kRightMargin;
   dc->DrawRectangle(side);
}

// Given rectangle should be the whole track rectangle
// Paint the separator area below in a background color
void TrackPanel::ClearSeparator
(TrackPanelDrawingContext &context, const wxRect & rect)
{
   auto dc = &context.dc;

   // Fill in area outside of the track
   AColor::TrackPanelBackground(dc, false);

   // Area below the track, where the resizer will be
   auto height = kSeparatorThickness;
   wxRect side{
      rect.x,
      rect.y + rect.height - height,
      rect.width,
      height
   };
   dc->DrawRectangle(side);
}

void TrackPanel::DrawSash(
   wxDC * dc, const wxRect & rect, int labelw, bool bSelected )
{
   // Area between channels of a group
   // Paint the channel separator over (what would be) the lower border of this
   // channel, down to and including the upper border of the next channel

   ADCChanger cleanup{ dc };

   // Paint the left part of the background
   AColor::MediumTrackInfo(dc, bSelected);
   dc->DrawRectangle( rect.GetX(), rect.GetY(), labelw, rect.GetHeight() );

   // Stroke the left border
   dc->SetPen(*wxBLACK_PEN);
   {
      const auto left = rect.GetLeft();
      AColor::Line( *dc, left, rect.GetTop(), left, rect.GetBottom() );
   }

   AColor::TrackPanelBackground(dc, false);

   wxRect rec{ rect };
   rec.width -= labelw - rec.x;
   rec.x = labelw;

   dc->DrawRectangle( wxRect( rec ).Inflate( 0, -kBorderThickness ) );

   // These lines stroke over what is otherwise "border" of each channel
   dc->SetBrush(*wxTRANSPARENT_BRUSH);
   dc->SetPen(*wxBLACK_PEN);
   const auto left = rec.GetLeft();
   const auto right = rec.GetRight();
   const auto top = rec.GetTop();
   const auto bottom = rec.GetBottom();
   AColor::Line( *dc, left, top,    right, top    );
   AColor::Line( *dc, left, bottom, right, bottom );
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

/// Draw a three-level highlight gradient around the focused track.
void TrackPanel::HighlightFocusedTrack(wxDC * dc, const wxRect & rect)
{
   wxRect theRect = rect;
   theRect.Inflate( kBorderThickness );
   theRect.width += kShadowThickness;
   theRect.height += kShadowThickness;
   dc->SetBrush(*wxTRANSPARENT_BRUSH);

   AColor::TrackFocusPen(dc, 0);
   theRect.Inflate(1);
   dc->DrawRectangle(theRect);

   AColor::TrackFocusPen(dc, 1);
   theRect.Inflate(1);
   dc->DrawRectangle(theRect);

   AColor::TrackFocusPen(dc, 2);
   theRect.Inflate(1);
   dc->DrawRectangle(theRect);
}

void TrackPanel::UpdateVRulers()
{
   for (auto t : GetTracks()->Any< const WaveTrack >())
      UpdateTrackVRuler(t);

   UpdateVRulerSize();
}

void TrackPanel::UpdateVRuler(Track *t)
{
   if (t)
      UpdateTrackVRuler(t);

   UpdateVRulerSize();
}

void TrackPanel::UpdateTrackVRuler(const Track *t)
{
   wxASSERT(t);
   if (!t)
      return;

   wxRect rect(GetVRulerOffset(),
            kTopMargin,
            GetVRulerWidth(),
            0);


   for (auto channel : TrackList::Channels(t)) {
      rect.height = channel->GetHeight() - (kTopMargin + kBottomMargin);
      mTrackArtist->UpdateVRuler(channel, rect);
   }
}

void TrackPanel::UpdateVRulerSize()
{
   auto trackRange = GetTracks()->Any();
   if (trackRange) {
      wxSize s { 0, 0 };
      for (auto t : trackRange)
         s.IncTo(t->vrulerSize);

      if (vrulerSize != s) {
         vrulerSize = s;
         mRuler->SetLeftOffset(GetLeftOffset());  // bevel on AdornedRuler
         mRuler->Refresh();
      }
   }
   Refresh(false);
}

// Make sure selection edge is in view
void TrackPanel::ScrollIntoView(double pos)
{
   int w;
   GetTracksUsableArea( &w, NULL );

   int pixel = mViewInfo->TimeToPosition(pos);
   if (pixel < 0 || pixel >= w)
   {
      mListener->TP_ScrollWindow
         (mViewInfo->OffsetTimeByPixels(pos, -(w / 2)));
      Refresh(false);
   }
}

void TrackPanel::ScrollIntoView(int x)
{
   ScrollIntoView(mViewInfo->PositionToTime(x, GetLeftOffset()));
}

void TrackPanel::OnTrackMenu(Track *t)
{
   CellularPanel::DoContextMenu( t );
}

Track * TrackPanel::GetFirstSelectedTrack()
{
   auto t = *GetTracks()->Selected().begin();
   if (t)
      return t;
   else
      //if nothing is selected, return the first track
      return *GetTracks()->Any().begin();
}

void TrackPanel::EnsureVisible(Track * t)
{
   SetFocusedTrack(t);

   int trackTop = 0;
   int trackHeight =0;

   for (auto it : GetTracks()->Leaders()) {
      trackTop += trackHeight;

      auto channels = TrackList::Channels(it);
      trackHeight = channels.sum( &Track::GetHeight );

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
}

// 0.0 scrolls to top
// 1.0 scrolls to bottom.
void TrackPanel::VerticalScroll( float fracPosition){

   int trackTop = 0;
   int trackHeight = 0;

   auto tracks = GetTracks();
   auto GetHeight =
      [&]( const Track *t ){ return tracks->GetGroupHeight(t); };

   auto range = tracks->Leaders();
   if (!range.empty()) {
      trackHeight = GetHeight( *range.rbegin() );
      --range.second;
   }
   trackTop = range.sum( GetHeight );

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


// Draw a rectangular border
void TrackPanel::DrawBordersAroundTrack( wxDC * dc, const wxRect & rect )
{
   // Border around track and label area
   // leaving room for the shadow
   dc->SetBrush(*wxTRANSPARENT_BRUSH);
   dc->SetPen(*wxBLACK_PEN);
   dc->DrawRectangle( rect.Inflate( kBorderThickness, kBorderThickness ) );
}

// Given rectangle is the track rectangle excluding the border
// Stroke lines along bottom and right, which are slightly short at
// bottom-left and top-right
void TrackPanel::DrawShadow( wxDC * dc, const wxRect & rect )
{
   int right = rect.GetRight() + kBorderThickness + kShadowThickness;
   int bottom = rect.GetBottom() + kBorderThickness + kShadowThickness;

   // shadow color for lines
   dc->SetPen(*wxBLACK_PEN);

   // bottom
   AColor::Line(*dc, rect.x, bottom, right, bottom);
   // right
   AColor::Line(*dc, right, rect.y, right, bottom);

   // background color erases small parts of those lines
   AColor::TrackPanelBackground(dc, false);

   // bottom-left
   AColor::Line(*dc, rect.x, bottom, rect.x + 1, bottom);
   // top-right
   AColor::Line(*dc, right, rect.y, right, rect.y + 1);
}

namespace {

// Helper classes to implement the subdivision of TrackPanel area for
// CellularPanel

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
};

// A vertical ruler left of a channel
struct VRulerAndChannel final : TrackPanelGroup {
   VRulerAndChannel(
      const std::shared_ptr< Track > &pChannel, wxCoord leftOffset )
         : mpChannel{ pChannel }, mLeftOffset{ leftOffset } {}
   Subdivision Children( const wxRect &rect ) override
   {
      return { Axis::X, Refinement{
         { rect.GetLeft(), mpChannel->GetVRulerControl() },
         { mLeftOffset, mpChannel }
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
            const auto substitute =
               channel->SubstitutePendingChangedTrack();
            yy += substitute->GetHeight();
            refinement.emplace_back(
               yy - kSeparatorThickness, channel->GetResizer() );
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
      { rect.GetLeft(), mpTrack->GetTrackControl() },
      { rect.GetLeft() + kTrackInfoWidth,
        std::make_shared< ChannelGroup >( mpTrack, mLeftOffset ) }
   } }; }
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
         ( *TrackList::Channels( mpTrack.get() ).rbegin() )->GetResizer() }
   } }; }
   std::shared_ptr< Track > mpTrack;
   wxCoord mLeftOffset;
};

// Stacks a dead area at top, the tracks, and the click-to-deselect area below
struct Subgroup final : TrackPanelGroup {
   explicit Subgroup( TrackPanel &panel ) : mPanel{ panel } {}
   Subdivision Children( const wxRect &rect ) override
   {
      wxCoord yy = -mPanel.GetViewInfo()->vpos;
      Refinement refinement;

      auto &tracks = *mPanel.GetTracks();
      if ( tracks.Any() )
         refinement.emplace_back( yy, EmptyCell::Instance() ),
         yy += kTopMargin;

      for ( const auto leader : tracks.Leaders() ) {
         wxCoord height = 0;
         for ( auto channel : TrackList::Channels( leader ) ) {
            auto substitute =
               channel->SubstitutePendingChangedTrack();
            height += substitute->GetHeight();
         }
         refinement.emplace_back( yy,
            std::make_shared< ResizingChannelGroup >(
               leader->SharedPointer(), mPanel.GetLeftOffset() )
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

int TrackPanel::GetVRulerWidth() const
{
   return vrulerSize.x;
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
   return mAx->GetFocus().get();
}

Track *TrackPanel::GetFocusedTrack()
{
   return static_cast<Track*>( GetFocusedCell() );
}

void TrackPanel::SetFocusedCell()
{
   SetFocusedTrack( GetFocusedTrack() );
}

void TrackPanel::SetFocusedTrack( Track *t )
{
   // Make sure we always have the first linked track of a stereo track
   t = *GetTracks()->FindLeader(t);

   auto cell = mAx->SetFocus( Track::SharedPointer( t ) ).get();

   if (cell) {
      AudacityProject::CaptureKeyboard(this);
      Refresh( false );
   }
}

/**********************************************************************

  TrackInfo code is destined to move out of this file.

**********************************************************************/

namespace {

wxFont gFont;

std::unique_ptr<LWSlider>
   gGainCaptured
   , gPanCaptured
   , gGain
   , gPan
#ifdef EXPERIMENTAL_MIDI_OUT
   , gVelocityCaptured
   , gVelocity
#endif
;

}

void TrackInfo::ReCreateSliders( wxWindow *pParent ){
   const wxPoint point{ 0, 0 };
   wxRect sliderRect;
   GetGainRect(point, sliderRect);

   float defPos = 1.0;
   /* i18n-hint: Title of the Gain slider, used to adjust the volume */
   gGain = std::make_unique<LWSlider>(pParent, _("Gain"),
                        wxPoint(sliderRect.x, sliderRect.y),
                        wxSize(sliderRect.width, sliderRect.height),
                        DB_SLIDER);
   gGain->SetDefaultValue(defPos);

   gGainCaptured = std::make_unique<LWSlider>(pParent, _("Gain"),
                                wxPoint(sliderRect.x, sliderRect.y),
                                wxSize(sliderRect.width, sliderRect.height),
                                DB_SLIDER);
   gGainCaptured->SetDefaultValue(defPos);

   GetPanRect(point, sliderRect);

   defPos = 0.0;
   /* i18n-hint: Title of the Pan slider, used to move the sound left or right */
   gPan = std::make_unique<LWSlider>(pParent, _("Pan"),
                       wxPoint(sliderRect.x, sliderRect.y),
                       wxSize(sliderRect.width, sliderRect.height),
                       PAN_SLIDER);
   gPan->SetDefaultValue(defPos);

   gPanCaptured = std::make_unique<LWSlider>(pParent, _("Pan"),
                               wxPoint(sliderRect.x, sliderRect.y),
                               wxSize(sliderRect.width, sliderRect.height),
                               PAN_SLIDER);
   gPanCaptured->SetDefaultValue(defPos);

#ifdef EXPERIMENTAL_MIDI_OUT
   GetVelocityRect(point, sliderRect);

   /* i18n-hint: Title of the Velocity slider, used to adjust the volume of note tracks */
   gVelocity = std::make_unique<LWSlider>(pParent, _("Velocity"),
      wxPoint(sliderRect.x, sliderRect.y),
      wxSize(sliderRect.width, sliderRect.height),
      VEL_SLIDER);
   gVelocity->SetDefaultValue(0.0);
   gVelocityCaptured = std::make_unique<LWSlider>(pParent, _("Velocity"),
      wxPoint(sliderRect.x, sliderRect.y),
      wxSize(sliderRect.width, sliderRect.height),
      VEL_SLIDER);
   gVelocityCaptured->SetDefaultValue(0.0);
#endif

}

void TrackInfo::GetCloseBoxHorizontalBounds( const wxRect & rect, wxRect &dest )
{
   dest.x = rect.x;
   dest.width = kTrackInfoBtnSize;
}

void TrackInfo::GetCloseBoxRect(const wxRect & rect, wxRect & dest)
{
   GetCloseBoxHorizontalBounds( rect, dest );
   auto results = CalcItemY( commonTrackTCPLines, kItemBarButtons );
   dest.y = rect.y + results.first;
   dest.height = results.second;
}

static const int TitleSoloBorderOverlap = 1;

void TrackInfo::GetTitleBarHorizontalBounds( const wxRect & rect, wxRect &dest )
{
   // to right of CloseBoxRect, plus a little more
   wxRect closeRect;
   GetCloseBoxHorizontalBounds( rect, closeRect );
   dest.x = rect.x + closeRect.width + 1;
   dest.width = rect.x + rect.width - dest.x + TitleSoloBorderOverlap;
}

void TrackInfo::GetTitleBarRect(const wxRect & rect, wxRect & dest)
{
   GetTitleBarHorizontalBounds( rect, dest );
   auto results = CalcItemY( commonTrackTCPLines, kItemBarButtons );
   dest.y = rect.y + results.first;
   dest.height = results.second;
}

void TrackInfo::GetNarrowMuteHorizontalBounds( const wxRect & rect, wxRect &dest )
{
   dest.x = rect.x;
   dest.width = rect.width / 2 + 1;
}

void TrackInfo::GetNarrowSoloHorizontalBounds( const wxRect & rect, wxRect &dest )
{
   wxRect muteRect;
   GetNarrowMuteHorizontalBounds( rect, muteRect );
   dest.x = rect.x + muteRect.width;
   dest.width = rect.width - muteRect.width + TitleSoloBorderOverlap;
}

void TrackInfo::GetWideMuteSoloHorizontalBounds( const wxRect & rect, wxRect &dest )
{
   // Larger button, symmetrically placed intended.
   // On windows this gives 15 pixels each side.
   dest.width = rect.width - 2 * kTrackInfoBtnSize + 6;
   dest.x = rect.x + kTrackInfoBtnSize -3;
}

void TrackInfo::GetMuteSoloRect
(const wxRect & rect, wxRect & dest, bool solo, bool bHasSoloButton,
 const Track *pTrack)
{

   auto resultsM = CalcItemY( getTCPLines( *pTrack ), kItemMute );
   auto resultsS = CalcItemY( getTCPLines( *pTrack ), kItemSolo );
   dest.height = resultsS.second;

   int yMute = resultsM.first;
   int ySolo = resultsS.first;

   bool bSameRow = ( yMute == ySolo );
   bool bNarrow = bSameRow && bHasSoloButton;

   if( bNarrow )
   {
      if( solo )
         GetNarrowSoloHorizontalBounds( rect, dest );
      else
         GetNarrowMuteHorizontalBounds( rect, dest );
   }
   else
      GetWideMuteSoloHorizontalBounds( rect, dest );

   if( bSameRow || !solo )
      dest.y = rect.y + yMute;
   else
      dest.y = rect.y + ySolo;

}

void TrackInfo::GetSliderHorizontalBounds( const wxPoint &topleft, wxRect &dest )
{
   dest.x = topleft.x + 6;
   dest.width = kTrackInfoSliderWidth;
}

void TrackInfo::GetGainRect(const wxPoint &topleft, wxRect & dest)
{
   GetSliderHorizontalBounds( topleft, dest );
   auto results = CalcItemY( waveTrackTCPLines, kItemGain );
   dest.y = topleft.y + results.first;
   dest.height = results.second;
}

void TrackInfo::GetPanRect(const wxPoint &topleft, wxRect & dest)
{
   GetGainRect( topleft, dest );
   auto results = CalcItemY( waveTrackTCPLines, kItemPan );
   dest.y = topleft.y + results.first;
}

#ifdef EXPERIMENTAL_MIDI_OUT
void TrackInfo::GetVelocityRect(const wxPoint &topleft, wxRect & dest)
{
   GetSliderHorizontalBounds( topleft, dest );
   auto results = CalcItemY( noteTrackTCPLines, kItemVelocity );
   dest.y = topleft.y + results.first;
   dest.height = results.second;
}
#endif

void TrackInfo::GetMinimizeHorizontalBounds( const wxRect &rect, wxRect &dest )
{
   const int space = 0;// was 3.
   dest.x = rect.x + space;

   wxRect syncLockRect;
   GetSyncLockHorizontalBounds( rect, syncLockRect );

   // Width is rect.width less space on left for track select
   // and on right for sync-lock icon.
   dest.width = kTrackInfoBtnSize;
// rect.width - (space + syncLockRect.width);
}

void TrackInfo::GetMinimizeRect(const wxRect & rect, wxRect &dest)
{
   GetMinimizeHorizontalBounds( rect, dest );
   auto results = CalcBottomItemY
      ( commonTrackTCPBottomLines, kItemMinimize, rect.height);
   dest.y = rect.y + results.first;
   dest.height = results.second;
}

void TrackInfo::GetSelectButtonHorizontalBounds( const wxRect &rect, wxRect &dest )
{
   const int space = 0;// was 3.
   dest.x = rect.x + space;

   wxRect syncLockRect;
   GetSyncLockHorizontalBounds( rect, syncLockRect );
   wxRect minimizeRect;
   GetMinimizeHorizontalBounds( rect, minimizeRect );

   dest.x = dest.x + space + minimizeRect.width;
   // Width is rect.width less space on left for track select
   // and on right for sync-lock icon.
   dest.width = rect.width - (space + syncLockRect.width) - (space + minimizeRect.width);
}


void TrackInfo::GetSelectButtonRect(const wxRect & rect, wxRect &dest)
{
   GetSelectButtonHorizontalBounds( rect, dest );
   auto results = CalcBottomItemY
      ( commonTrackTCPBottomLines, kItemMinimize, rect.height);
   dest.y = rect.y + results.first;
   dest.height = results.second;
}

void TrackInfo::GetSyncLockHorizontalBounds( const wxRect &rect, wxRect &dest )
{
   dest.width = kTrackInfoBtnSize;
   dest.x = rect.x + rect.width - dest.width;
}

void TrackInfo::GetSyncLockIconRect(const wxRect & rect, wxRect &dest)
{
   GetSyncLockHorizontalBounds( rect, dest );
   auto results = CalcBottomItemY
      ( commonTrackTCPBottomLines, kItemSyncLock, rect.height);
   dest.y = rect.y + results.first;
   dest.height = results.second;
}

#ifdef USE_MIDI
void TrackInfo::GetMidiControlsHorizontalBounds
( const wxRect &rect, wxRect &dest )
{
   dest.x = rect.x + 1; // To center slightly
   // PRL: TODO: kMidiCellWidth is defined in terms of the other constant
   // kTrackInfoWidth but I am trying to avoid use of that constant.
   // Can cell width be computed from dest.width instead?
   dest.width = kMidiCellWidth * 4;
}

void TrackInfo::GetMidiControlsRect(const wxRect & rect, wxRect & dest)
{
   GetMidiControlsHorizontalBounds( rect, dest );
   auto results = CalcItemY( noteTrackTCPLines, kItemMidiControlsRect );
   dest.y = rect.y + results.first;
   dest.height = results.second;
}
#endif

/// \todo Probably should move to 'Utils.cpp'.
void TrackInfo::SetTrackInfoFont(wxDC * dc)
{
   dc->SetFont(gFont);
}

#if 0
void TrackInfo::DrawBordersWithin
   ( wxDC* dc, const wxRect & rect, const Track &track ) const
{
   AColor::Dark(dc, false); // same color as border of toolbars (ToolBar::OnPaint())

   // below close box and title bar
   wxRect buttonRect;
   GetTitleBarRect( rect, buttonRect );
   AColor::Line
      (*dc, rect.x,              buttonRect.y + buttonRect.height,
            rect.width - 1,      buttonRect.y + buttonRect.height);

   // between close box and title bar
   AColor::Line
      (*dc, buttonRect.x, buttonRect.y,
            buttonRect.x, buttonRect.y + buttonRect.height - 1);

   GetMuteSoloRect( rect, buttonRect, false, true, &track );

   bool bHasMuteSolo = dynamic_cast<const PlayableTrack*>( &track ) != NULL;
   if( bHasMuteSolo && !TrackInfo::HideTopItem( rect, buttonRect ) )
   {
      // above mute/solo
      AColor::Line
         (*dc, rect.x,          buttonRect.y,
               rect.width - 1,  buttonRect.y);

      // between mute/solo
      // Draw this little line; if there is no solo, wide mute button will
      // overpaint it later:
      AColor::Line
         (*dc, buttonRect.x + buttonRect.width, buttonRect.y,
               buttonRect.x + buttonRect.width, buttonRect.y + buttonRect.height - 1);

      // below mute/solo
      AColor::Line
         (*dc, rect.x,          buttonRect.y + buttonRect.height,
               rect.width - 1,  buttonRect.y + buttonRect.height);
   }

   // left of and above minimize button
   wxRect minimizeRect;
   this->GetMinimizeRect(rect, minimizeRect);
   AColor::Line
      (*dc, minimizeRect.x - 1, minimizeRect.y,
            minimizeRect.x - 1, minimizeRect.y + minimizeRect.height - 1);
   AColor::Line
      (*dc, minimizeRect.x,                          minimizeRect.y - 1,
            minimizeRect.x + minimizeRect.width - 1, minimizeRect.y - 1);
}
#endif

//#define USE_BEVELS

// Paint the whole given rectangle some fill color
void TrackInfo::DrawBackground(
   wxDC * dc, const wxRect & rect, bool bSelected, const int vrul)
{
   // fill in label
   wxRect fill = rect;
   fill.width = vrul - kLeftMargin;
   AColor::MediumTrackInfo(dc, bSelected);
   dc->DrawRectangle(fill);

#ifdef USE_BEVELS
   // This branch is not now used
   // PRL:  todo:  banish magic numbers.
   // PRL: vrul was the x coordinate of left edge of the vertical ruler.
   // PRL: bHasMuteSolo was true iff the track was WaveTrack.
   if( bHasMuteSolo )
   {
      int ylast = rect.height-20;
      int ybutton = wxMin(32,ylast-17);
      int ybuttonEnd = 67;

      fill=wxRect( rect.x+1, rect.y+17, vrul-6, ybutton);
      AColor::BevelTrackInfo( *dc, true, fill );
   
      if( ybuttonEnd < ylast ){
         fill=wxRect( rect.x+1, rect.y+ybuttonEnd, fill.width, ylast - ybuttonEnd);
         AColor::BevelTrackInfo( *dc, true, fill );
      }
   }
   else
   {
      fill=wxRect( rect.x+1, rect.y+17, vrul-6, rect.height-37);
      AColor::BevelTrackInfo( *dc, true, fill );
   }
#endif
}

namespace {
unsigned DefaultTrackHeight( const TCPLines &topLines )
{
   int needed =
      kTopMargin + kBottomMargin +
      totalTCPLines( topLines, true ) +
      totalTCPLines( commonTrackTCPBottomLines, false ) + 1;
   return (unsigned) std::max( needed, (int) Track::DefaultHeight );
}
}

unsigned TrackInfo::DefaultNoteTrackHeight()
{
   return DefaultTrackHeight( noteTrackTCPLines );
}

unsigned TrackInfo::DefaultWaveTrackHeight()
{
   return DefaultTrackHeight( waveTrackTCPLines );
}

LWSlider * TrackInfo::GainSlider
(const wxRect &sliderRect, const WaveTrack *t, bool captured, wxWindow *pParent)
{
   wxPoint pos = sliderRect.GetPosition();
   float gain = t ? t->GetGain() : 1.0;

   gGain->Move(pos);
   gGain->Set(gain);
   gGainCaptured->Move(pos);
   gGainCaptured->Set(gain);

   auto slider = (captured ? gGainCaptured : gGain).get();
   slider->SetParent( pParent ? pParent : ::GetActiveProject() );
   return slider;
}

LWSlider * TrackInfo::PanSlider
(const wxRect &sliderRect, const WaveTrack *t, bool captured, wxWindow *pParent)
{
   wxPoint pos = sliderRect.GetPosition();
   float pan = t ? t->GetPan() : 0.0;

   gPan->Move(pos);
   gPan->Set(pan);
   gPanCaptured->Move(pos);
   gPanCaptured->Set(pan);

   auto slider = (captured ? gPanCaptured : gPan).get();
   slider->SetParent( pParent ? pParent : ::GetActiveProject() );
   return slider;
}

#ifdef EXPERIMENTAL_MIDI_OUT
LWSlider * TrackInfo::VelocitySlider
(const wxRect &sliderRect, const NoteTrack *t, bool captured, wxWindow *pParent)
{
   wxPoint pos = sliderRect.GetPosition();
   float velocity = t ? t->GetVelocity() : 0.0;

   gVelocity->Move(pos);
   gVelocity->Set(velocity);
   gVelocityCaptured->Move(pos);
   gVelocityCaptured->Set(velocity);

   auto slider = (captured ? gVelocityCaptured : gVelocity).get();
   slider->SetParent( pParent ? pParent : ::GetActiveProject() );
   return slider;
}
#endif

void TrackInfo::UpdatePrefs( wxWindow *pParent )
{
   // Calculation of best font size depends on language, so it should be redone in case
   // the language preference changed.

   int fontSize = 10;
   gFont.Create(fontSize, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL);

   int allowableWidth =
      // PRL:  was it correct to include the margin?
      ( kTrackInfoWidth + kLeftMargin )
         - 2; // 2 to allow for left/right borders
   int textWidth, textHeight;
   do {
      gFont.SetPointSize(fontSize);
      pParent->GetTextExtent(_("Stereo, 999999Hz"),
                             &textWidth,
                             &textHeight,
                             NULL,
                             NULL,
                             &gFont);
      fontSize--;
   } while (textWidth >= allowableWidth);
}

static TrackPanel * TrackPanelFactory(wxWindow * parent,
   wxWindowID id,
   const wxPoint & pos,
   const wxSize & size,
   const std::shared_ptr<TrackList> &tracks,
   ViewInfo * viewInfo,
   TrackPanelListener * listener,
   AdornedRulerPanel * ruler)
{
   wxASSERT(parent); // to justify safenew
   return safenew TrackPanel(
      parent,
      id,
      pos,
      size,
      tracks,
      viewInfo,
      listener,
      ruler);
}


// Declare the static factory function.
// We defined it in the class.
TrackPanel *(*TrackPanel::FactoryFunction)(
              wxWindow * parent,
              wxWindowID id,
              const wxPoint & pos,
              const wxSize & size,
              const std::shared_ptr<TrackList> &tracks,
              ViewInfo * viewInfo,
              TrackPanelListener * listener,
              AdornedRulerPanel * ruler) = TrackPanelFactory;

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

unsigned TrackPanelCell::CaptureKey(wxKeyEvent &event, ViewInfo &, wxWindow *)
{
   event.Skip();
   return RefreshCode::RefreshNone;
}

unsigned TrackPanelCell::KeyDown(wxKeyEvent &event, ViewInfo &, wxWindow *)
{
   event.Skip();
   return RefreshCode::RefreshNone;
}

unsigned TrackPanelCell::KeyUp(wxKeyEvent &event, ViewInfo &, wxWindow *)
{
   event.Skip();
   return RefreshCode::RefreshNone;
}

unsigned TrackPanelCell::Char(wxKeyEvent &event, ViewInfo &, wxWindow *)
{
   event.Skip();
   return RefreshCode::RefreshNone;
}

IsVisibleTrack::IsVisibleTrack(AudacityProject *project)
   : mPanelRect {
        wxPoint{ 0, project->mViewInfo.vpos },
        project->GetTPTracksUsableArea()
     }
{}

bool IsVisibleTrack::operator () (const Track *pTrack) const
{
   // Need to return true if this track or a later channel intersects
   // the view
   return
   TrackList::Channels(pTrack).StartingWith(pTrack).any_of(
      [this]( const Track *pT ) {
         wxRect r(0, pT->GetY(), 1, pT->GetHeight());
         return r.Intersects(mPanelRect);
      }
   );
}
