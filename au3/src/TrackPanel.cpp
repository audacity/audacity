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

#include <wx/setup.h> // for wxUSE_* macros

#include "AdornedRulerPanel.h"
#include "tracks/ui/CommonTrackPanelCell.h"
#include "KeyboardCapture.h"
#include "PendingTracks.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "ProjectAudioManager.h"
#include "ProjectHistory.h"
#include "ProjectWindows.h"
#include "ProjectSettings.h"
#include "ProjectStatus.h"
#include "ProjectTimeRuler.h"
#include "ProjectWindow.h"
#include "SyncLock.h"
#include "Theme.h"
#include "TrackArt.h"
#include "TrackPanelMouseEvent.h"

#include "UndoManager.h"
#include "UIHandle.h"

#include "AColor.h"
#include "AllThemeResources.h"
#include "AudioIO.h"
#include "float_cast.h"

#include "Prefs.h"
#include "RefreshCode.h"
#include "TrackArtist.h"
#include "TrackPanelAx.h"
#include "TrackPanelResizerCell.h"
#include "Viewport.h"
#include "WaveTrack.h"

#include "FrameStatistics.h"

#include "tracks/ui/TrackControls.h"
#include "tracks/ui/ChannelView.h"
#include "tracks/ui/ChannelVRulerControls.h"

//This loads the appropriate set of cursors, depending on platform.
#include "../images/Cursors.h"

#include <algorithm>

#include <wx/dc.h>
#include <wx/dcclient.h>
#include <wx/graphics.h>

#include "RealtimeEffectManager.h"

static_assert(kVerticalPadding == kTopMargin + kBottomMargin);
static_assert(kTrackInfoTitleHeight + kTrackInfoTitleExtra == kAffordancesAreaHeight, "Drag bar is misaligned with the menu button");

/**

\class TrackPanel

This is a diagram of TrackPanel's division of one (non-stereo) track rectangle.
Total height equals ChannelView::GetHeight()'s value.  Total width is the wxWindow's
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
template< class A, class B, class DIST > bool within(A a, B b, DIST d)
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
std::unique_ptr<wxCursor> MakeCursor(int WXUNUSED(CursorId), const char* const pXpm[36],  int HotX, int HotY)
{
#define CURSORS_SIZE32
#ifdef CURSORS_SIZE32
    const int HotAdjust =0;
#else
    const int HotAdjust =8;
#endif

    wxImage Image = wxImage(wxBitmap(pXpm).ConvertToImage());
    Image.SetMaskColour(255, 0, 0);
    Image.SetMask();// Enable mask.

    Image.SetOption(wxIMAGE_OPTION_CUR_HOTSPOT_X, HotX - HotAdjust);
    Image.SetOption(wxIMAGE_OPTION_CUR_HOTSPOT_Y, HotY - HotAdjust);
    return std::make_unique<wxCursor>(Image);
}

namespace {
AttachedWindows::RegisteredFactory sKey{
    []( AudacityProject& project ) -> wxWeakRef< wxWindow > {
        auto& ruler = AdornedRulerPanel::Get(project);
        auto& viewInfo = ViewInfo::Get(project);
        auto& window = ProjectWindow::Get(project);
        auto mainPage = window.GetTrackListWindow();
        wxASSERT(mainPage); // to justify safenew

        auto& tracks = TrackList::Get(project);
        auto result = safenew TrackPanel(mainPage,
                                         window.NextWindowID(),
                                         wxDefaultPosition,
                                         wxDefaultSize,
                                         tracks.shared_from_this(),
                                         &viewInfo,
                                         &project,
                                         &ruler);
        SetProjectPanel(project, *result);
        return result;
    }
};
}

TrackPanel& TrackPanel::Get(AudacityProject& project)
{
    return GetAttachedWindows(project).Get< TrackPanel >(sKey);
}

const TrackPanel& TrackPanel::Get(const AudacityProject& project)
{
    return Get(const_cast< AudacityProject& >(project));
}

void TrackPanel::Destroy(AudacityProject& project)
{
    auto* pPanel = GetAttachedWindows(project).Find<TrackPanel>(sKey);
    if (pPanel) {
        pPanel->wxWindow::Destroy();
        GetAttachedWindows(project).Assign(sKey, nullptr);
    }
}

// Don't warn us about using 'this' in the base member initializer list.
#ifndef __WXGTK__ //Get rid if this pragma for gtk
#pragma warning( disable: 4355 )
#endif
TrackPanel::TrackPanel(wxWindow* parent, wxWindowID id,
                       const wxPoint& pos,
                       const wxSize& size,
                       const std::shared_ptr<TrackList>& tracks,
                       ViewInfo* viewInfo,
                       AudacityProject* project,
                       AdornedRulerPanel* ruler)
    : CellularPanel(parent, id, pos, size, viewInfo,
                    wxWANTS_CHARS | wxNO_BORDER),
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

#if wxUSE_ACCESSIBILITY
    // Inject finder of track rectangles into the accessibility helper
    {
        const auto finder = [
            weakThis = wxWeakRef<TrackPanel> { this }
                            ] (const Track& track) -> wxRect {
            if (weakThis) {
                return weakThis->FindTrackRect(&track);
            }
            return {};
        };
        auto& focus = TrackFocus::Get(*GetProject());
        auto& viewport = Viewport::Get(*GetProject());
        TrackPanelAx* pAx{};
        SetAccessible(pAx
                          =safenew TrackPanelAx {
            viewport.weak_from_this(), focus.weak_from_this(), finder });
        focus.SetCallbacks(std::make_unique<TrackPanelAx::Adapter>(pAx));
    }
#endif

    mTrackArtist = std::make_unique<TrackArtist>(this);

    mTimeCount = 0;
    mTimer.parent = this;
    // Timer is started after the window is visible
    ProjectWindow::Get(*GetProject()).Bind(wxEVT_IDLE,
                                           &TrackPanel::OnIdle, this);

    // Register for tracklist updates
    mTrackListSubscription = PendingTracks::Get(*GetProject())
                             .Subscribe([this](const TrackListEvent& event){
        switch (event.mType) {
            case TrackListEvent::RESIZING:
            case TrackListEvent::ADDITION:
                OnTrackListResizing(event);
                break;
            case TrackListEvent::DELETION:
                OnTrackListDeletion();
                break;
            default:
                break;
        }
    });

    auto theProject = GetProject();
    mSyncLockSubscription = SyncLockState::Get(*theProject)
                            .Subscribe(*this, &TrackPanel::OnSyncLockChange);

    mFocusChangeSubscription = TrackFocus::Get(*theProject)
                               .Subscribe(*this, &TrackPanel::OnTrackFocusChange);

    mUndoSubscription = UndoManager::Get(*theProject)
                        .Subscribe(*this, &TrackPanel::OnUndoReset);

    mAudioIOSubscription
        =AudioIO::Get()->Subscribe(*this, &TrackPanel::OnAudioIO);

    mRealtimeEffectManagerSubscription = RealtimeEffectManager::Get(*theProject)
                                         .Subscribe([this](const RealtimeEffectManagerMessage& msg)
    {
        if (auto pTrack = dynamic_cast<Track*>(msg.group)) {
            //update "effects" button
            RefreshTrack(pTrack);
        }
    });

    mProjectRulerInvalidatedSubscription
        =ProjectTimeRuler::Get(*theProject).GetRuler().Subscribe([this](auto mode) { Refresh(); });
    mSelectionSubscription = viewInfo->selectedRegion
                             .Subscribe([this](auto&){ Refresh(false); });

    UpdatePrefs();
}

TrackPanel::~TrackPanel()
{
    mTimer.Stop();

    // This can happen if a label is being edited and the user presses
    // ALT+F4 or Command+Q
    if (HasCapture()) {
        ReleaseMouse();
    }
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
AudacityProject* TrackPanel::GetProject() const
{
    auto window = GetParent();

    while (window != nullptr)
    {
        if (const auto projectWindow = dynamic_cast<ProjectWindow*>(window)) {
            return projectWindow->FindProject().get();
        }

        window = window->GetParent();
    }
    return nullptr;
}

void TrackPanel::OnSize(wxSizeEvent& evt)
{
    evt.Skip();
    const auto& size = evt.GetSize();
    mViewInfo->SetWidth(size.GetWidth());
    mViewInfo->SetHeight(size.GetHeight());
}

void TrackPanel::OnIdle(wxIdleEvent& event)
{
    event.Skip();
    // The window must be ready when the timer fires (#1401)
    if (IsShownOnScreen()) {
        mTimer.Start(std::chrono::milliseconds { kTimerInterval }.count(), FALSE);

        // Timer is started, we don't need the event anymore
        GetProjectFrame(*GetProject()).Unbind(wxEVT_IDLE,
                                              &TrackPanel::OnIdle, this);
    } else {
        // Get another idle event, wx only guarantees we get one
        // event after "some other normal events occur"
        event.RequestMore();
    }
}

/// AS: This gets called on our wx timer events.
void TrackPanel::OnTimer(wxTimerEvent&)
{
    mTimeCount++;

    AudacityProject* const p = GetProject();
    auto& window = ProjectWindow::Get(*p);
    auto& viewport = Viewport::Get(*p);

    auto& projectAudioIO = ProjectAudioIO::Get(*p);
    auto gAudioIO = AudioIO::Get();

    // Check whether we were playing or recording, but the stream has stopped.
    if (projectAudioIO.GetAudioIOToken() > 0 && !IsAudioActive()) {
        //the stream may have been started up after this one finished (by some other project)
        //in that case reset the buttons don't stop the stream
        auto& projectAudioManager = ProjectAudioManager::Get(*p);
        projectAudioManager.Stop(!gAudioIO->IsStreamActive());
    }

    // Next, check to see if we were playing or recording
    // audio, but now Audio I/O is completely finished.
    if (projectAudioIO.GetAudioIOToken() > 0
        && !gAudioIO->IsAudioTokenActive(projectAudioIO.GetAudioIOToken())) {
        projectAudioIO.SetAudioIOToken(0);
        viewport.Redraw();
    }
    if (mLastDrawnSelectedRegion != mViewInfo->selectedRegion) {
        UpdateSelectionDisplay();
    }

    // Notify listeners for timer ticks
    window.GetPlaybackScroller().OnTimer();

    DrawOverlays(false);
    mRuler->DrawOverlays(false);

    if (IsAudioActive() && gAudioIO->GetNumCaptureChannels()) {
        // Periodically update the display while recording

        if ((mTimeCount % 5) == 0) {
            // Must tell OnPaint() to recreate the backing bitmap
            // since we've not done a full refresh.
            mRefreshBacking = true;
            Refresh(false);
        }
    }
    if (mTimeCount > 1000) {
        mTimeCount = 0;
    }
}

void TrackPanel::OnSyncLockChange(SyncLockChangeMessage)
{
    Refresh(false);
}

void TrackPanel::OnUndoReset(UndoRedoMessage message)
{
    if (message.type == UndoRedoMessage::Reset) {
        TrackFocus::Get(*GetProject()).Set(nullptr);
        Refresh(false);
    }
}

/// AS: OnPaint( ) is called during the normal course of
///  completing a repaint operation.
void TrackPanel::OnPaint(wxPaintEvent& /* event */)
{
    // If the selected region changes - we must repaint the tracks, because the
    // selection is baked into track image
    if (mLastDrawnSelectedRegion != mViewInfo->selectedRegion) {
        mRefreshBacking = true;
        mLastDrawnSelectedRegion = mViewInfo->selectedRegion;
    }

    auto sw
        =FrameStatistics::CreateStopwatch(FrameStatistics::SectionID::TrackPanel);

    {
        wxPaintDC dc(this);

        // Retrieve the damage rectangle
        wxRect box = GetUpdateRegion().GetBox();

        // Recreate the backing bitmap if we have a full refresh
        // (See TrackPanel::Refresh())
        if (mRefreshBacking || (box == GetRect())) {
            // Reset (should a mutex be used???)
            mRefreshBacking = false;

            // Redraw the backing bitmap
            DrawTracks(&GetBackingDCForRepaint());

            // Copy it to the display
            DisplayBitmap(dc);
        } else {
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
}

void TrackPanel::MakeParentRedrawScrollbars()
{
    Viewport::Get(*GetProject()).UpdateScrollbarsForTracks();
}

namespace {
std::shared_ptr<Track> FindTrack(TrackPanelCell* pCell)
{
    if (pCell) {
        // FindTrack as applied through the CommonTrackPanelCell interface
        // will really find a track, though for now it finds a left or right
        // channel.
        return static_cast<CommonTrackPanelCell*>(pCell)->FindTrack();
    }
    return {};
}
}

void TrackPanel::ProcessUIHandleResult
    (TrackPanelCell* pClickedCell, TrackPanelCell* pLatestCell,
    UIHandle::Result refreshResult)
{
    const auto panel = this;
    auto pLatestTrack = FindTrack(pLatestCell).get();

    // This precaution checks that the track is not only nonnull, but also
    // really owned by the track list
    auto pClickedTrack = GetTracks()->Lock(
        std::weak_ptr<Track> { FindTrack(pClickedCell) }
        ).get();

    // TODO:  make a finer distinction between refreshing the track control area,
    // and the waveform area.  As it is, redraw both whenever you must redraw either.

    // Copy data from the underlying tracks to the pending tracks that are
    // really displayed
    PendingTracks::Get(*panel->GetProject()).UpdatePendingTracks();

    using namespace RefreshCode;

    if (refreshResult & DestroyedCell) {
        panel->UpdateViewIfNoTracks();
        // Beware stale pointer!
        if (pLatestTrack == pClickedTrack) {
            pLatestTrack = nullptr;
        }
        pClickedTrack = nullptr;
    }

    if (pClickedTrack && (refreshResult & RefreshCode::UpdateVRuler)) {
        panel->UpdateVRuler(pClickedTrack);
    }

    if (refreshResult & RefreshCode::DrawOverlays) {
        panel->DrawOverlays(false);
        mRuler->DrawOverlays(false);
    }

    // Refresh all if told to do so, or if told to refresh a track that
    // is not known.
    const bool refreshAll
        =((refreshResult & RefreshAll)
          || ((refreshResult & RefreshCell) && !pClickedTrack)
          || ((refreshResult & RefreshLatestCell) && !pLatestTrack));

    if (refreshAll) {
        panel->Refresh(false);
    } else {
        if (refreshResult & RefreshCell) {
            panel->RefreshTrack(pClickedTrack);
        }
        if (refreshResult & RefreshLatestCell) {
            panel->RefreshTrack(pLatestTrack);
        }
    }

    if (refreshResult & FixScrollbars) {
        panel->MakeParentRedrawScrollbars();
    }

    if (refreshResult & Resize) {
        Viewport::Get(*GetProject()).HandleResize();
    }

    if ((refreshResult & RefreshCode::EnsureVisible) && pClickedTrack) {
        auto& focus = TrackFocus::Get(*GetProject());
        focus.Set(pClickedTrack);
        if (const auto pFocus = focus.Get()) {
            Viewport::Get(*GetProject()).ShowTrack(*pFocus);
        }
    }
}

void TrackPanel::HandlePageUpKey()
{
    Viewport::Get(*GetProject())
    .SetHorizontalThumb(2 * mViewInfo->hpos - mViewInfo->GetScreenEndTime());
}

void TrackPanel::HandlePageDownKey()
{
    Viewport::Get(*GetProject())
    .SetHorizontalThumb(mViewInfo->GetScreenEndTime());
}

bool TrackPanel::IsAudioActive()
{
    AudacityProject* p = GetProject();
    return ProjectAudioIO::Get(*p).IsAudioActive();
}

void TrackPanel::UpdateStatusMessage(const TranslatableString& st)
{
    auto status = st;
    if (HasEscape()) {
        /* i18n-hint Esc is a key on the keyboard */
        status.Join(XO("(Esc to cancel)"), " ");
    }
    ProjectStatus::Get(*GetProject()).Set(status);
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
    return GetTracks()->Selected().size();
}

void TrackPanel::UpdateViewIfNoTracks()
{
    if (mTracks->empty()) {
        // BG: There are no more tracks on screen
        //BG: Set zoom to normal
        mViewInfo->SetZoom(ZoomInfo::GetDefaultZoom());

        //STM: Set selection to 0,0
        //PRL: and default the rest of the selection information
        mViewInfo->selectedRegion = SelectedRegion();

        // PRL:  Following causes the time ruler to align 0 with left edge.
        // Bug 972
        mViewInfo->hpos = 0;

        Viewport::Get(*GetProject()).HandleResize();
        //STM: Clear message if all tracks are removed
        ProjectStatus::Get(*GetProject()).Set({});
    }
}

// The tracks positions within the list have changed, so update the vertical
// ruler size for the track that triggered the event.
void TrackPanel::OnTrackListResizing(const TrackListEvent& e)
{
    auto t = e.mpTrack.lock();
    // A deleted track can trigger the event.  In which case do nothing here.
    // A deleted track can have a valid pointer but no owner, bug 2060
    if (t && t->HasOwner()) {
        UpdateVRuler(t.get());
    }

    // fix for bug 2477
    MakeParentRedrawScrollbars();
}

// Tracks have been removed from the list.
void TrackPanel::OnTrackListDeletion()
{
    // copy shared_ptr for safety, as in HandleClick
    auto handle = Target();
    if (handle) {
        handle->OnProjectChange(GetProject());
    }

    // If the focused track disappeared but there are still other tracks,
    // this reassigns focus.
    TrackFocus(*GetProject()).Get();

    UpdateVRulerSize();
}

void TrackPanel::OnKeyDown(wxKeyEvent& event)
{
    switch (event.GetKeyCode()) {
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

void TrackPanel::OnMouseEvent(wxMouseEvent& event)
{
    if (event.LeftDown()) {
        // wxTimers seem to be a little unreliable, so this
        // "primes" it to make sure it keeps going for a while...

        // When this timer fires, we call TrackPanel::OnTimer and
        // possibly update the screen for offscreen scrolling.
        mTimer.Stop();
        mTimer.Start(std::chrono::milliseconds { kTimerInterval }.count(), FALSE);
    }

    if (event.ButtonUp()) {
        //ShowTrack should be called after processing the up-click.
        this->CallAfter([this, event]{
            const auto foundCell = FindCell(event.m_x, event.m_y);
            const auto t = FindTrack(foundCell.pCell.get());
            if (t) {
                auto& focus = TrackFocus::Get(*GetProject());
                focus.Set(t.get());
                Viewport::Get(*GetProject()).ShowTrack(*t);
            }
        });
    }

    // Must also fall through to base class handler
    event.Skip();
}

double TrackPanel::GetMostRecentXPos()
{
    return mViewInfo->PositionToTime(
        MostRecentXCoord(), mViewInfo->GetLeftOffset());
}

void TrackPanel::RefreshTrack(Track* trk, bool refreshbacking)
{
    if (!trk) {
        return;
    }

    auto height = ChannelView::GetChannelGroupHeight(trk);

    // Set rectangle top according to the scrolling position, `vpos`
    // Subtract the inset (above) and shadow (below) from the height of the
    // rectangle, but not the border
    // This matters because some separators do paint over the border
    auto& view = ChannelView::Get(*trk->GetChannel(0));
    const auto top
        =-mViewInfo->vpos + view.GetCumulativeHeightBefore() + kTopInset;
    height -= (kTopInset + kShadowThickness);

    // Width also subtracts insets (left and right) plus shadow (right)
    const auto left = kLeftInset;
    const auto width = GetRect().GetWidth()
                       - (kLeftInset + kRightInset + kShadowThickness);

    wxRect rect(left, top, width, height);

    if (refreshbacking) {
        mRefreshBacking = true;
    }

    Refresh(false, &rect);
}

/// This method overrides Refresh() of wxWindow so that the
/// boolean play indicator can be set to false, so that an old play indicator that is
/// no longer there won't get  XORed (to erase it), thus redrawing it on the
/// TrackPanel
void TrackPanel::Refresh(bool eraseBackground /* = TRUE */,
                         const wxRect* rect /* = NULL */)
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
    if (!rect || (*rect == GetRect())) {
        mRefreshBacking = true;
    }
    wxWindow::Refresh(eraseBackground, rect);

    CallAfter([this]{
        if (GetProject()) {
            CellularPanel::HandleCursorForPresentMouseState();
        }
    });
}

void TrackPanel::OnAudioIO(AudioIOEvent evt)
{
    if (evt.type == AudioIOEvent::MONITOR || evt.type == AudioIOEvent::PAUSE) {
        return;
    }
    // Some hit tests want to change their cursor to and from the ban symbol
    CallAfter([this]{ CellularPanel::HandleCursorForPresentMouseState(); });
}

#include "TrackPanelDrawingContext.h"

/// Draw the actual track areas.  We only draw the borders
/// and the little buttons and menues and whatnot here, the
/// actual contents of each track are drawn by the TrackArtist.
void TrackPanel::DrawTracks(wxDC* dc)
{
    wxRegion region = GetUpdateRegion();

    const wxRect clip = GetRect();

    const SelectedRegion& sr = mViewInfo->selectedRegion;
    mTrackArtist->pSelectedRegion = &sr;
    const auto& pendingTracks = PendingTracks::Get(*GetProject());
    mTrackArtist->pPendingTracks = &pendingTracks;
    mTrackArtist->pZoomInfo = mViewInfo;
    TrackPanelDrawingContext context {
        *dc, Target(), mLastMouseState, mTrackArtist.get()
    };

    // Don't draw a bottom margin here.

    const auto& settings = ProjectSettings::Get(*GetProject());
    bool bMultiToolDown
        =(ToolCodes::multiTool == settings.GetTool());
    bool envelopeFlag
        =bMultiToolDown || (ToolCodes::envelopeTool == settings.GetTool());
    bool bigPointsFlag
        =bMultiToolDown || (ToolCodes::drawTool == settings.GetTool());
    bool sliderFlag     = bMultiToolDown;
    bool brushFlag   = false;
#ifdef EXPERIMENTAL_BRUSH_TOOL
    brushFlag   = (ToolCodes::brushTool == settings.GetTool());
#endif

    const bool hasSolo = GetTracks()->Any<PlayableTrack>()
                         .any_of([&](const PlayableTrack* pt) {
        pt = static_cast<const PlayableTrack*>(
            &pendingTracks.SubstitutePendingChangedTrack(*pt));
        return pt->GetSolo();
    });

    mTrackArtist->drawEnvelope = envelopeFlag;
    mTrackArtist->bigPoints = bigPointsFlag;
    mTrackArtist->drawSliders = sliderFlag;
    mTrackArtist->onBrushTool = brushFlag;
    mTrackArtist->hasSolo = hasSolo;

    this->CellularPanel::Draw(context, TrackArtist::NPasses);
}

void TrackPanel::SetBackgroundCell
    (const std::shared_ptr< CommonTrackPanelCell >& pCell)
{
    mpBackground = pCell;
}

std::shared_ptr< CommonTrackPanelCell > TrackPanel::GetBackgroundCell()
{
    return mpBackground;
}

namespace {
std::vector<int> FindAdjustedChannelHeights(Track& t)
{
    auto channels = t.Channels();
    assert(!channels.empty());

    // Collect heights, and count affordances
    int nAffordances = 0;
    int totalHeight = 0;
    std::vector<int> oldHeights;
    for (auto pChannel : channels) {
        auto& view = ChannelView::Get(*pChannel);
        const auto height = view.GetHeight();
        totalHeight += height;
        oldHeights.push_back(height);
        if (view.GetAffordanceControls()) {
            ++nAffordances;
        }
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
    for (const auto& oldHeight : oldHeights) {
        // Preserve the porportions among the stored heights
        cumulativeOldHeight += oldHeight;
        const auto newHeight
            =cumulativeOldHeight * availableHeight / totalHeight
              - cumulativeNewHeight;
        cumulativeNewHeight += newHeight;
        results.push_back(newHeight);
    }

    return results;
}
}

void TrackPanel::UpdateVRulers()
{
    for (auto t : GetTracks()->Any<WaveTrack>()) {
        UpdateTrackVRuler(*t);
    }

    UpdateVRulerSize();
}

void TrackPanel::UpdateVRuler(Track* t)
{
    if (t) {
        UpdateTrackVRuler(*t);
    }

    UpdateVRulerSize();
}

void TrackPanel::UpdateTrackVRuler(Track& t)
{
    auto heights = FindAdjustedChannelHeights(t);

    wxRect rect(mViewInfo->GetVRulerOffset(),
                0,
                mViewInfo->GetVRulerWidth(),
                0);

    auto pHeight = heights.begin();
    for (auto pChannel : t.Channels()) {
        auto& view = ChannelView::Get(*pChannel);
        const auto height = *pHeight++;
        rect.SetHeight(height);
        const auto subViews = view.GetSubViews(rect);
        if (subViews.empty()) {
            continue;
        }

        auto iter = subViews.begin(), end = subViews.end(), next = iter;
        auto yy = iter->first;
        wxSize vRulerSize{ 0, 0 };
        auto& size = view.vrulerSize;
        for (; iter != end; iter = next) {
            ++next;
            auto nextY = (next == end)
                         ? height
                         : next->first;
            rect.SetHeight(nextY - yy);
            // This causes ruler size in the track to be reassigned:
            ChannelVRulerControls::Get(*iter->second).UpdateRuler(rect);
            // But we want to know the maximum width and height over all sub-views:
            vRulerSize.IncTo({ size.first, size.second });
            yy = nextY;
        }
        size = { vRulerSize.x, vRulerSize.y };
    }
}

void TrackPanel::UpdateVRulerSize()
{
    auto trackRange = GetTracks()->Any();
    if (trackRange) {
        wxSize s{ 0, 0 };
        // Find maximum width over all channels
        for (auto t : trackRange) {
            for (auto pChannel : t->Channels()) {
                const auto& size = ChannelView::Get(*pChannel).vrulerSize;
                s.IncTo({ size.first, size.second });
            }
        }

        if (mViewInfo->GetVRulerWidth() != s.GetWidth()) {
            mViewInfo->SetVRulerWidth(s.GetWidth());
            mRuler->SetLeftOffset(
                mViewInfo->GetLeftOffset()); // bevel on AdornedRuler
            mRuler->Refresh();
        }
    }
    Refresh(false);
}

void TrackPanel::OnTrackMenu(Track* t)
{
    CellularPanel::DoContextMenu(
        t ? ChannelView::Get(*t->GetChannel(0)).shared_from_this() : nullptr);
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

Track& GetTrack(Channel& channel)
{
    // It is assumed that all channels we ever see are in groups that are
    // also Tracks
    return static_cast<Track&>(channel.GetChannelGroup());
}

const Track& GetTrack(const Channel& channel)
{
    // It is assumed that all channels we ever see are in groups that are
    // also Tracks
    return static_cast<const Track&>(channel.GetChannelGroup());
}

void GetTrackNameExtent(
    wxDC& dc, const Channel& channel, wxCoord* pW, wxCoord* pH)
{
    wxFont labelFont(12, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL);
    dc.SetFont(labelFont);
    dc.GetTextExtent(GetTrack(channel).GetName(), pW, pH);
}

wxRect GetTrackNameRect(
    int leftOffset,
    const wxRect& trackRect, wxCoord textWidth, wxCoord textHeight)
{
    return {
        leftOffset + DisplaceX,
        trackRect.y + DisplaceY,
        textWidth + MarginsX,
        textHeight + MarginsY
    };
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
        const TrackPanelMouseState&, const AudacityProject*) override
    { return {}; }
    virtual std::shared_ptr< Track > DoFindTrack() override { return {}; }
    static std::shared_ptr<EmptyCell> Instance()
    {
        static auto instance = std::make_shared< EmptyCell >();
        return instance;
    }

    // TrackPanelDrawable implementation
    void Draw(
        TrackPanelDrawingContext& context,
        const wxRect& rect, unsigned iPass) override
    {
        if (iPass == TrackArtist::PassMargins) {
            // Draw a margin area of TrackPanel
            auto dc = &context.dc;

            AColor::TrackPanelBackground(dc, false);
            dc->DrawRectangle(rect);
        }
    }
};

// A vertical ruler left of a channel
struct VRulerAndChannel final : TrackPanelGroup {
    VRulerAndChannel(
        const std::shared_ptr<ChannelView>& pView, wxCoord leftOffset)
        : mpView{pView}, mLeftOffset{leftOffset} {}
    Subdivision Children(const wxRect& rect) override
    {
        return { Axis::X, Refinement{
                     { rect.GetLeft(),
                       ChannelVRulerControls::Get(*mpView).shared_from_this() },
                     { mLeftOffset, mpView }
                 } };
    }

    std::shared_ptr<ChannelView> mpView;
    wxCoord mLeftOffset;
};

// One or more sub-views of one channel, stacked vertically, each containing
// a vertical ruler and a channel
struct VRulersAndChannels final : TrackPanelGroup {
    VRulersAndChannels(
        const std::shared_ptr<Channel>& pChannel,
        ChannelView::Refinement refinement, wxCoord leftOffset)
        : mpChannel{pChannel}
        , mRefinement{std::move(refinement)}
        , mLeftOffset{leftOffset} {}
    Subdivision Children(const wxRect& rect) override
    {
        Refinement refinement;
        auto y1 = rect.GetTop();
        for ( const auto& subView : mRefinement ) {
            y1 = std::max(y1, subView.first);
            refinement.emplace_back(y1,
                                    std::make_shared< VRulerAndChannel >(
                                        subView.second, mLeftOffset));
        }
        return { Axis::Y, std::move(refinement) };
    }

    // TrackPanelDrawable implementation
    void Draw(
        TrackPanelDrawingContext& context,
        const wxRect& rect, unsigned iPass) override
    {
        // This overpaints the track area, but sometimes too the stereo channel
        // separator, so draw at least later than that

        if (iPass == TrackArtist::PassBorders) {
            if (mRefinement.size() > 1) {
                // Draw lines separating sub-views
                auto& dc = context.dc;
                AColor::CursorColor(&dc);
                auto iter = mRefinement.begin() + 1, end = mRefinement.end();
                for (; iter != end; ++iter ) {
                    auto yy = iter->first;
                    AColor::Line(dc, rect.x, yy, mLeftOffset - 1, yy);
                    AColor::Line(dc, mLeftOffset, yy, rect.GetRight(), yy);
                }
            }
        }
    }

    std::shared_ptr<Channel> mpChannel;
    ChannelView::Refinement mRefinement;
    wxCoord mLeftOffset;
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

// optional affordance area, and n channels with vertical rulers,
// alternating with n - 1 resizers;
// each channel-ruler pair might be divided into multiple views
struct ChannelStack final : TrackPanelGroup {
    ChannelStack(const std::shared_ptr<Track>& pTrack, wxCoord leftOffset)
        : mpTrack{pTrack}, mLeftOffset{leftOffset} {}
    Subdivision Children(const wxRect& rect_) override
    {
        auto rect = rect_;
        Refinement refinement;

        const auto channels = mpTrack->Channels();
        const auto pLast = *channels.rbegin();
        wxCoord yy = rect.GetTop();
        auto heights = FindAdjustedChannelHeights(*mpTrack);
        auto pHeight = heights.begin();
        for (auto pChannel : channels) {
            auto& view = ChannelView::Get(*pChannel);
            if (auto affordance = view.GetAffordanceControls()) {
                Refinement hgroup {
                    std::make_pair(mLeftOffset, affordance)
                };
                refinement
                .emplace_back(yy, std::make_shared<HorizontalGroup>(hgroup));
                yy += kAffordancesAreaHeight;
            }

            auto height = *pHeight++;
            rect.SetTop(yy);
            rect.SetHeight(height - kChannelSeparatorThickness);
            refinement.emplace_back(yy,
                                    std::make_shared<VRulersAndChannels>(pChannel,
                                                                         ChannelView::Get(*pChannel).GetSubViews(rect),
                                                                         mLeftOffset));
            if (pChannel != pLast) {
                yy += height;
                refinement.emplace_back(
                    yy - kChannelSeparatorThickness,
                    TrackPanelResizerCell::Get(*pChannel)
                    .shared_from_this());
            }
        }

        return { Axis::Y, std::move(refinement) };
    }

    void Draw(TrackPanelDrawingContext& context,
              const wxRect& rect, unsigned iPass) override
    {
        TrackPanelGroup::Draw(context, rect, iPass);
        if (iPass == TrackArtist::PassTracks) {
            auto vRulerRect = rect;
            vRulerRect.width = mLeftOffset - rect.x;

            auto dc = &context.dc;

            // Paint the background;
            AColor::MediumTrackInfo(dc, mpTrack->GetSelected());
            dc->DrawRectangle(vRulerRect);

            const auto channels = mpTrack->Channels();
            auto& view = ChannelView::Get(**channels.begin());
            if (auto affordance = view.GetAffordanceControls()) {
                const auto yy = vRulerRect.y + kAffordancesAreaHeight - 1;
                AColor::Dark(dc, false);
                AColor::Line(*dc, vRulerRect.GetLeft(), yy, vRulerRect.GetRight(), yy);
            }

            // Stroke left and right borders
            dc->SetPen(*wxBLACK_PEN);

            AColor::Line(*dc, vRulerRect.GetLeftTop(), vRulerRect.GetLeftBottom());
            AColor::Line(*dc, vRulerRect.GetRightTop(), vRulerRect.GetRightBottom());
        }
        if (iPass == TrackArtist::PassFocus && mpTrack->IsSelected()) {
            const auto channels = mpTrack->Channels();
            const auto pLast = *channels.rbegin();
            wxCoord yy = rect.GetTop();
            auto heights = FindAdjustedChannelHeights(*mpTrack);
            auto pHeight = heights.begin();
            for (auto pChannel : channels) {
                auto& view = ChannelView::Get(*pChannel);
                auto height = *pHeight++;
                if (auto affordance = view.GetAffordanceControls()) {
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

    const std::shared_ptr<Track> mpTrack;
    wxCoord mLeftOffset;
};

// A track control panel, left of n vertical rulers and n channels
// alternating with n - 1 resizers
struct LabeledChannelGroup final : TrackPanelGroup {
    LabeledChannelGroup(
        const std::shared_ptr<Track>& pTrack, wxCoord leftOffset)
        : mpTrack{pTrack}, mLeftOffset{leftOffset} {}
    Subdivision Children(const wxRect& rect) override
    {
        return { Axis::X, Refinement{
                     { rect.GetLeft(),
                       TrackControls::Get(*mpTrack).shared_from_this() },
                     { rect.GetLeft() + kTrackInfoWidth,
                       std::make_shared<ChannelStack>(mpTrack, mLeftOffset) }
                 } };
    }

    // TrackPanelDrawable implementation
    void Draw(TrackPanelDrawingContext& context,
              const wxRect& rect, unsigned iPass) override
    {
        if (iPass == TrackArtist::PassBorders) {
            auto& dc = context.dc;
            dc.SetBrush(*wxTRANSPARENT_BRUSH);
            dc.SetPen(*wxBLACK_PEN);

            // border
            dc.DrawRectangle(
                rect.x, rect.y,
                rect.width - kShadowThickness, rect.height - kShadowThickness
                );

            // shadow
            if constexpr (kShadowThickness > 0) {
                // Stroke lines along bottom and right, which are slightly short at
                // bottom-left and top-right
                const auto right = rect.GetRight();
                const auto bottom = rect.GetBottom();

                // bottom
                AColor::Line(dc, rect.x + 2, bottom, right, bottom);
                // right
                AColor::Line(dc, right, rect.y + 2, right, bottom);
            }
        }
        if (iPass == TrackArtist::PassFocus) {
            // Sometimes highlight is not drawn on backing bitmap. I thought
            // it was because FindFocus did not return the TrackPanel on Mac, but
            // when I removed that test, yielding this condition:
            //     if (GetFocusedTrack() != NULL) {
            // the highlight was reportedly drawn even when something else
            // was the focus and no highlight should be drawn. -RBD
            const auto artist = TrackArtist::Get(context);
            auto& trackPanel = *artist->parent;
            auto& trackFocus = TrackFocus::Get(*trackPanel.GetProject());
            if (trackFocus.Get() == mpTrack.get()
                && wxWindow::FindFocus() == &trackPanel) {
                /// Draw a three-level highlight gradient around the focused track.
                wxRect theRect = rect;
                auto& dc = context.dc;
                dc.SetBrush(*wxTRANSPARENT_BRUSH);

                AColor::TrackFocusPen(&dc, 2);
                dc.DrawRectangle(theRect);
                theRect.Deflate(1);

                AColor::TrackFocusPen(&dc, 1);
                dc.DrawRectangle(theRect);
                theRect.Deflate(1);

                AColor::TrackFocusPen(&dc, 0);
                dc.DrawRectangle(theRect);
            }
        }
    }

    wxRect DrawingArea(TrackPanelDrawingContext&,
                       const wxRect& rect, const wxRect&, unsigned iPass) override
    {
        if (iPass == TrackArtist::PassBorders) {
            return {
                rect.x - kBorderThickness,
                rect.y - kBorderThickness,
                rect.width + 2 * kBorderThickness + kShadowThickness,
                rect.height + 2 * kBorderThickness + kShadowThickness
            }
        } else if (iPass == TrackArtist::PassFocus) {
            constexpr auto extra = kBorderThickness + 3;
            return {
                rect.x - extra,
                rect.y - extra,
                rect.width + 2 * extra + kShadowThickness,
                rect.height + 2 * extra + kShadowThickness
            };
        } else {
            return rect;
        }
    }

    const std::shared_ptr<Track> mpTrack;
    wxCoord mLeftOffset;
};

// Stacks a label and a single or multi-channel track on a resizer below,
// which is associated with the last channel
struct ResizingChannelGroup final : TrackPanelGroup {
    ResizingChannelGroup(
        const std::shared_ptr<Track>& pTrack, wxCoord leftOffset)
        : mpTrack{pTrack}, mLeftOffset{leftOffset} {}
    Subdivision Children(const wxRect& rect) override
    {
        return { Axis::Y, Refinement{
                     { rect.GetTop(),
                       std::make_shared<LabeledChannelGroup>(mpTrack, mLeftOffset) },
                     { rect.GetTop() + rect.GetHeight() - kTrackSeparatorThickness,
                       TrackPanelResizerCell::Get(
                           **mpTrack->Channels().rbegin()).shared_from_this()
                     }
                 } };
    }

    const std::shared_ptr<Track> mpTrack;
    wxCoord mLeftOffset;
};

// Stacks a dead area at top, the tracks, and the click-to-deselect area below
struct Subgroup final : TrackPanelGroup {
    explicit Subgroup(TrackPanel& panel)
        : mPanel{panel} {}
    Subdivision Children(const wxRect& rect) override
    {
        const auto& viewInfo = *mPanel.GetViewInfo();
        wxCoord yy = -viewInfo.vpos;
        Refinement refinement;

        auto& tracks = *mPanel.GetTracks();
        if (!tracks.empty()) {
            refinement.emplace_back(yy, EmptyCell::Instance()),
            yy += kTopMargin;
        }

        for (const auto pTrack : tracks) {
            wxCoord height = 0;
            for (auto pChannel : pTrack->Channels()) {
                auto& view = ChannelView::Get(*pChannel);
                height += view.GetHeight();
            }
            refinement.emplace_back(yy,
                                    std::make_shared<ResizingChannelGroup>(
                                        pTrack->SharedPointer(), viewInfo.GetLeftOffset())
                                    );
            yy += height;
        }

        refinement.emplace_back(std::max(0, yy), mPanel.GetBackgroundCell());

        return { Axis::Y, std::move(refinement) };
    }

    TrackPanel& mPanel;
};

// Main group shaves off the left and right margins
struct MainGroup final : TrackPanelGroup {
    explicit MainGroup(TrackPanel& panel)
        : mPanel{panel} {}
    Subdivision Children(const wxRect& rect) override
    {
        return { Axis::X, Refinement{
                     { 0, EmptyCell::Instance() },
                     { kLeftMargin, std::make_shared<Subgroup>(mPanel) },
                     { rect.GetRight() + 1 - kRightMargin, EmptyCell::Instance() }
                 } };
    }

    TrackPanel& mPanel;
};
}

std::shared_ptr<TrackPanelNode> TrackPanel::Root()
{
    // Root and other subgroup objects are throwaways.
    // They might instead be cached to avoid repeated allocation.
    // That cache would need invalidation when there is addition, deletion, or
    // permutation of tracks, or change of width of the vertical rulers.
    return std::make_shared< MainGroup >(*this);
}

// This finds the rectangle of a given track (including all channels),
// either that of the label 'adornment' or the track itself
// The given track is assumed to be the first channel
wxRect TrackPanel::FindTrackRect(const Track* target)
{
    return CellularPanel::FindRect([&] ( TrackPanelNode& node ) {
        if (auto pGroup = dynamic_cast<const LabeledChannelGroup*>(&node)) {
            return pGroup->mpTrack.get() == target;
        }
        return false;
    });
}

wxRect TrackPanel::FindFocusedTrackRect(const Track* target)
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
        // rectangle allotted to the track, according to ChannelView::GetY() and
        // ChannelView::GetHeight(), but also over the margin of the next track.)

        rect.height += kBottomMargin;
        int dy = kTopMargin - 1;
        rect.Inflate(0, dy);

        // Note that this rectangle does not coincide with any one of
        // the nodes in the subdivision.
    }
    return rect;
}

std::vector<wxRect> TrackPanel::FindRulerRects(const Channel& target)
{
    std::vector<wxRect> results;
    VisitCells([&](const wxRect& rect, TrackPanelCell& visited) {
        if (auto pRuler = dynamic_cast<const ChannelVRulerControls*>(&visited)) {
            if (auto pView = pRuler->GetChannelView()) {
                if (pView->FindChannel().get() == &target) {
                    results.push_back(rect);
                }
            }
        }
    });
    return results;
}

std::shared_ptr<TrackPanelCell> TrackPanel::GetFocusedCell()
{
    auto pTrack = TrackFocus::Get(*GetProject()).Get();
    return pTrack
           ? ChannelView::Get(*pTrack->GetChannel(0)).shared_from_this()
           : GetBackgroundCell();
}

void TrackPanel::SetFocusedCell()
{
    // This may have a side-effect of assigning a focus if there was none
    auto& trackFocus = TrackFocus::Get(*GetProject());
    trackFocus.Set(trackFocus.Get());
    KeyboardCapture::Capture(this);
}

void TrackPanel::OnTrackFocusChange(TrackFocusChangeMessage message)
{
    if (message.focusPanel) {
        SetFocus();
    }
    if (auto cell = GetFocusedCell()) {
        Refresh(false);
    }
}
