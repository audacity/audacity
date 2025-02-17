/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackPanel.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_TRACK_PANEL__
#define __AUDACITY_TRACK_PANEL__

#include <chrono>
#include <vector>

#include <wx/setup.h> // for wxUSE_* macros
#include <wx/timer.h> // to inherit

#include "HitTestResult.h"
#include "Prefs.h"

#include "SelectedRegion.h"

#include "CellularPanel.h"
#include "Observer.h"

#include "commands/CommandManagerWindowClasses.h"

class wxRect;

struct AudioIOEvent;

// All cells of the TrackPanel are subclasses of this
class CommonTrackPanelCell;

class Channel;
class SpectrumAnalyst;
class Track;
class TrackList;
struct TrackListEvent;
class TrackPanel;
class TrackArtist;
class Ruler;
class AdornedRulerPanel;
class LWSlider;

class TrackPanelAx;

struct TrackPanelDrawingContext;

enum class UndoPush : unsigned char;

static constexpr auto kTimerInterval = std::chrono::milliseconds{ 50 };

const int DragThreshold = 3;// Anything over 3 pixels is a drag, else a click.

class AUDACITY_DLL_API TrackPanel final : public CellularPanel, public NonKeystrokeInterceptingWindow, private PrefsListener
{
public:
    static TrackPanel& Get(AudacityProject& project);
    static const TrackPanel& Get(const AudacityProject& project);
    static void Destroy(AudacityProject& project);

    TrackPanel(wxWindow* parent, wxWindowID id, const wxPoint& pos, const wxSize& size, const std::shared_ptr<TrackList>& tracks,
               ViewInfo* viewInfo, AudacityProject* project, AdornedRulerPanel* ruler);

    virtual ~TrackPanel();

    void UpdatePrefs() override;

    void OnAudioIO(AudioIOEvent);

    void OnPaint(wxPaintEvent& event);
    void OnMouseEvent(wxMouseEvent& event);
    void OnKeyDown(wxKeyEvent& event);

    void OnTrackListResizing(const TrackListEvent& event);
    void OnTrackListDeletion();
    void UpdateViewIfNoTracks(); // Call this to update mViewInfo, etc, after track(s) removal, before Refresh().

    double GetMostRecentXPos();

    void OnSize(wxSizeEvent&);
    void OnIdle(wxIdleEvent& event);
    void OnTimer(wxTimerEvent& event);
    void OnSyncLockChange(struct SyncLockChangeMessage);
    void OnTrackFocusChange(struct TrackFocusChangeMessage);

    void OnUndoReset(struct UndoRedoMessage);

    void Refresh(bool eraseBackground = true, const wxRect* rect = (const wxRect*)NULL)
    override;

    void RefreshTrack(Track* trk, bool refreshbacking = true);

    void HandlePageUpKey();
    void HandlePageDownKey();
    AudacityProject* GetProject() const override;

    void OnTrackMenu(Track* t = NULL);

    std::shared_ptr<TrackPanelCell> GetFocusedCell() override;
    void SetFocusedCell() override;

    void UpdateVRulers();
    void UpdateVRuler(Track* t);
    void UpdateTrackVRuler(Track& t);
    void UpdateVRulerSize();

protected:
    bool IsAudioActive();

public:
    size_t GetSelectedTrackCount() const;

protected:
    void UpdateSelectionDisplay();

public:
    void MakeParentRedrawScrollbars();

    /*!
     @return includes track control panel, and the vertical ruler, and
     the proper track area of all channels, and the separators between them.
     If target is nullptr, returns empty rectangle.
    */
    wxRect FindTrackRect(const Track* target);

    /*!
     @return includes what's in `FindTrackRect(target)` and the focus ring
     area around it.
     If target is nullptr, returns empty rectangle.
    */
    wxRect FindFocusedTrackRect(const Track* target);

    /*!
     @return extents of the vertical rulers of one channel, top to bottom.
     (There may be multiple sub-views, each with a ruler.)
     If target is nullptr, returns an empty vector.
     */
    std::vector<wxRect> FindRulerRects(const Channel& target);

protected:
    // Get the root object defining a recursive subdivision of the panel's
    // area into cells
    std::shared_ptr<TrackPanelNode> Root() override;

public:
// JKC Nov-2011: These four functions only used from within a dll
// They work around some messy problems with constructors.
    const TrackList* GetTracks() const { return mTracks.get(); }
    TrackList* GetTracks() { return mTracks.get(); }
    ViewInfo* GetViewInfo() { return mViewInfo; }
    AdornedRulerPanel* GetRuler() { return mRuler; }

protected:
    void DrawTracks(wxDC* dc);

public:
    // Set the object that performs catch-all event handling when the pointer
    // is not in any track or ruler or control panel.
    void SetBackgroundCell(const std::shared_ptr< CommonTrackPanelCell >& pCell);
    std::shared_ptr< CommonTrackPanelCell > GetBackgroundCell();

public:

protected:
    Observer::Subscription mTrackListSubscription,
                           mAudioIOSubscription,
                           mUndoSubscription,
                           mFocusChangeSubscription,
                           mRealtimeEffectManagerSubscription,
                           mSyncLockSubscription,
                           mProjectRulerInvalidatedSubscription,
                           mSelectionSubscription
    ;

    std::shared_ptr<TrackList> mTracks;

    AdornedRulerPanel* mRuler;

    std::unique_ptr<TrackArtist> mTrackArtist;

    class AUDACITY_DLL_API AudacityTimer final : public wxTimer
    {
    public:
        void Notify() override
        {
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

        TrackPanel* parent;
    } mTimer;

    int mTimeCount;

    bool mRefreshBacking;

protected:

    SelectedRegion mLastDrawnSelectedRegion {};

protected:

    std::shared_ptr<CommonTrackPanelCell> mpBackground;

    DECLARE_EVENT_TABLE()

    void ProcessUIHandleResult(TrackPanelCell* pClickedTrack, TrackPanelCell* pLatestCell, unsigned refreshResult) override;

    void UpdateStatusMessage(const TranslatableString& status) override;
};

#endif
