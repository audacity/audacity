/**********************************************************************

  Audacity: A Digital Audio Editor

  AdornedRulerPanel.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_ADORNED_RULER_PANEL__
#define __AUDACITY_ADORNED_RULER_PANEL__

#include "CellularPanel.h"
#include "widgets/Ruler.h" // member variable
#include "widgets/LinearUpdater.h"
#include "widgets/TimeFormat.h"
#include "Observer.h"
#include "Prefs.h"
#include "ViewInfo.h" // for PlayRegion
#include "TimeDisplayMode.h"

class AudacityProject;
struct AudioIOEvent;
class LinearUpdater;
class TrackList;

// This is an Audacity Specific ruler panel.
class AUDACITY_DLL_API AdornedRulerPanel final : public CellularPanel, private PrefsListener
{
public:
    static AdornedRulerPanel& Get(AudacityProject& project);
    static const AdornedRulerPanel& Get(const AudacityProject& project);
    static void Destroy(AudacityProject& project);

    AdornedRulerPanel(AudacityProject* project, wxWindow* parent, wxWindowID id, const wxPoint& pos = wxDefaultPosition,
                      const wxSize& size = wxDefaultSize, ViewInfo* viewinfo = NULL);

    ~AdornedRulerPanel();

    void Refresh(bool eraseBackground = true, const wxRect* rect = (const wxRect*)NULL)
    override;

    bool AcceptsFocus() const override { return s_AcceptsFocus; }
    bool AcceptsFocusFromKeyboard() const override { return true; }
    void SetFocusFromKbd() override;

public:
    int GetRulerHeight() { return GetRulerHeight(ShowingScrubRuler()); }
    static int GetRulerHeight(bool showScrubBar);
    wxRect GetInnerRect() const { return mInner; }

    void SetLeftOffset(int offset);

    void DrawSelection();

    void SetPlayRegion(double playRegionStart, double playRegionEnd);
    void ClearPlayRegion();
    void TogglePinnedHead();

    void GetMaxSize(wxCoord* width, wxCoord* height);

    void InvalidateRuler();

    void UpdatePrefs() override;
    void ReCreateButtons();

    void UpdateQuickPlayPos(wxCoord& mousePosX);

    bool ShowingScrubRuler() const;
    //void OnToggleScrubRulerFromMenu(wxCommandEvent& );
    bool SetPanelSize();

    void DrawBothOverlays();

private:
    void DoIdle();
    void OnIdle(wxIdleEvent& evt);
    void OnAudioStartStop(AudioIOEvent);
    void OnPaint(wxPaintEvent& evt);
    void OnSize(wxSizeEvent& evt);
    void OnLeave(wxMouseEvent& evt);
    void OnThemeChange(struct ThemeChangeMessage);
    void OnSelectionChange(Observer::Message);
    void DoSelectionChange(const SelectedRegion& selectedRegion);
    bool UpdateRects();
    void HandleQPClick(wxMouseEvent& event, wxCoord mousePosX);
    void HandleQPDrag(wxMouseEvent& event, wxCoord mousePosX);
    void HandleQPRelease(wxMouseEvent& event);
    void StartQPPlay(
        bool newDefault, bool cutPreview, const double* pStartTime = nullptr);

    void DoDrawBackground(wxDC* dc);
    void DoDrawEdge(wxDC* dc);
    void DoDrawMarks(wxDC* dc, bool /*text */);
    wxRect RegionRectangle(double t0, double t1) const;
    wxRect PlayRegionRectangle() const;
    wxRect SelectedRegionRectangle() const;
    void DoDrawPlayRegion(wxDC* dc, const wxRect& rectP, const wxRect& rectL, const wxRect& rectR);
    void DoDrawPlayRegionLimits(wxDC* dc, const wxRect& rect);
    void DoDrawOverlap(wxDC* dc, const wxRect& rect);
    void DoDrawSelection(wxDC* dc, const wxRect& rectS, const wxRect& rectL, const wxRect& rectR);

public:
    void DoDrawScrubIndicator(wxDC* dc, wxCoord xx, int width, bool scrub, bool seek);
    void UpdateButtonStates();

private:
    static bool s_AcceptsFocus;
    struct Resetter {
        void operator ()(bool* p) const
        {
            if (p) {
                *p = false;
            }
        }
    };
    using TempAllowFocus = std::unique_ptr<bool, Resetter>;

public:
    static TempAllowFocus TemporarilyAllowFocus();

    void SetNumGuides(size_t nn);

private:
    enum class MenuChoice {
        QuickPlay, Scrub
    };
    void ShowContextMenu(MenuChoice choice, const wxPoint* pPosition);

    double Pos2Time(int p, bool ignoreFisheye = false) const;
    int Time2Pos(double t, bool ignoreFisheye = false) const;

    bool IsWithinMarker(int mousePosX, double markerTime);

private:
    AudacityProject* const mProject;

    LinearUpdater& mUpdater;
    Ruler& mRuler;

    TrackList* mTracks;

    wxRect mOuter;
    wxRect mScrubZone;
    wxRect mInner;

    int mLeftOffset; // Number of pixels before we hit the 'zero position'.

    double mIndTime;

    static constexpr size_t MAX_GUIDES = 2;
    double mQuickPlayOffset[MAX_GUIDES]{};
    double mQuickPlayPosUnsnapped[MAX_GUIDES]{};
    double mQuickPlayPos[MAX_GUIDES]{};
    bool mIsSnapped[MAX_GUIDES]{};
    size_t mNumGuides{ 1 };

    PlayRegion mOldPlayRegion;

    bool mIsRecording;

    //
    // Pop-up menu
    //
    void ShowMenu(const wxPoint& pos);
    void ShowScrubMenu(const wxPoint& pos);
    static void DragSelection(AudacityProject& project);
    void HandleSnapping(size_t index);
    void OnTimelineFormatChange(wxCommandEvent& evt);
    void OnSyncSelToQuickPlay(wxCommandEvent& evt);
    void OnAutoScroll(wxCommandEvent& evt);
    void OnTogglePlayRegion(wxCommandEvent& evt);
    void OnClearPlayRegion(wxCommandEvent& evt);
    void OnSetPlayRegionToSelection(wxCommandEvent& evt);

    void OnPinnedButton(wxCommandEvent& event);
    void OnTogglePinnedState(wxCommandEvent& event);

    bool mPlayRegionDragsSelection;

    enum MouseEventState {
        mesNone,
        mesDraggingPlayRegionStart,
        mesDraggingPlayRegionEnd,
        mesSelectingPlayRegionClick,
        mesSelectingPlayRegionRange
    };

    MouseEventState mMouseEventState;
    double mLeftDownClickUnsnapped; // click position in seconds, before snap
    double mLeftDownClick; // click position in seconds
    bool mIsDragging;

    DECLARE_EVENT_TABLE()

    wxWindow* mButtons[3];
    bool mNeedButtonUpdate { true };

    //
    // CellularPanel implementation
    //

    // Get the root object defining a recursive subdivision of the panel's
    // area into cells
    std::shared_ptr<TrackPanelNode> Root() override;
public:
    AudacityProject* GetProject() const override;
private:
    std::shared_ptr<TrackPanelCell> GetFocusedCell() override;
    void SetFocusedCell() override;
    void ProcessUIHandleResult(TrackPanelCell* pClickedTrack, TrackPanelCell* pLatestCell, unsigned refreshResult) override;

    void UpdateStatusMessage(const TranslatableString&) override;

    void CreateOverlays();

    // Cooperating objects
    class TrackPanelGuidelineOverlay;
    std::shared_ptr<TrackPanelGuidelineOverlay> mOverlay;

    class ScrubbingRulerOverlay;

private:
    class CommonRulerHandle;
    class QPHandle;
    class PlayRegionAdjustingHandle;
    class MovePlayRegionHandle;
    class ResizePlayRegionHandle;
    class NewPlayRegionHandle;
    class ScrubbingHandle;

    class CommonCell;

    class QPCell;
    std::shared_ptr<QPCell> mQPCell;

    class ScrubbingCell;
    std::shared_ptr<ScrubbingCell> mScrubbingCell;

    Observer::Subscription mAudioIOSubscription,
                           mPlayRegionSubscription,
                           mThemeChangeSubscription,
                           mRulerInvalidatedSubscription;

    // classes implementing subdivision for CellularPanel
    struct Subgroup;
    struct MainGroup;

    SelectedRegion mLastDrawnSelectedRegion;
    std::pair<double, double> mLastDrawnPlayRegion{};
    bool mLastPlayRegionActive = false;
    double mLastDrawnH{};
    double mLastDrawnZoom{};

public:
    TimeDisplayMode GetTimeDisplayMode() const;
    void SetTimeDisplayMode(TimeDisplayMode rulerType);

private:

    TimeDisplayMode mTimeDisplayMode;
};

#endif //define __AUDACITY_ADORNED_RULER_PANEL__
