/**********************************************************************

Audacity: A Digital Audio Editor

ProjectWindow.h

Paul Licameli split from AudacityProject.h

**********************************************************************/

#ifndef __AUDACITY_PROJECT_WINDOW__
#define __AUDACITY_PROJECT_WINDOW__

#include <memory>
#include "ProjectWindowBase.h" // to inherit
#include "Prefs.h"
#include "Viewport.h"

class wxScrollBar;
class wxPanel;
class wxSplitterWindow;
class RealtimeEffectPanel;
enum class ProjectFileIOMessage : int;

class ProjectWindow;
void InitProjectWindow(ProjectWindow& window);

//! Message sent when the project window is closed.
struct ProjectWindowDestroyedMessage final : Observer::Message {};

///\brief A top-level window associated with a project, and handling scrollbars
/// and zooming
class AUDACITY_DLL_API ProjectWindow final : public ProjectWindowBase, public PrefsListener,
    public Observer::Publisher<ProjectWindowDestroyedMessage>
{
public:
    using Observer::Publisher<ProjectWindowDestroyedMessage>::Publish;
    static ProjectWindow& Get(AudacityProject& project);
    static const ProjectWindow& Get(const AudacityProject& project);
    static ProjectWindow* Find(AudacityProject* pProject);
    static const ProjectWindow* Find(const AudacityProject* pProject);

    explicit ProjectWindow(
        wxWindow* parent, wxWindowID id, const wxPoint& pos, const wxSize& size, AudacityProject& project);
    ~ProjectWindow() override;

    // Next available ID for sub-windows
    int NextWindowID();

    bool IsActive() override;
    bool IsIconized() const override;

    bool IsBeingDeleted() const { return mIsDeleting; }
    void SetIsBeingDeleted() { mIsDeleting = true; }

    /**
     * \brief Track list window is the parent container for TrackPanel
     * \return Pointer to a track list window (not null)
     */
    wxWindow* GetTrackListWindow() noexcept;
    /**
     * \brief Container is a parent window for both effects panel and
     * track list windows
     * \return Pointer to a container window (not null)
     */
    wxSplitterWindow* GetContainerWindow() noexcept;
    /**
     * \brief Top panel contains project-related controls and tools.
     * \return Pointer to a top panel window (not null)
     */
    wxPanel* GetTopPanel() noexcept;

    void UpdateStatusWidths();

    struct PlaybackScrollerMessage : Observer::Message {};

    class PlaybackScroller final : public Observer::Publisher<PlaybackScrollerMessage>
    {
    public:
        explicit PlaybackScroller(AudacityProject* project);

        enum class Mode {
            Off,
            Refresh,
            Pinned,
            Right,
        };

        Mode GetMode() const { return mMode; }
        void Activate(Mode mode)
        {
            mMode = mode;
        }

        double GetRecentStreamTime() const { return mRecentStreamTime; }

        void OnTimer();

    private:
        AudacityProject* mProject;
        Mode mMode { Mode::Off };

        // During timer update, grab the volatile stream time just once, so that
        // various other drawing code can use the exact same value.
        double mRecentStreamTime{ -1.0 };
    };
    PlaybackScroller& GetPlaybackScroller() { return *mPlaybackScroller; }

    void SetNormalizedWindowState(wxRect pSizeAndLocation) { mNormalizedWindowState = pSizeAndLocation; }
    wxRect GetNormalizedWindowState() const { return mNormalizedWindowState; }

    void ApplyUpdatedTheme();

    // Scrollbars

    wxScrollBar& GetVerticalScrollBar() { return *mVsbar; }
    wxScrollBar& GetHorizontalScrollBar() { return *mHsbar; }

    void OnScrollLeftButton(wxScrollEvent& event);
    void OnScrollRightButton(wxScrollEvent& event);

    std::pair<int, int> ViewportSize() const;
    unsigned MinimumTrackHeight();
    bool IsTrackMinimized(const Track& track);
    void SetMinimized(Track& track, bool minimized);
    int GetTrackHeight(const Track& track);
    void SetChannelHeights(Track& track, unsigned height);
    int GetTotalHeight(const TrackList& trackList);
    int GetHorizontalThumbPosition() const;
    int GetHorizontalThumbSize() const;
    int GetHorizontalRange() const;
    void SetHorizontalThumbPosition(int viewStart);
    void SetHorizontalScrollbar(int position, int thumbSize, int range, int pageSize, bool refresh);
    void ShowHorizontalScrollbar(bool shown);

    int GetVerticalThumbPosition() const;
    int GetVerticalThumbSize() const;
    int GetVerticalRange() const;
    void SetVerticalThumbPosition(int viewStart);
    void SetVerticalScrollbar(int position, int thumbSize, int range, int pageSize, bool refresh);
    void ShowVerticalScrollbar(bool shown);

    void SetToDefaultSize();

    // PRL:  old and incorrect comment below, these functions are used elsewhere than TrackPanel
    // TrackPanel access
    wxSize GetTPTracksUsableArea() /* not override */;
    void RefreshTPTrack(Track* pTrk, bool refreshbacking = true) /* not override */;

    wxStatusBar* CreateProjectStatusBar();
private:
    void OnThemeChange(struct ThemeChangeMessage);

    // PrefsListener implementation
    void UpdatePrefs() override;

public:
    // Message Handlers

    void OnMenu(wxCommandEvent& event);
    void OnUpdateUI(wxUpdateUIEvent& event);

    void MacShowUndockedToolbars(bool show);
    void OnActivate(wxActivateEvent& event);

    void OnMouseEvent(wxMouseEvent& event);
    void OnIconize(wxIconizeEvent& event);
    void OnSize(wxSizeEvent& event);
    void UpdateLayout();
    void OnShow(wxShowEvent& event);
    void OnMove(wxMoveEvent& event);
    void OnScroll(wxScrollEvent& event);
    void OnToolBarUpdate(wxCommandEvent& event);
    void OnProjectTitleChange(ProjectFileIOMessage);

private:
    wxRect mNormalizedWindowState;

    wxPanel* mTopPanel{};
    wxSplitterWindow* mContainerWindow;
    wxWindow* mTrackListWindow{};

    wxScrollBar* mHsbar{};
    wxScrollBar* mVsbar{};

    int mNextWindowID{};

    bool mActive{ true };
    bool mIconized{ false };
    bool mShownOnce{ false };

    bool mIsDeleting{ false };

private:
    void OnViewportMessage(const ViewportMessage& message);

    Observer::Subscription
        mThemeChangeSubscription,
        mTitleChangeSubscription,
        mSnappingChangedSubscription
    ;
    std::unique_ptr<PlaybackScroller> mPlaybackScroller;
    const Observer::Subscription mViewportSubscription;

    DECLARE_EVENT_TABLE()
};

void GetDefaultWindowRect(wxRect* defRect);
void GetNextWindowPlacement(wxRect* nextRect, bool* pMaximized, bool* pIconized);

extern AUDACITY_DLL_API BoolSetting ProjectWindowMaximized;
extern AUDACITY_DLL_API BoolSetting ProjectWindowIconized;
extern AUDACITY_DLL_API IntSetting ProjectWindowX;
extern AUDACITY_DLL_API IntSetting ProjectWindowY;
extern AUDACITY_DLL_API IntSetting ProjectWindowWidth;
extern AUDACITY_DLL_API IntSetting ProjectWindowHeight;
extern AUDACITY_DLL_API IntSetting ProjectWindowNormalX;
extern AUDACITY_DLL_API IntSetting ProjectWindowNormalY;
extern AUDACITY_DLL_API IntSetting ProjectWindowNormalWidth;
extern AUDACITY_DLL_API IntSetting ProjectWindowNormalHeight;

#endif
