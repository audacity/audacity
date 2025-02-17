/**********************************************************************

Audacity: A Digital Audio Editor

ProjectWindow.cpp

Paul Licameli split from AudacityProject.cpp

**********************************************************************/
#include "ProjectWindow.h"

#include "ActiveProject.h"
#include "AllThemeResources.h"
#include "AudioIO.h"
#include "tracks/ui/CommonTrackInfo.h"
#include "ProjectAudioIO.h"
#include "ProjectFileIO.h"
#include "ProjectWindows.h"
#include "ProjectStatus.h"
#include "ViewInfo.h"
#include "WaveClip.h"
#include "WaveTrack.h"
#include "CommandManager.h"
#include "prefs/TracksPrefs.h"
#include "toolbars/ToolManager.h"
#include "tracks/ui/ChannelView.h"
#include "wxPanelWrapper.h"
#include "WindowAccessible.h"

#include "ThemedWrappers.h"

#include <wx/display.h>
#include <wx/scrolbar.h>
#include <wx/sizer.h>
#include <wx/splitter.h>
#include <wx/wupdlock.h>

#include "TrackPanel.h"

namespace {
#ifdef HAS_AUDIOCOM_UPLOAD
constexpr int DEFAULT_WINDOW_WIDTH = 1180;
#else
constexpr int DEFAULT_WINDOW_WIDTH = 1120;
#endif
constexpr int DEFAULT_WINDOW_HEIGHT = 674;
}

BoolSetting ProjectWindowMaximized{ L"/Window/Maximized", false };
BoolSetting ProjectWindowIconized{ L"/Window/Iconized", false };
IntSetting ProjectWindowX{ L"/Window/X", 0 };
IntSetting ProjectWindowY{ L"/Window/Y", 0 };
IntSetting ProjectWindowWidth{ L"/Window/Width", DEFAULT_WINDOW_WIDTH };
IntSetting ProjectWindowHeight{ L"/Window/Height", DEFAULT_WINDOW_HEIGHT };
IntSetting ProjectWindowNormalX{ L"/Window/Normal_X", 0 };
IntSetting ProjectWindowNormalY{ L"/Window/Normal_Y", 0 };
IntSetting ProjectWindowNormalWidth{ L"/Window/Normal_Width", DEFAULT_WINDOW_WIDTH };
IntSetting ProjectWindowNormalHeight{ L"/Window/Normal_Height", DEFAULT_WINDOW_HEIGHT };

// Returns the screen containing a rectangle, or -1 if none does.
int ScreenContaining(wxRect& r)
{
    unsigned int n = wxDisplay::GetCount();
    for (unsigned int i = 0; i < n; i++) {
        wxDisplay d(i);
        wxRect scr = d.GetClientArea();
        if (scr.Contains(r)) {
            return (int)i;
        }
    }
    return -1;
}

// true IFF TL and BR corners are on a connected display.
// Does not need to check all four.  We just need to check that
// the window probably is straddling screens in a sensible way.
// If the user wants to use mixed landscape and portrait, they can.
bool CornersOnScreen(wxRect& r)
{
    if (wxDisplay::GetFromPoint(r.GetTopLeft()) == wxNOT_FOUND) {
        return false;
    }
    if (wxDisplay::GetFromPoint(r.GetBottomRight()) == wxNOT_FOUND) {
        return false;
    }
    return true;
}

// true iff we have enough of the top bar to be able to reposition the window.
bool IsWindowAccessible(wxRect* requestedRect)
{
    wxDisplay display;
    wxRect targetTitleRect(requestedRect->GetLeftTop(), requestedRect->GetBottomRight());
    // Hackery to approximate a window top bar size from a window size.
    // and exclude the open/close and borders.
    targetTitleRect.x += 15;
    targetTitleRect.width -= 100;
    if (targetTitleRect.width < 165) {
        targetTitleRect.width = 165;
    }
    targetTitleRect.height = 15;
    int targetBottom = targetTitleRect.GetBottom();
    int targetRight = targetTitleRect.GetRight();
    // This looks like overkill to check each and every pixel in the ranges.
    // and decide that if any is visible on screen we are OK.
    for (int i =  targetTitleRect.GetLeft(); i < targetRight; i++) {
        for (int j = targetTitleRect.GetTop(); j < targetBottom; j++) {
            int monitor = display.GetFromPoint(wxPoint(i, j));
            if (monitor != wxNOT_FOUND) {
                return TRUE;
            }
        }
    }
    return FALSE;
}

// BG: The default size and position of the first window
void GetDefaultWindowRect(wxRect* defRect)
{
    *defRect = wxGetClientDisplayRect();

    int width = DEFAULT_WINDOW_WIDTH;
    int height = DEFAULT_WINDOW_HEIGHT;

    //These conditional values assist in improving placement and size
    //of NEW windows on different platforms.
#ifdef __WXGTK__
    height += 20;
#endif

#ifdef __WXMSW__
    height += 40;
#endif

#ifdef __WXMAC__
    height += 55;
#endif

    // Use screen size where it is smaller than the values we would like.
    // Otherwise use the values we would like, and centred.
    if (width < defRect->width) {
        defRect->x = (defRect->width - width) / 2;
        defRect->width = width;
    }

    if (height < defRect->height) {
        defRect->y = (defRect->height - height) / 2;
        // Bug 1119 workaround
        // Small adjustment for very small Mac screens.
        // If there is only a tiny space at the top
        // then instead of vertical centre, align to bottom.
        const int pixelsFormenu = 60;
        if (defRect->y < pixelsFormenu) {
            defRect->y *=2;
        }
        defRect->height = height;
    }
}

// BG: Calculate where to place the next window (could be the first window)
// BG: Does not store X and Y in prefs. This is intentional.
//
// LL: This should NOT need to be this complicated...FIXME
void GetNextWindowPlacement(wxRect* nextRect, bool* pMaximized, bool* pIconized)
{
    int inc = 25;

    wxRect defaultRect;
    GetDefaultWindowRect(&defaultRect);

    *pMaximized = ProjectWindowMaximized.Read();
    *pIconized = ProjectWindowIconized.Read();

    wxRect windowRect;
    windowRect.x = ProjectWindowX.ReadWithDefault(defaultRect.x);
    windowRect.y = ProjectWindowY.ReadWithDefault(defaultRect.y);
    windowRect.width = ProjectWindowWidth.ReadWithDefault(defaultRect.width);
    windowRect.height = ProjectWindowHeight.ReadWithDefault(defaultRect.height);

    wxRect normalRect;
    normalRect.x = ProjectWindowNormalX.ReadWithDefault(defaultRect.x);
    normalRect.y = ProjectWindowNormalY.ReadWithDefault(defaultRect.y);
    normalRect.width = ProjectWindowNormalWidth.ReadWithDefault(defaultRect.width);
    normalRect.height = ProjectWindowNormalHeight.ReadWithDefault(defaultRect.height);

    // Workaround 2.1.1 and earlier bug on OSX...affects only normalRect, but let's just
    // validate for all rects and plats
    if (normalRect.width == 0 || normalRect.height == 0) {
        normalRect = defaultRect;
    }
    if (windowRect.width == 0 || windowRect.height == 0) {
        windowRect = defaultRect;
    }

    wxRect screenRect(wxGetClientDisplayRect());
#if defined(__WXMAC__)

    // On OSX, the top of the window should never be less than the menu height,
    // so something is amiss if it is
    if (normalRect.y < screenRect.y) {
        normalRect = defaultRect;
    }
    if (windowRect.y < screenRect.y) {
        windowRect = defaultRect;
    }
#endif

    // IF projects empty, THEN it's the first window.
    // It lands where the config says it should, and can straddle screen.
    if (AllProjects{}.empty()) {
        if (*pMaximized || *pIconized) {
            *nextRect = normalRect;
        } else {
            *nextRect = windowRect;
        }
        // Resize, for example if one monitor that was on is now off.
        if (!CornersOnScreen(wxRect(*nextRect).Deflate(32, 32))) {
            *nextRect = defaultRect;
        }
        if (!IsWindowAccessible(nextRect)) {
            *nextRect = defaultRect;
        }
        // Do not trim the first project window down.
        // All corners are on screen (or almost so), and
        // the rect may straddle screens.
        return;
    }

    // ELSE a subsequent NEW window.  It will NOT straddle screens.

    // We don't mind being 32 pixels off the screen in any direction.
    // Make sure initial sizes (pretty much) fit within the display bounds
    // We used to trim the sizes which could result in ridiculously small windows.
    // contributing to bug 1243.
    // Now instead if the window significantly doesn't fit the screen, we use the default
    // window instead, which we know does.
    if (ScreenContaining(wxRect(normalRect).Deflate(32, 32)) < 0) {
        normalRect = defaultRect;
    }
    if (ScreenContaining(wxRect(windowRect).Deflate(32, 32)) < 0) {
        windowRect = defaultRect;
    }

    bool validWindowSize = false;
    ProjectWindow* validProject = NULL;
    for ( auto iter = AllProjects{}.rbegin(), end = AllProjects{}.rend();
          iter != end; ++iter
          ) {
        auto pProject = *iter;
        if (!GetProjectFrame(*pProject).IsIconized()) {
            validWindowSize = true;
            validProject = &ProjectWindow::Get(*pProject);
            break;
        }
    }
    if (validWindowSize) {
        *nextRect = validProject->GetRect();
        *pMaximized = validProject->IsMaximized();
        *pIconized = validProject->IsIconized();
        // Do not straddle screens.
        if (ScreenContaining(wxRect(*nextRect).Deflate(32, 32)) < 0) {
            *nextRect = defaultRect;
        }
    } else {
        *nextRect = normalRect;
    }

    //Placement depends on the increments
    nextRect->x += inc;
    nextRect->y += inc;

    // defaultrect is a rectangle on the first screen.  It's the right fallback to
    // use most of the time if things are not working out right with sizing.
    // windowRect is a saved rectangle size.
    // normalRect seems to be a substitute for windowRect when iconized or maximised.

    // Windows can say that we are off screen when actually we are not.
    // On Windows 10 I am seeing miscalculation by about 6 pixels.
    // To fix this we allow some sloppiness on the edge being counted as off screen.
    // This matters most when restoring very carefully sized windows that are maximised
    // in one dimension (height or width) but not both.
    const int edgeSlop = 10;

    // Next four lines are getting the rectangle for the screen that contains the
    // top left corner of nextRect (and defaulting to rect of screen 0 otherwise).
    wxPoint p = nextRect->GetLeftTop();
    int scr = std::max(0, wxDisplay::GetFromPoint(p));
    wxDisplay d(scr);
    screenRect = d.GetClientArea();

    // Now we (possibly) start trimming our rectangle down.
    // Have we hit the right side of the screen?
    wxPoint bottomRight = nextRect->GetBottomRight();
    if (bottomRight.x > (screenRect.GetRight() + edgeSlop)) {
        int newWidth = screenRect.GetWidth() - nextRect->GetLeft();
        if (newWidth < defaultRect.GetWidth()) {
            nextRect->x = windowRect.x;
            nextRect->y = windowRect.y;
            nextRect->width = windowRect.width;
        } else {
            nextRect->width = newWidth;
        }
    }

    // Have we hit the bottom of the screen?
    bottomRight = nextRect->GetBottomRight();
    if (bottomRight.y > (screenRect.GetBottom() + edgeSlop)) {
        nextRect->y -= inc;
        bottomRight = nextRect->GetBottomRight();
        if (bottomRight.y > (screenRect.GetBottom() + edgeSlop)) {
            nextRect->SetBottom(screenRect.GetBottom());
        }
    }

    // After all that we could have a window that does not have a visible
    // top bar.  [It is unlikely, but something might have gone wrong]
    // If so, use the safe fallback size.
    if (!IsWindowAccessible(nextRect)) {
        *nextRect = defaultRect;
    }
}

namespace {
// This wrapper prevents the scrollbars from retaining focus after being
// used.  Otherwise, the only way back to the track panel is to click it
// and that causes your original location to be lost.
class ScrollBar final : public wxScrollBar
{
public:
    ScrollBar(wxWindow* parent, wxWindowID id, long style)
        :   wxScrollBar(parent, id, wxDefaultPosition, wxDefaultSize, style)
    {
    }

    void OnSetFocus(wxFocusEvent& e)
    {
        wxWindow* w = e.GetWindow();
        if (w != NULL) {
            w->SetFocus();
        }
    }

    void SetScrollbar(int position, int thumbSize, int range, int pageSize, bool refresh = true) override;

private:
    DECLARE_EVENT_TABLE()
};

void ScrollBar::SetScrollbar(int position, int thumbSize,
                             int range, int pageSize,
                             bool refresh)
{
    // Mitigate flashing of scrollbars by refreshing only when something really changes.

    // PRL:  This may have been made unnecessary by other fixes for flashing, see
    // commit ac05b190bee7dd0000bce56edb0e5e26185c972f

    auto changed
        =position != GetThumbPosition()
          || thumbSize != GetThumbSize()
          || range != GetRange()
          || pageSize != GetPageSize();
    if (!changed) {
        return;
    }

    wxScrollBar::SetScrollbar(position, thumbSize, range, pageSize, refresh);
}

BEGIN_EVENT_TABLE(ScrollBar, wxScrollBar)
EVT_SET_FOCUS(ScrollBar::OnSetFocus)
END_EVENT_TABLE()

AttachedWindows::RegisteredFactory sProjectWindowKey{
    []( AudacityProject& parent ) -> wxWeakRef< wxWindow > {
        wxRect wndRect;
        bool bMaximized = false;
        bool bIconized = false;
        GetNextWindowPlacement(&wndRect, &bMaximized, &bIconized);

        auto pWindow = safenew ProjectWindow(
            nullptr, -1,
            wxDefaultPosition,
            wxSize(wndRect.width, wndRect.height),
            parent
            );

        auto& window = *pWindow;
        // wxGTK3 seems to need to require creating the window using default position
        // and then manually positioning it.
        window.SetPosition(wndRect.GetPosition());

        if (bMaximized) {
            window.Maximize(true);
        } else if (bIconized) {
            // if the user close down and iconized state we could start back up and iconized state
            // window.Iconize(TRUE);
        }

        return pWindow;
    }
};
}

ProjectWindow& ProjectWindow::Get(AudacityProject& project)
{
    return GetAttachedWindows(project).Get< ProjectWindow >(sProjectWindowKey);
}

const ProjectWindow& ProjectWindow::Get(const AudacityProject& project)
{
    return Get(const_cast< AudacityProject& >(project));
}

ProjectWindow* ProjectWindow::Find(AudacityProject* pProject)
{
    return pProject
           ? GetAttachedWindows(*pProject).Find< ProjectWindow >(sProjectWindowKey)
           : nullptr;
}

const ProjectWindow* ProjectWindow::Find(const AudacityProject* pProject)
{
    return Find(const_cast< AudacityProject* >(pProject));
}

int ProjectWindow::NextWindowID()
{
    return mNextWindowID++;
}

enum {
    FirstID = 1000,

    // Window controls

    HSBarID,
    VSBarID,

    NextID,
};

namespace {
bool IsWindowValid(const ProjectWindow* window) { return window && !window->IsBeingDeleted(); }

//! This allows either the ProjectWindow or the AudacityProject to be
//! destroyed first.  This object is given to an attached object of the project.
struct Adapter final : ViewportCallbacks {
    explicit Adapter(ProjectWindow& window)
        : mwWindow{&window} {}
    ~Adapter() override = default;

    std::pair<int, int> ViewportSize() const override
    { return IsWindowValid(mwWindow) ? mwWindow->ViewportSize() : std::pair{ 1, 1 }; }

    unsigned MinimumTrackHeight() override
    { return IsWindowValid(mwWindow) ? mwWindow->MinimumTrackHeight() : 0; }
    bool IsTrackMinimized(const Track& track) override
    { return IsWindowValid(mwWindow) ? mwWindow->IsTrackMinimized(track) : false; }
    void SetMinimized(Track& track, bool minimized) override
    {
        if (IsWindowValid(mwWindow)) {
            mwWindow->SetMinimized(track, minimized);
        }
    }

    int GetTrackHeight(const Track& track) override
    { return IsWindowValid(mwWindow) ? mwWindow->GetTrackHeight(track) : 0; }
    void SetChannelHeights(Track& track, unsigned height) override
    {
        if (IsWindowValid(mwWindow)) {
            mwWindow->SetChannelHeights(track, height);
        }
    }

    int GetTotalHeight(const TrackList& trackList) override
    { return IsWindowValid(mwWindow) ? mwWindow->GetTotalHeight(trackList) : 0; }

    int GetHorizontalThumbPosition() const override
    { return IsWindowValid(mwWindow) ? mwWindow->GetHorizontalThumbPosition() : 0; }
    int GetHorizontalThumbSize() const override
    { return IsWindowValid(mwWindow) ? mwWindow->GetHorizontalThumbSize() : 0; }
    int GetHorizontalRange() const override
    { return IsWindowValid(mwWindow) ? mwWindow->GetHorizontalRange() : 0; }
    void SetHorizontalThumbPosition(int viewStart) override
    {
        if (IsWindowValid(mwWindow)) {
            mwWindow->SetHorizontalThumbPosition(viewStart);
        }
    }

    void SetHorizontalScrollbar(int position, int thumbSize,
                                int range, int pageSize, bool refresh) override
    {
        if (IsWindowValid(mwWindow)) {
            mwWindow->SetHorizontalScrollbar(
                position, thumbSize, range, pageSize, refresh);
        }
    }

    void ShowHorizontalScrollbar(bool shown) override
    {
        if (IsWindowValid(mwWindow)) {
            mwWindow->ShowHorizontalScrollbar(shown);
        }
    }

    int GetVerticalThumbPosition() const override
    { return IsWindowValid(mwWindow) ? mwWindow->GetVerticalThumbPosition() : 0; }
    int GetVerticalThumbSize() const override
    { return IsWindowValid(mwWindow) ? mwWindow->GetVerticalThumbSize() : 0; }
    int GetVerticalRange() const override
    { return IsWindowValid(mwWindow) ? mwWindow->GetVerticalRange() : 0; }
    void SetVerticalThumbPosition(int viewStart) override
    {
        if (IsWindowValid(mwWindow)) {
            mwWindow->SetVerticalThumbPosition(viewStart);
        }
    }

    void SetVerticalScrollbar(int position, int thumbSize,
                              int range, int pageSize, bool refresh) override
    {
        if (IsWindowValid(mwWindow)) {
            mwWindow->SetVerticalScrollbar(
                position, thumbSize, range, pageSize, refresh);
        }
    }

    void ShowVerticalScrollbar(bool shown) override
    {
        if (IsWindowValid(mwWindow)) {
            mwWindow->ShowVerticalScrollbar(shown);
        }
    }

    void SetToDefaultSize() override
    {
        if (IsWindowValid(mwWindow)) {
            mwWindow->SetToDefaultSize();
        }
    }

    wxWeakRef<ProjectWindow> mwWindow;
};
}

ProjectWindow::ProjectWindow(wxWindow* parent, wxWindowID id,
                             const wxPoint& pos,
                             const wxSize& size, AudacityProject& project)
    : ProjectWindowBase{parent, id, pos, size, project}
    , mViewportSubscription{
                            Viewport::Get(project)
                            .Subscribe(*this, &ProjectWindow::OnViewportMessage)}
{
    Viewport::Get(project).SetCallbacks(std::make_unique<Adapter>(*this));

    mNextWindowID = NextID;

    constexpr auto EffectsPanelMaxWidth { 500 };
    constexpr auto TrackPanelMinWidth { 250 };

    // Two sub-windows need to be made before Init(),
    // so that this constructor can complete, and then TrackPanel and
    // AdornedRulerPanel can retrieve those windows from this in their
    // factory functions

    // PRL:  this panel groups the top tool dock and the ruler into one
    // tab cycle.
    // Must create it with non-default width equal to the main window width,
    // or else the device toolbar doesn't make initial widths of the choice
    // controls correct.
    mTopPanel = safenew wxPanelWrapper {
        this, wxID_ANY, wxDefaultPosition,
        wxSize{ this->GetSize().GetWidth(), -1 }
    };
    mTopPanel->SetLabel("Top Panel"); // Not localised
    mTopPanel->SetLayoutDirection(wxLayout_LeftToRight);
    mTopPanel->SetAutoLayout(true);

    auto container = safenew wxSplitterWindow(this, wxID_ANY,
                                              wxDefaultPosition,
                                              wxDefaultSize,
                                              wxNO_BORDER | wxSP_LIVE_UPDATE | wxSP_THIN_SASH);
    container->Bind(wxEVT_SPLITTER_DOUBLECLICKED, [](wxSplitterEvent& event){
        //"The default behaviour is to unsplit the window"
        event.Veto();//do noting instead
    });
    container->Bind(wxEVT_SPLITTER_SASH_POS_CHANGING, [=](wxSplitterEvent& event){
        if (event.GetSashPosition() > EffectsPanelMaxWidth) {
            //Prevents left panel from expanding further
            event.SetSashPosition(-1);
        }
    });
    mContainerWindow = container;

    mTrackListWindow = safenew wxPanelWrapper(mContainerWindow, wxID_ANY,
                                              wxDefaultPosition,
                                              wxDefaultSize,
                                              wxNO_BORDER);
    mTrackListWindow->SetMinSize({ TrackPanelMinWidth, -1 });
    mTrackListWindow->SetSizer(safenew wxBoxSizer(wxVERTICAL));
    mTrackListWindow->SetLabel("Main Panel");// Not localized.
    mTrackListWindow->SetLayoutDirection(wxLayout_LeftToRight);

    mContainerWindow->Initialize(mTrackListWindow);

    mPlaybackScroller = std::make_unique<PlaybackScroller>(&project);

    // PRL: Old comments below.  No longer observing the ordering that it
    //      recommends.  ProjectWindow::OnActivate puts the focus directly into
    //      the TrackPanel, which avoids the problems.
    // LLL: When Audacity starts or becomes active after returning from
    //      another application, the first window that can accept focus
    //      will be given the focus even if we try to SetFocus().  By
    //      creating the scrollbars after the TrackPanel, we resolve
    //      several focus problems.

    mHsbar = safenew ScrollBar(mTrackListWindow, HSBarID, wxSB_HORIZONTAL);
    mVsbar = safenew ScrollBar(mTrackListWindow, VSBarID, wxSB_VERTICAL);
#if wxUSE_ACCESSIBILITY
    // so that name can be set on a standard control
    mHsbar->SetAccessible(safenew WindowAccessible(mHsbar));
    mVsbar->SetAccessible(safenew WindowAccessible(mVsbar));
#endif
    mHsbar->SetLayoutDirection(wxLayout_LeftToRight);
    mHsbar->SetName(_("Horizontal Scrollbar"));
    mVsbar->SetName(_("Vertical Scrollbar"));

    mThemeChangeSubscription
        =theTheme.Subscribe(*this, &ProjectWindow::OnThemeChange);

    // Subscribe to title changes published by ProjectFileIO
    mTitleChangeSubscription = ProjectFileIO::Get(project)
                               .Subscribe(*this, &ProjectWindow::OnProjectTitleChange);

    // And also establish my initial consistency with it
    this->OnProjectTitleChange(ProjectFileIOMessage::ProjectTitleChange);
}

ProjectWindow::~ProjectWindow()
{
    // Tool manager gives us capture sometimes
    if (HasCapture()) {
        ReleaseMouse();
    }
}

BEGIN_EVENT_TABLE(ProjectWindow, wxFrame)
EVT_MENU(wxID_ANY, ProjectWindow::OnMenu)
EVT_MOUSE_EVENTS(ProjectWindow::OnMouseEvent)
EVT_CLOSE(ProjectWindow::OnCloseWindow)
EVT_SIZE(ProjectWindow::OnSize)
EVT_SHOW(ProjectWindow::OnShow)
EVT_ICONIZE(ProjectWindow::OnIconize)
EVT_MOVE(ProjectWindow::OnMove)
EVT_ACTIVATE(ProjectWindow::OnActivate)
EVT_COMMAND_SCROLL_LINEUP(HSBarID, ProjectWindow::OnScrollLeftButton)
EVT_COMMAND_SCROLL_LINEDOWN(HSBarID, ProjectWindow::OnScrollRightButton)
EVT_COMMAND_SCROLL(HSBarID, ProjectWindow::OnScroll)
EVT_COMMAND_SCROLL(VSBarID, ProjectWindow::OnScroll)
// Fires for menu with ID #1...first menu defined
EVT_UPDATE_UI(1, ProjectWindow::OnUpdateUI)
EVT_COMMAND(wxID_ANY, EVT_TOOLBAR_UPDATED, ProjectWindow::OnToolBarUpdate)
//mchinen:multithreaded calls - may not be threadsafe with CommandEvent: may have to change.
END_EVENT_TABLE()

void ProjectWindow::ApplyUpdatedTheme()
{
    SetBackgroundColour(theTheme.Colour(clrMedium));
    ClearBackground();// For wxGTK.
}

void ProjectWindow::OnThemeChange(ThemeChangeMessage message)
{
    auto pProject = FindProject();
    if (!pProject) {
        return;
    }
    auto& project = *pProject;

    if (message.appearance) {
        return;
    }
    this->ApplyUpdatedTheme();
    auto& toolManager = ToolManager::Get(project);
    toolManager.ForEach([](auto pToolBar){
        if (pToolBar) {
            pToolBar->ReCreateButtons();
        }
    });
}

void ProjectWindow::UpdatePrefs()
{
    // Update status bar widths in case of language change
    UpdateStatusWidths();
}

#include "AllThemeResources.h"

void ProjectWindow::OnScrollLeftButton(wxScrollEvent&)
{
    auto pProject = FindProject();
    if (!pProject) {
        return;
    }
    Viewport::Get(*pProject).OnScrollLeftButton();
}

void ProjectWindow::OnScrollRightButton(wxScrollEvent&)
{
    auto pProject = FindProject();
    if (!pProject) {
        return;
    }
    Viewport::Get(*pProject).OnScrollRightButton();
}

std::pair<int, int> ProjectWindow::ViewportSize() const
{
    auto pProject = FindProject();
    if (!pProject) {
        return { 0, 0 }
    }
    auto& project = *pProject;
    auto& trackPanel = TrackPanel::Get(project);
    int width, height;
    trackPanel.GetSize(&width, &height);
    return { width, height };
}

unsigned ProjectWindow::MinimumTrackHeight()
{
    return CommonTrackInfo::MinimumTrackHeight();
}

bool ProjectWindow::IsTrackMinimized(const Track& track)
{
    return ChannelView::Get(*track.GetChannel(0)).GetMinimized();
}

void ProjectWindow::SetMinimized(Track& track, bool minimized)
{
    for (auto pChannel : track.Channels()) {
        ChannelView::Get(*pChannel).SetMinimized(minimized);
    }
}

int ProjectWindow::GetTrackHeight(const Track& track)
{
    return ChannelView::GetChannelGroupHeight(&track);
}

int ProjectWindow::GetTotalHeight(const TrackList& trackList)
{
    return ChannelView::GetTotalHeight(trackList);
}

void ProjectWindow::SetChannelHeights(Track& track, unsigned height)
{
    for (auto pChannel : track.Channels()) {
        ChannelView::Get(*pChannel).SetExpandedHeight(height);
    }
}

int ProjectWindow::GetHorizontalThumbPosition() const
{
    return mHsbar->GetThumbPosition();
}

int ProjectWindow::GetHorizontalThumbSize() const
{
    return mHsbar->GetThumbSize();
}

int ProjectWindow::GetHorizontalRange() const
{
    return mHsbar->GetRange();
}

void ProjectWindow::SetHorizontalThumbPosition(int viewStart)
{
    mHsbar->SetThumbPosition(viewStart);
}

void ProjectWindow::SetHorizontalScrollbar(int position, int thumbSize,
                                           int range, int pageSize, bool refresh)
{
    mHsbar->SetScrollbar(position, thumbSize, range, pageSize, refresh);
}

void ProjectWindow::ShowHorizontalScrollbar(bool shown)
{
#ifdef __WXGTK__
    mHsbar->Show(shown);
#else
    mHsbar->Enable(shown);
#endif
}

int ProjectWindow::GetVerticalThumbPosition() const
{
    return mVsbar->GetThumbPosition();
}

int ProjectWindow::GetVerticalThumbSize() const
{
    return mVsbar->GetThumbSize();
}

int ProjectWindow::GetVerticalRange() const
{
    return mVsbar->GetRange();
}

void ProjectWindow::SetVerticalThumbPosition(int viewStart)
{
    mVsbar->SetThumbPosition(viewStart);
}

void ProjectWindow::SetVerticalScrollbar(int position, int thumbSize,
                                         int range, int pageSize, bool refresh)
{
    mVsbar->SetScrollbar(position, thumbSize, range, pageSize, refresh);
}

void ProjectWindow::ShowVerticalScrollbar(bool shown)
{
#ifdef __WXGTK__
    mVsbar->Show(shown);
#else
    mVsbar->Enable(shown);
#endif
}

void ProjectWindow::UpdateLayout()
{
    auto pProject = FindProject();
    if (!pProject) {
        return;
    }
    auto& project = *pProject;
    auto& trackPanel = GetProjectPanel(project);
    auto& toolManager = ToolManager::Get(project);

    // 1. Layout panel, to get widths of the docks.
    Layout();
    // 2. Layout toolbars to pack the toolbars correctly in docks which
    // are now the correct width.
    toolManager.LayoutToolBars();
    // 3. Layout panel, to resize docks, in particular reducing the height
    // of any empty docks, or increasing the height of docks that need it.
    Layout();

    // Bug 2455
    // The commented out code below is to calculate a nice minimum size for
    // the window.  However on Ubuntu when the window is minimised it leads to
    // an insanely tall window.
    // Using a fixed min size fixes that.
    // However there is still something strange when minimised, as once
    // UpdateLayout is called once, when minimised, it gets called repeatedly.
#if 0
    // Retrieve size of this projects window
    wxSize mainsz = GetSize();

    // Retrieve position of the track panel to use as the size of the top
    // third of the window
    wxPoint tppos = ClientToScreen(trackPanel.GetParent()->GetPosition());

    // Retrieve position of bottom dock to use as the size of the bottom
    // third of the window
    wxPoint sbpos = ClientToScreen(toolManager.GetBotDock()->GetPosition());

    // The "+ 50" is the minimum height of the TrackPanel
    SetMinSize(wxSize(250, (mainsz.y - sbpos.y) + tppos.y + 50));
#endif
    SetMinSize(wxSize(250, 250));
    SetMaxSize(wxSize(20000, 20000));
}

bool ProjectWindow::IsIconized() const
{
    return mIconized;
}

wxWindow* ProjectWindow::GetTrackListWindow() noexcept
{
    return mTrackListWindow;
}

wxSplitterWindow* ProjectWindow::GetContainerWindow() noexcept
{
    return mContainerWindow;
}

wxPanel* ProjectWindow::GetTopPanel() noexcept
{
    return mTopPanel;
}

void ProjectWindow::SetToDefaultSize()
{
    wxRect defaultRect;
    GetDefaultWindowRect(&defaultRect);

    SetSize(defaultRect.width, defaultRect.height);
}

wxStatusBar* ProjectWindow::CreateProjectStatusBar()
{
    auto statusBar = GetStatusBar();

    if (statusBar != nullptr) {
        statusBar->Destroy();
    }

    auto pProject = FindProject();

    // Note that the first field of the status bar is a dummy, and its width is
    // set to zero latter in the code. This field is needed for wxWidgets 2.8.12
    // because if you move to the menu bar, the first field of the menu bar is
    // cleared, which is undesirable behaviour. In addition, the help strings of
    // menu items are by default sent to the first field. Currently there are no
    // such help strings, but it they were introduced, then there would need to
    // be an event handler to send them to the appropriate field.
    statusBar = CreateStatusBar(
        1 + ProjectStatusFieldsRegistry::Count(pProject.get()));

    statusBar->Bind(
        wxEVT_SIZE,
        [this](auto& evt)
    {
        evt.Skip();
        auto pProject = FindProject();
        if (pProject != nullptr) {
            ProjectStatusFieldsRegistry::OnSize(*pProject);
        }
    });

    // We have a new status bar now, we need a full content update
    if (pProject) {
        int index = 1;
        ProjectStatusFieldsRegistry::Visit(
            [&](const StatusBarFieldItem& field, const auto&)
        {
            if (field.IsVisible(*pProject)) {
                statusBar->SetStatusText(
                    field.GetText(*pProject).Translation(), index++);
            }
        });
    }

    return statusBar;
}

void ProjectWindow::UpdateStatusWidths()
{
    auto pProject = FindProject();
    if (!pProject) {
        return;
    }

    const auto fieldsCount
        =ProjectStatusFieldsRegistry::Count(pProject.get()) + 1;
    auto statusBar = GetStatusBar();

    bool statusBarRecreated = false;

    if (!statusBar || fieldsCount != statusBar->GetFieldsCount()) {
        statusBar = CreateProjectStatusBar();
        statusBarRecreated = true;
    }

    const auto& functions = ProjectStatus::GetStatusWidthFunctions();

    auto& project = *pProject;

    std::vector<int> widths(fieldsCount, 0);

    // The old behavior with zero-width first field is kept
    int index = 1;

    ProjectStatusFieldsRegistry::Visit(
        [&](const StatusBarFieldItem& field, const auto&)
    {
        if (!field.IsVisible(project)) {
            return;
        }

        auto width = field.GetDefaultWidth(project);

        // Negative width indicates that the field is expandable
        if (width >= 0) {
            for (const auto& function : functions) {
                auto results = function(project, field.name);
                for (const auto& string : results.first) {
                    int w;
                    statusBar->GetTextExtent(string.Translation(), &w, nullptr);
                    width = std::max<int>(width, w + results.second);
                }
            }
        }

        widths[index++] = width;
    });

    statusBar->SetStatusWidths(fieldsCount, widths.data());
    ProjectStatusFieldsRegistry::OnSize(project);
}

void ProjectWindow::MacShowUndockedToolbars(bool show)
{
    (void)show;//compiler food
#ifdef __WXMAC__
    // Save the focus so we can restore it to whatever had it before since
    // showing a previously hidden toolbar will cause the focus to be set to
    // its frame.  If this is not done it will appear that activation events
    // aren't being sent to the project window since they are actually being
    // delivered to the last tool frame shown.
    wxWindow* focused = FindFocus();

    // Find all the floating toolbars, and show or hide them
    const auto& children = GetChildren();
    for (const auto& child : children) {
        if (auto frame = dynamic_cast<ToolFrame*>(child)) {
            if (!show) {
                frame->Hide();
            } else if (frame->GetBar()
                       && frame->GetBar()->IsVisible()) {
                frame->Show();
            }
        }
    }

    // Restore the focus if needed
    if (focused) {
        focused->SetFocus();
    }
#endif
}

void ProjectWindow::OnIconize(wxIconizeEvent& event)
{
    //JKC: On Iconizing we get called twice.  Don't know
    // why but it does no harm.
    // Should we be returning true/false rather than
    // void return?  I don't know.
    mIconized = event.IsIconized();

#if defined(__WXMAC__)
    // Readdresses bug 1431 since a crash could occur when restoring iconized
    // floating toolbars due to recursion (bug 2411).
    MacShowUndockedToolbars(!mIconized);
    if (!mIconized) {
        Raise();
    }
#endif

    // VisibileProjectCount seems to be just a counter for debugging.
    // It's not used outside this function.
    auto VisibleProjectCount = std::count_if(
        AllProjects {}.begin(), AllProjects {}.end(),
        []( const AllProjects::value_type& ptr ){
        return !GetProjectFrame(*ptr).IsIconized();
    }
        );
    event.Skip();

    // This step is to fix part of Bug 2040, where the BackingPanel
    // size was not restored after we leave Iconized state.

    // Queue up a resize event using OnShow so that we
    // refresh the track panel.  But skip this, if we're iconized.
    if (mIconized) {
        return;
    }
    wxShowEvent Evt;
    OnShow(Evt);
}

void ProjectWindow::OnMove(wxMoveEvent& event)
{
    if (!this->IsMaximized() && !this->IsIconized()) {
        SetNormalizedWindowState(this->GetRect());
    }
    event.Skip();
}

void ProjectWindow::OnSize(wxSizeEvent& event)
{
    // (From Debian)
    //
    // (3.) GTK critical warning "IA__gdk_window_get_origin: assertion
    // 'GDK_IS_WINDOW (window)' failed": Received events of type wxSizeEvent
    // on the main project window cause calls to "ClientToScreen" - which is
    // not available until the window is first shown. So the class has to
    // keep track of wxShowEvent events and inhibit those actions until the
    // window is first shown.
    if (mShownOnce) {
        auto pProject = FindProject();
        if (pProject) {
            Viewport::Get(*pProject).HandleResize();
        }
        if (!this->IsMaximized() && !this->IsIconized()) {
            SetNormalizedWindowState(this->GetRect());
        }
    }
    event.Skip();
}

void ProjectWindow::OnShow(wxShowEvent& event)
{
    // Remember that the window has been shown at least once
    mShownOnce = true;

    // (From Debian...see also TrackPanel::OnTimer and AudacityTimer::Notify)
    //
    // Description: Workaround for wxWidgets bug: Reentry in clipboard
    //  The wxWidgets bug http://trac.wxwidgets.org/ticket/16636 prevents
    //  us from doing clipboard operations in wxShowEvent and wxTimerEvent
    //  processing because those event could possibly be processed during
    //  the (not sufficiently protected) Yield() of a first clipboard
    //  operation, causing reentry. Audacity had a workaround in place
    //  for this problem (the class "CaptureEvents"), which however isn't
    //  applicable with wxWidgets 3.0 because it's based on changing the
    //  gdk event handler, a change that would be overridden by wxWidgets's
    //  own gdk event handler change.
    //  Instead, as a NEW workaround, specifically protect those processings
    //  of wxShowEvent and wxTimerEvent that try to do clipboard operations
    //  from being executed within Yield(). This is done by delaying their
    //  execution by posting pure wxWidgets events - which are never executed
    //  during Yield().
    // Author: Martin Stegh  fer <martin@steghoefer.eu>
    //  Bug-Debian: https://bugs.debian.org/765341

    // the actual creation/showing of the window).
    // Post the event instead of calling OnSize(..) directly. This ensures that
    // this is a pure wxWidgets event (no GDK event behind it) and that it
    // therefore isn't processed within the YieldFor(..) of the clipboard
    // operations (workaround for Debian bug #765341).
    // QueueEvent() will take ownership of the event
    GetEventHandler()->QueueEvent(safenew wxSizeEvent(GetSize()));

    // Further processing by default handlers
    event.Skip();
}

///
///  A toolbar has been updated, so handle it like a sizing event.
///
void ProjectWindow::OnToolBarUpdate(wxCommandEvent& event)
{
    auto pProject = FindProject();
    if (pProject) {
        Viewport::Get(*pProject).HandleResize();
    }
    event.Skip(false);            /* No need to propagate any further */
}

void ProjectWindow::OnProjectTitleChange(ProjectFileIOMessage message)
{
    if (message == ProjectFileIOMessage::ProjectTitleChange) {
        auto pProject = FindProject();
        if (!pProject) {
            return;
        }
        auto& project = *pProject;
        const auto& name = ProjectFileIO::Get(project).GetProjectTitle();
        if (name != GetTitle()) {
            SetTitle(name);
            SetName(name); // to make the nvda screen reader read the correct title
        }
    }
}

void ProjectWindow::OnScroll(wxScrollEvent&)
{
    auto pProject = FindProject();
    if (!pProject) {
        return;
    }
    Viewport::Get(*pProject).OnScroll();
}

void ProjectWindow::OnMenu(wxCommandEvent& event)
{
#ifdef __WXMSW__
    // Bug 1642: We can arrive here with bogus menu IDs, which we
    // proceed to process.  So if bogus, don't.
    // The bogus menu IDs are probably generated by controls on the TrackPanel,
    // such as the Project Rate.
    // 17000 is the magic number at which we start our menu.
    // This code would probably NOT be OK on Mac, since we assign
    // some specific ID numbers.
    if (event.GetId() < 17000) {
        event.Skip();
        return;
    }
#endif
    auto pProject = FindProject();
    if (!pProject) {
        return;
    }
    auto& project = *pProject;
    auto& commandManager = CommandManager::Get(project);
    bool handled = commandManager.HandleMenuID(
        event.GetId(), CommandManager::Get(project).GetUpdateFlags(),
        false);

    if (handled) {
        event.Skip(false);
    } else {
        event.ResumePropagation(999);
        event.Skip(true);
    }
}

void ProjectWindow::OnUpdateUI(wxUpdateUIEvent& WXUNUSED(event))
{
    auto pProject = FindProject();
    if (!pProject) {
        return;
    }
    auto& project = *pProject;
    CommandManager::Get(project).UpdateMenus();
}

void ProjectWindow::OnActivate(wxActivateEvent& event)
{
    // Activate events can fire during window teardown, so just
    // ignore them.
    if (IsBeingDeleted()) {
        return;
    }

    auto pProject = FindProject();
    if (!pProject) {
        return;
    }
    auto& project = *pProject;

    mActive = event.GetActive();

    // Under Windows, focus can be "lost" when returning to
    // Audacity from a different application.
    //
    // This was observed by minimizing all windows using WINDOWS+M and
    // then ALT+TAB to return to Audacity.  Focus will be given to the
    // project window frame which is not at all useful.
    //
    // So, we use ToolManager's observation of focus changes in a wxEventFilter.
    // Then, when we receive the
    // activate event, we restore that focus to the child or the track
    // panel if no child had the focus (which probably should never happen).
    if (mActive) {
        auto& toolManager = ToolManager::Get(project);
        SetActiveProject(&project);
        if (!toolManager.RestoreFocus()) {
            GetProjectPanel(project).SetFocus();
        }
    }
    event.Skip();
}

bool ProjectWindow::IsActive()
{
    return mActive;
}

void ProjectWindow::OnMouseEvent(wxMouseEvent& event)
{
    auto pProject = FindProject();
    if (!pProject) {
        return;
    }
    auto& project = *pProject;
    if (event.ButtonDown()) {
        SetActiveProject(&project);
    }
}

ProjectWindow::PlaybackScroller::PlaybackScroller(AudacityProject* project)
    : mProject(project)
{
}

void ProjectWindow::PlaybackScroller::OnTimer()
{
    auto gAudioIO = AudioIO::Get();
    mRecentStreamTime = gAudioIO->GetStreamTime();

    auto cleanup = finally([&]{
        // Propagate the message to other listeners bound to this
        this->Publish({});
    });

    if (!ProjectAudioIO::Get(*mProject).IsAudioActive()) {
        return;
    } else if (mMode == Mode::Refresh) {
        // PRL:  see comments in Scrubbing.cpp for why this is sometimes needed.
        // These unnecessary refreshes cause wheel rotation events to be delivered more uniformly
        // to the application, so scrub speed control is smoother.
        // (So I see at least with OS 10.10 and wxWidgets 3.0.2.)
        // Is there another way to ensure that than by refreshing?
        auto& trackPanel = GetProjectPanel(*mProject);
        trackPanel.Refresh(false);
    } else if (mMode != Mode::Off) {
        // Pan the view, so that we put the play indicator at some fixed
        // fraction of the window width.

        auto& viewInfo = ViewInfo::Get(*mProject);
        auto& trackPanel = GetProjectPanel(*mProject);
        const int posX = viewInfo.TimeToPosition(mRecentStreamTime);
        auto width = viewInfo.GetTracksUsableWidth();
        int deltaX;
        switch (mMode) {
        default:
            wxASSERT(false);
        /* fallthru */
        case Mode::Pinned:
            deltaX
                =posX - width * TracksPrefs::GetPinnedHeadPositionPreference();
            break;
        case Mode::Right:
            deltaX = posX - width;
            break;
        }
        viewInfo.hpos
            =viewInfo.OffsetTimeByPixels(viewInfo.hpos, deltaX, true);
        // Can't scroll too far left
        viewInfo.hpos = std::max(0.0, viewInfo.hpos);
        trackPanel.Refresh(false);
    }
}

void ProjectWindow::OnViewportMessage(const ViewportMessage& message)
{
    // Activate events can fire during window teardown, so just
    // ignore them.
    if (mIsDeleting) {
        return;
    }
    auto pProject = FindProject();
    if (!pProject) {
        return;
    }
    auto& project = *pProject;
    auto& viewInfo = ViewInfo::Get(project);
    auto& trackPanel = GetProjectPanel(project);

    auto [rescroll, scrollbarVisibilityChanged, resize] = message;

    if (rescroll) {
        trackPanel.Refresh(false);
    }

    // why?  Is there a menu item whose availability depends on scroll position
    // or zoom level?
    CommandManager::Get(project).UpdateMenus();

    if (scrollbarVisibilityChanged || resize) {
        UpdateLayout();
    }
}

static ToolManager::TopPanelHook::Scope scope {
    []( wxWindow& window ){
        auto pProjectWindow = dynamic_cast< ProjectWindow* >(&window);
        return pProjectWindow ? pProjectWindow->GetTopPanel() : nullptr;
    } };
