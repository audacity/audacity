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
#include "Project.h"
#include "ProjectAudioIO.h"
#include "ProjectFileIO.h"
#include "ProjectWindows.h"
#include "ProjectStatus.h"
#include "ProjectSnap.h"
#include "RefreshCode.h"
#include "TrackPanelMouseEvent.h"
#include "TrackFocus.h"
#include "UndoManager.h"
#include "ViewInfo.h"
#include "WaveClip.h"
#include "WaveTrack.h"
#include "CommandContext.h"
#include "CommandManager.h"
#include "prefs/ThemePrefs.h"
#include "prefs/TracksPrefs.h"
#include "toolbars/ToolManager.h"
#include "tracks/ui/Scrubbing.h"
#include "tracks/ui/ChannelView.h"
#include "wxPanelWrapper.h"
#include "WindowAccessible.h"

#include "ThemedWrappers.h"

#include <wx/app.h>
#include <wx/display.h>
#include <wx/scrolbar.h>
#include <wx/sizer.h>
#include <wx/splitter.h>
#include <wx/wupdlock.h>
#include <numeric>

#include "TrackPanel.h"

namespace
{
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
int ScreenContaining( wxRect & r ){
   unsigned int n = wxDisplay::GetCount();
   for(unsigned int i = 0;i<n;i++){
      wxDisplay d(i);
      wxRect scr = d.GetClientArea();
      if( scr.Contains( r ) )
         return (int)i;
   }
   return -1;
}

// true IFF TL and BR corners are on a connected display.
// Does not need to check all four.  We just need to check that 
// the window probably is straddling screens in a sensible way.
// If the user wants to use mixed landscape and portrait, they can.
bool CornersOnScreen( wxRect & r ){
   if( wxDisplay::GetFromPoint( r.GetTopLeft()  ) == wxNOT_FOUND) return false;
   if( wxDisplay::GetFromPoint( r.GetBottomRight()  ) == wxNOT_FOUND) return false;
   return true;
}

// true iff we have enough of the top bar to be able to reposition the window.
bool IsWindowAccessible(wxRect *requestedRect)
{
   wxDisplay display;
   wxRect targetTitleRect(requestedRect->GetLeftTop(), requestedRect->GetBottomRight());
   // Hackery to approximate a window top bar size from a window size.
   // and exclude the open/close and borders.
   targetTitleRect.x += 15;
   targetTitleRect.width -= 100;
   if (targetTitleRect.width <  165) targetTitleRect.width = 165;
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
void GetDefaultWindowRect(wxRect *defRect)
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
   if (width < defRect->width)
   {
      defRect->x = (defRect->width - width)/2;
      defRect->width = width;
   }

   if (height < defRect->height)
   {
      defRect->y = (defRect->height - height)/2;
      // Bug 1119 workaround
      // Small adjustment for very small Mac screens.
      // If there is only a tiny space at the top
      // then instead of vertical centre, align to bottom.
      const int pixelsFormenu = 60;
      if( defRect->y < pixelsFormenu )
         defRect->y *=2;
      defRect->height = height;
   }
}

// BG: Calculate where to place the next window (could be the first window)
// BG: Does not store X and Y in prefs. This is intentional.
//
// LL: This should NOT need to be this complicated...FIXME
void GetNextWindowPlacement(wxRect *nextRect, bool *pMaximized, bool *pIconized)
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


   wxRect screenRect( wxGetClientDisplayRect());
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
      }
      else {
         *nextRect = windowRect;
      }
      // Resize, for example if one monitor that was on is now off.
      if (!CornersOnScreen( wxRect(*nextRect).Deflate( 32, 32 ))) {
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
   if (ScreenContaining( wxRect(normalRect).Deflate( 32, 32 ))<0) {
      normalRect = defaultRect;
   }
   if (ScreenContaining( wxRect(windowRect).Deflate( 32, 32 ) )<0) {
      windowRect = defaultRect;
   }

   bool validWindowSize = false;
   ProjectWindow * validProject = NULL;
   for ( auto iter = AllProjects{}.rbegin(), end = AllProjects{}.rend();
      iter != end; ++iter
   ) {
      auto pProject = *iter;
      if (!GetProjectFrame( *pProject ).IsIconized()) {
         validWindowSize = true;
         validProject = &ProjectWindow::Get( *pProject );
         break;
      }
   }
   if (validWindowSize) {
      *nextRect = validProject->GetRect();
      *pMaximized = validProject->IsMaximized();
      *pIconized = validProject->IsIconized();
      // Do not straddle screens.
      if (ScreenContaining( wxRect(*nextRect).Deflate( 32, 32 ) )<0) {
         *nextRect = defaultRect;
      }
   }
   else {
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
   int scr = std::max( 0, wxDisplay::GetFromPoint( p ));
   wxDisplay d( scr );
   screenRect = d.GetClientArea();

   // Now we (possibly) start trimming our rectangle down.
   // Have we hit the right side of the screen?
   wxPoint bottomRight = nextRect->GetBottomRight();
   if (bottomRight.x > (screenRect.GetRight()+edgeSlop)) {
      int newWidth = screenRect.GetWidth() - nextRect->GetLeft();
      if (newWidth < defaultRect.GetWidth()) {
         nextRect->x = windowRect.x;
         nextRect->y = windowRect.y;
         nextRect->width = windowRect.width;
      }
      else {
         nextRect->width = newWidth;
      }
   }

   // Have we hit the bottom of the screen?
   bottomRight = nextRect->GetBottomRight();
   if (bottomRight.y > (screenRect.GetBottom()+edgeSlop)) {
      nextRect->y -= inc;
      bottomRight = nextRect->GetBottomRight();
      if (bottomRight.y > (screenRect.GetBottom()+edgeSlop)) {
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

   void OnSetFocus(wxFocusEvent & e)
   {
      wxWindow *w = e.GetWindow();
      if (w != NULL) {
         w->SetFocus();
      }
   }

   void SetScrollbar(int position, int thumbSize,
                     int range, int pageSize,
                     bool refresh = true) override;

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

   auto changed =
      position != GetThumbPosition() ||
      thumbSize != GetThumbSize() ||
      range != GetRange() ||
      pageSize != GetPageSize();
   if (!changed)
      return;

   wxScrollBar::SetScrollbar(position, thumbSize, range, pageSize, refresh);
}

BEGIN_EVENT_TABLE(ScrollBar, wxScrollBar)
   EVT_SET_FOCUS(ScrollBar::OnSetFocus)
END_EVENT_TABLE()

// Common mouse wheel handling in track panel cells, moved here to avoid
// compilation dependencies on Track, TrackPanel, and Scrubbing at low levels
// which made cycles
static CommonTrackPanelCell::MouseWheelHook::Scope scope{
// Need a bit of memory from one call to the next
[mVertScrollRemainder = 0.0](
   const TrackPanelMouseEvent &evt, AudacityProject *pProject )
mutable -> unsigned {
   using namespace RefreshCode;

   if ( TrackList::Get( *pProject ).empty() )
      // Scrolling and Zoom in and out commands are disabled when there are no tracks.
      // This should be disabled too for consistency.  Otherwise
      // you do see changes in the time ruler.
      return Cancelled;

   unsigned result = RefreshAll;
   const wxMouseEvent &event = evt.event;
   auto &viewInfo = ViewInfo::Get( *pProject );
   Scrubber &scrubber = Scrubber::Get( *pProject );
   auto &window = ProjectWindow::Get( *pProject );
   const auto steps = evt.steps;

   if (event.ShiftDown()
       // Don't pan during smooth scrolling.  That would conflict with keeping
       // the play indicator centered.
       && !scrubber.IsScrollScrubbing()
      )
   {
      // MM: Scroll left/right when used with Shift key down
      window.SetHorizontalThumb(
         viewInfo.OffsetTimeByPixels(
            viewInfo.PositionToTime(0), 50.0 * -steps));
   }
   else if (event.CmdDown())
   {
#if 0
         // JKC: Alternative scroll wheel zooming code
         // using AudacityProject zooming, which is smarter,
         // it keeps selections on screen and centred if it can,
         // also this ensures mousewheel and zoom buttons give same result.
         double ZoomFactor = pow(2.0, steps);
         AudacityProject *p = GetProject();
         if( steps > 0 )
            // PRL:  Track panel refresh may be needed if you reenable this
            // code, but we don't want this file dependent on TrackPanel.cpp
            p->ZoomInByFactor( ZoomFactor );
         else
            p->ZoomOutByFactor( ZoomFactor );
#endif
      // MM: Zoom in/out when used with Control key down
      // We're converting pixel positions to times,
      // counting pixels from the left edge of the track.
      int trackLeftEdge = viewInfo.GetLeftOffset();

      // Time corresponding to mouse position
      wxCoord xx;
      double center_h;
      double mouse_h = viewInfo.PositionToTime(event.m_x, trackLeftEdge);

      // Scrubbing? Expand or contract about the center, ignoring mouse position
      if (scrubber.IsScrollScrubbing())
         center_h = viewInfo.hpos +
            (viewInfo.GetScreenEndTime() - viewInfo.hpos) / 2.0;
      // Zooming out? Focus on mouse.
      else if( steps <= 0 )
         center_h = mouse_h;
      // No Selection? Focus on mouse.
      else if((viewInfo.selectedRegion.t1() - viewInfo.selectedRegion.t0() ) < 0.00001  )
         center_h = mouse_h;
      // Before Selection? Focus on left
      else if( mouse_h < viewInfo.selectedRegion.t0() )
         center_h = viewInfo.selectedRegion.t0();
      // After Selection? Focus on right
      else if( mouse_h > viewInfo.selectedRegion.t1() )
         center_h = viewInfo.selectedRegion.t1();
      // Inside Selection? Focus on mouse
      else
         center_h = mouse_h;

      xx = viewInfo.TimeToPosition(center_h, trackLeftEdge);

      // Time corresponding to last (most far right) audio.
      double audioEndTime = TrackList::Get(*pProject).GetEndTime();

// Disabled this code to fix Bug 1923 (tricky to wheel-zoom right of waveform).
#if 0
      // When zooming in empty space, it's easy to 'lose' the waveform.
      // This prevents it.
      // IF zooming in
      if (steps > 0)
      {
         // IF mouse is to right of audio
         if (center_h > audioEndTime)
            // Zooming brings far right of audio to mouse.
            center_h = audioEndTime;
      }
#endif

      wxCoord xTrackEnd = viewInfo.TimeToPosition( audioEndTime );
      viewInfo.ZoomBy(pow(2.0, steps/4.0));

      double new_center_h = viewInfo.PositionToTime(xx, trackLeftEdge);
      viewInfo.hpos += (center_h - new_center_h);

      // If wave has gone off screen, bring it back.
      // This means that the end of the track stays where it was.
      if( viewInfo.hpos > audioEndTime )
         viewInfo.hpos += audioEndTime - viewInfo.PositionToTime( xTrackEnd );


      result |= FixScrollbars;
   }
   else
   {
#ifdef EXPERIMENTAL_SCRUBBING_SCROLL_WHEEL
      if (scrubber.IsScrubbing()) {
         scrubber.HandleScrollWheel(steps);
         evt.event.Skip(false);
      }
      else
#endif
      {
         // MM: Scroll up/down when used without modifier keys
         double lines = steps * 4 + mVertScrollRemainder;
         mVertScrollRemainder = lines - floor(lines);
         lines = floor(lines);
         auto didSomething = window.ScrollUpDown((int)-lines);
         if (!didSomething)
            result |= Cancelled;
      }
   }

   return result;
} };

AttachedWindows::RegisteredFactory sProjectWindowKey{
   []( AudacityProject &parent ) -> wxWeakRef< wxWindow > {
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

      auto &window = *pWindow;
      // wxGTK3 seems to need to require creating the window using default position
      // and then manually positioning it.
      window.SetPosition(wndRect.GetPosition());

      if(bMaximized) {
         window.Maximize(true);
      }
      else if (bIconized) {
         // if the user close down and iconized state we could start back up and iconized state
         // window.Iconize(TRUE);
      }

      return pWindow;
   }
};

}

ViewportCallbacks::~ViewportCallbacks() = default;

Viewport::Viewport(AudacityProject &project)
   : mwProject{ project.weak_from_this() }
{}

void Viewport::SetCallbacks(ViewportCallbacks *pCallbacks)
{
   mpCallbacks = pCallbacks;
}

ProjectWindow &ProjectWindow::Get( AudacityProject &project )
{
   return GetAttachedWindows(project).Get< ProjectWindow >(sProjectWindowKey);
}

const ProjectWindow &ProjectWindow::Get( const AudacityProject &project )
{
   return Get( const_cast< AudacityProject & >( project ) );
}

ProjectWindow *ProjectWindow::Find( AudacityProject *pProject )
{
   return pProject
      ? GetAttachedWindows(*pProject).Find< ProjectWindow >(sProjectWindowKey)
      : nullptr;
}

const ProjectWindow *ProjectWindow::Find( const AudacityProject *pProject )
{
   return Find( const_cast< AudacityProject * >( pProject ) );
}

void ProjectWindow::OnResetWindow(const CommandContext& context)
{
   auto& project = context.project;
   auto& window = ProjectWindow::Get(project);

   window.Reset();
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

ProjectWindow::ProjectWindow(wxWindow * parent, wxWindowID id,
   const wxPoint & pos,
   const wxSize & size, AudacityProject &project
)  : Viewport{ project }
   , ProjectWindowBase{ parent, id, pos, size, project }
   , mViewportSubscription{
      Viewport::Subscribe(*this, &ProjectWindow::OnViewportMessage) }
{
   Viewport::SetCallbacks(this);

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
   mTopPanel->SetLabel( "Top Panel" );// Not localised
   mTopPanel->SetLayoutDirection(wxLayout_LeftToRight);
   mTopPanel->SetAutoLayout(true);
#ifdef EXPERIMENTAL_DA2
   mTopPanel->SetBackgroundColour(theTheme.Colour( clrMedium ));
#endif

   auto container = safenew wxSplitterWindow(this, wxID_ANY,
      wxDefaultPosition,
      wxDefaultSize,
      wxNO_BORDER | wxSP_LIVE_UPDATE | wxSP_THIN_SASH);
   container->Bind(wxEVT_SPLITTER_DOUBLECLICKED, [](wxSplitterEvent& event){
      //"The default behaviour is to unsplit the window"
      event.Veto();//do noting instead
   });
   container->Bind(wxEVT_SPLITTER_SASH_POS_CHANGING, [=](wxSplitterEvent& event){
      if(event.GetSashPosition() > EffectsPanelMaxWidth)
         //Prevents left panel from expanding further
         event.SetSashPosition(-1);
   });
   mContainerWindow = container;

   mTrackListWindow = safenew wxPanelWrapper(mContainerWindow, wxID_ANY,
      wxDefaultPosition,
      wxDefaultSize,
      wxNO_BORDER);
   mTrackListWindow->SetMinSize({TrackPanelMinWidth, -1});
   mTrackListWindow->SetSizer( safenew wxBoxSizer(wxVERTICAL) );
   mTrackListWindow->SetLabel("Main Panel");// Not localized.
   mTrackListWindow->SetLayoutDirection(wxLayout_LeftToRight);

   mContainerWindow->Initialize(mTrackListWindow);

#ifdef EXPERIMENTAL_DA2
   mTrackListWindow->SetBackgroundColour(theTheme.Colour( clrMedium ));
#endif

   mPlaybackScroller = std::make_unique<PlaybackScroller>( &project );

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

   mUndoSubscription = UndoManager::Get(project)
      .Subscribe([this](UndoRedoMessage message){
         switch (message.type) {
         case UndoRedoMessage::Pushed:
         case UndoRedoMessage::Modified:
            return OnUndoPushedModified();
         case UndoRedoMessage::UndoOrRedo:
            return OnUndoRedo();
         case UndoRedoMessage::Reset:
            return OnUndoReset();
         default:
            return;
         }
      });

   mThemeChangeSubscription =
      theTheme.Subscribe(*this, &ProjectWindow::OnThemeChange);

   // Subscribe to title changes published by ProjectFileIO
   mTitleChangeSubscription = ProjectFileIO::Get(project)
      .Subscribe(*this, &ProjectWindow::OnProjectTitleChange);

   // Subscribe to snapping changes
   mSnappingChangedSubscription =
      ProjectSnap::Get(project).Subscribe([this](auto) { RedrawProject(); });
   // And also establish my initial consistency with it
   this->OnProjectTitleChange(ProjectFileIOMessage::ProjectTitleChange);
}

ProjectWindow::~ProjectWindow()
{
   // Tool manager gives us capture sometimes
   if(HasCapture())
      ReleaseMouse();
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
   SetBackgroundColour(theTheme.Colour( clrMedium ));
   ClearBackground();// For wxGTK.
}

void ProjectWindow::RedrawProject()
{
   auto pThis = wxWeakRef<ProjectWindow>(this);
   CallAfter( [pThis]{

   if (!pThis)
      return;
   if (pThis->IsBeingDeleted())
      return;

   auto pProject = pThis->FindProject();
   if (!pProject)
      return;

   auto &project = *pProject;
   auto &tracks = TrackList::Get( project );
   auto &trackPanel = GetProjectPanel( project );
   pThis->UpdateScrollbarsForTracks();
   trackPanel.Refresh(false);

   });
}

void ProjectWindow::OnThemeChange(ThemeChangeMessage message)
{
   auto pProject = FindProject();
   if (!pProject)
      return;
   auto &project = *pProject;

   if (message.appearance)
      return;
   this->ApplyUpdatedTheme();
   auto &toolManager = ToolManager::Get( project );
   toolManager.ForEach([](auto pToolBar){
      if( pToolBar )
         pToolBar->ReCreateButtons();
   });
}

void ProjectWindow::UpdatePrefs()
{
   // Update status bar widths in case of language change
   UpdateStatusWidths();
}

void Viewport::FinishAutoScroll()
{
   // Set a flag so we don't have to generate two update events
   mAutoScrolling = true;

   // Call our Scroll method which updates our ViewInfo variables
   // to reflect the positions of the scrollbars
   DoScroll();

   mAutoScrolling = false;
}

#if defined(__WXMAC__)
// const int sbarSpaceWidth = 15;
// const int sbarControlWidth = 16;
// const int sbarExtraLen = 1;
const int sbarHjump = 30;       //STM: This is how far the thumb jumps when the l/r buttons are pressed, or auto-scrolling occurs -- in pixels
#elif defined(__WXMSW__)
const int sbarSpaceWidth = 16;
const int sbarControlWidth = 16;
const int sbarExtraLen = 0;
const int sbarHjump = 30;       //STM: This is how far the thumb jumps when the l/r buttons are pressed, or auto-scrolling occurs -- in pixels
#else // wxGTK, wxMOTIF, wxX11
const int sbarSpaceWidth = 15;
const int sbarControlWidth = 15;
const int sbarExtraLen = 0;
const int sbarHjump = 30;       //STM: This is how far the thumb jumps when the l/r buttons are pressed, or auto-scrolling occurs -- in pixels
#include "AllThemeResources.h"
#endif

// Make sure selection edge is in view
void ProjectWindow::ScrollIntoView(double pos)
{
   auto pProject = FindProject();
   if (!pProject)
      return;
   auto &project = *pProject;
   auto &trackPanel = GetProjectPanel( project );
   auto &viewInfo = ViewInfo::Get( project );
   auto w = viewInfo.GetTracksUsableWidth();

   int pixel = viewInfo.TimeToPosition(pos);
   if (pixel < 0 || pixel >= w)
   {
      SetHorizontalThumb(viewInfo.OffsetTimeByPixels(pos, -(w / 2)));
      trackPanel.Refresh(false);
   }
}

void ProjectWindow::ScrollIntoView(int x)
{
   auto pProject = FindProject();
   if (!pProject)
      return;
   auto &project = *pProject;
   auto &viewInfo = ViewInfo::Get( project );
   ScrollIntoView(viewInfo.PositionToTime(x, viewInfo.GetLeftOffset()));
}

void Viewport::OnScrollLeft()
{
   auto pProject = LockProject();
   if (!pProject)
      return;
   auto &project = *pProject;
   auto &viewInfo = ViewInfo::Get( project );
   wxInt64 pos = mpCallbacks ? mpCallbacks->GetHorizontalThumbPosition() : 0;
   const auto prevPos = pos;
   // move at least one scroll increment
   pos -= std::max<wxInt64>((sbarHjump * viewInfo.sbarScale), 1);
   pos = std::max<wxInt64>(pos, 0);
   viewInfo.sbarH -= sbarHjump;
   viewInfo.sbarH = std::max(viewInfo.sbarH,
      -(wxInt64) PixelWidthBeforeTime(0.0));


   if (mpCallbacks && pos != prevPos) {
      mpCallbacks->SetHorizontalThumbPosition(static_cast<int>(pos));
      FinishAutoScroll();
   }
}

void Viewport::OnScrollRight()
{
   auto pProject = LockProject();
   if (!pProject)
      return;
   auto &project = *pProject;
   auto &viewInfo = ViewInfo::Get( project );
   wxInt64 pos = mpCallbacks ? mpCallbacks->GetHorizontalThumbPosition() : 0;
   const auto prevPos = pos;
   // move at least one scroll increment
   // use wxInt64 for calculation to prevent temporary overflow
   pos += std::max<wxInt64>((sbarHjump * viewInfo.sbarScale), 1);
   wxInt64 max = mpCallbacks
      ? mpCallbacks->GetHorizontalRange()
         - mpCallbacks->GetHorizontalThumbSize()
      : 0;
   pos = std::min(pos, max);
   viewInfo.sbarH += sbarHjump;
   viewInfo.sbarH = std::min(viewInfo.sbarH,
      viewInfo.sbarTotal
         - (wxInt64) PixelWidthBeforeTime(0.0) - viewInfo.sbarScreen);

   if (mpCallbacks && pos != prevPos) {
      mpCallbacks->SetHorizontalThumbPosition(static_cast<int>(pos));
      FinishAutoScroll();
   }
}

void ProjectWindow::OnScrollLeftButton(wxScrollEvent &)
{
   Viewport::OnScrollLeftButton();
}

void Viewport::OnScrollLeftButton()
{
   auto pProject = LockProject();
   if (!pProject)
      return;
   auto &project = *pProject;
   auto &viewInfo = ViewInfo::Get( project );
   wxInt64 pos = mpCallbacks ? mpCallbacks->GetHorizontalThumbPosition() : 0;
   const auto prevPos = pos;
   // move at least one scroll increment
   pos -= std::max<wxInt64>((sbarHjump * viewInfo.sbarScale), 1);
   pos = std::max<wxInt64>(pos, 0);
   viewInfo.sbarH -= sbarHjump;
   viewInfo.sbarH = std::max(viewInfo.sbarH,
      - (wxInt64) PixelWidthBeforeTime(0.0));

   if (mpCallbacks && pos != prevPos) {
      mpCallbacks->SetHorizontalThumbPosition(static_cast<int>(pos));
      DoScroll();
   }
}

void ProjectWindow::OnScrollRightButton(wxScrollEvent &)
{
   Viewport::OnScrollRightButton();
}

void Viewport::OnScrollRightButton()
{
   auto pProject = LockProject();
   if (!pProject)
      return;
   auto &project = *pProject;
   auto &viewInfo = ViewInfo::Get( project );
   wxInt64 pos = mpCallbacks ? mpCallbacks->GetHorizontalThumbPosition() : 0;
   const auto prevPos = pos;
   // move at least one scroll increment
   // use wxInt64 for calculation to prevent temporary overflow
   pos += std::max<wxInt64>((sbarHjump * viewInfo.sbarScale), 1);
   wxInt64 max = mpCallbacks
      ? mpCallbacks->GetHorizontalRange()
         - mpCallbacks->GetHorizontalThumbSize()
      : 0;
   pos = std::min(pos, max);
   viewInfo.sbarH += sbarHjump;
   viewInfo.sbarH = std::min(viewInfo.sbarH,
      viewInfo.sbarTotal
         - (wxInt64) PixelWidthBeforeTime(0.0) - viewInfo.sbarScreen);

   if (mpCallbacks && pos != prevPos) {
      mpCallbacks->SetHorizontalThumbPosition(static_cast<int>(pos));
      DoScroll();
   }
}

std::pair<int, int> ProjectWindow::ViewportSize() const
{
   auto pProject = FindProject();
   if (!pProject)
      return { 0, 0 };
   auto &project = *pProject;
   auto &trackPanel = TrackPanel::Get(project);
   int width, height;
   trackPanel.GetSize(&width, &height);
   return { width, height };
}

bool ProjectWindow::MayScrollBeyondZero() const
{
   auto pProject = FindProject();
   if (!pProject)
      return false;
   auto &project = *pProject;
   auto &scrubber = Scrubber::Get( project );
   auto &viewInfo = ViewInfo::Get( project );
   if (viewInfo.bScrollBeyondZero)
      return true;

   if (scrubber.HasMark() ||
       ProjectAudioIO::Get( project ).IsAudioActive()) {
      if (mPlaybackScroller) {
         auto mode = mPlaybackScroller->GetMode();
         if (mode == PlaybackScroller::Mode::Pinned ||
             mode == PlaybackScroller::Mode::Right)
            return true;
      }
   }

   return false;
}

unsigned ProjectWindow::MinimumTrackHeight()
{
   return CommonTrackInfo::MinimumTrackHeight();
}

bool ProjectWindow::IsTrackMinimized(const Track &track)
{
   return ChannelView::Get(*track.GetChannel(0)).GetMinimized();
}

void ProjectWindow::SetMinimized(Track &track, bool minimized)
{
   for (auto pChannel : track.Channels())
      ChannelView::Get(*pChannel).SetMinimized(minimized);
}

int ProjectWindow::GetTrackHeight(const Track &track)
{
   return ChannelView::GetChannelGroupHeight(&track);
}

int ProjectWindow::GetTotalHeight(const TrackList &trackList)
{
   return ChannelView::GetTotalHeight(trackList);
}

void ProjectWindow::SetChannelHeights(Track &track, unsigned height)
{
   for (auto pChannel : track.Channels())
      ChannelView::Get(*pChannel).SetExpandedHeight(height);
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

double Viewport::ScrollingLowerBoundTime() const
{
   auto pProject = LockProject();
   if (!pProject)
      return 0;
   auto &project = *pProject;
   auto &tracks = TrackList::Get(project);
   auto &viewInfo = ViewInfo::Get(project);
   if (!(mpCallbacks && mpCallbacks->MayScrollBeyondZero()))
      return 0;
   const double screen = viewInfo.GetScreenEndTime() - viewInfo.hpos;
   return std::min(tracks.GetStartTime(), -screen);
}

// PRL: Bug1197: we seem to need to compute all in double, to avoid differing results on Mac
// That's why ViewInfo::TimeRangeToPixelWidth was defined, with some regret.
double Viewport::PixelWidthBeforeTime(double scrollto) const
{
   auto pProject = LockProject();
   if (!pProject)
      return 0;
   auto &project = *pProject;
   auto &viewInfo = ViewInfo::Get( project );
   const double lowerBound = ScrollingLowerBoundTime();
   return
      // Ignoring fisheye is correct here
      viewInfo.TimeRangeToPixelWidth(scrollto - lowerBound);
}

void Viewport::SetHorizontalThumb(double scrollto, bool doScroll)
{
   if (!mpCallbacks)
      return;
   auto pProject = LockProject();
   if (!pProject)
      return;
   auto &project = *pProject;
   auto &viewInfo = ViewInfo::Get( project );
   const auto unscaled = PixelWidthBeforeTime(scrollto);
   const int max =
      mpCallbacks->GetHorizontalRange() - mpCallbacks->GetHorizontalThumbSize();
   const int pos =
      std::min(max,
         std::max(0,
            (int)(floor(0.5 + unscaled * viewInfo.sbarScale))));
   mpCallbacks->SetHorizontalThumbPosition(pos);
   viewInfo.sbarH = floor(0.5 + unscaled - PixelWidthBeforeTime(0.0));
   viewInfo.sbarH = std::max(viewInfo.sbarH,
      - (wxInt64) PixelWidthBeforeTime(0.0));
   viewInfo.sbarH = std::min(viewInfo.sbarH,
      viewInfo.sbarTotal
         - (wxInt64) PixelWidthBeforeTime(0.0) - viewInfo.sbarScreen);

   if (doScroll)
      DoScroll();
}

// Scroll vertically. This is called for example by the mouse wheel
// handler in Track Panel. A positive argument makes the window
// scroll down, while a negative argument scrolls up.
//
bool ProjectWindow::ScrollUpDown(int delta)
{
   int oldPos = mVsbar->GetThumbPosition();
   int pos = oldPos + delta;
   int max = mVsbar->GetRange() - mVsbar->GetThumbSize();

   // Can be negative in case of only one track
   if (max < 0)
      max = 0;

   if (pos > max)
      pos = max;
   else if (pos < 0)
      pos = 0;

   if (pos != oldPos)
   {
      mVsbar->SetThumbPosition(pos);

      DoScroll();
      return true;
   }
   else
      return false;
}

void ProjectWindow::UpdateScrollbarsForTracks()
{
   auto pProject = FindProject();
   if (!pProject)
      return;
   auto &project = *pProject;
   auto &tracks = TrackList::Get( project );
   auto &trackPanel = GetProjectPanel( project );
   auto &viewInfo = ViewInfo::Get( project );

   // To decide whether to repaint the view
   bool refresh = false;
   bool rescroll = false;

   // Gather inputs
   const int totalHeight = GetTotalHeight(tracks) + 32;

   // (From Debian...at least I think this is the change corresponding
   // to this comment)
   //
   // (2.) GTK critical warning "IA__gtk_range_set_range: assertion
   // 'min < max' failed" because of negative numbers as result of window
   // size checking. Added a sanity check that straightens up the numbers
   // in edge cases.
   const auto panelWidth = std::max(0, viewInfo.GetTracksUsableWidth());
   const auto panelHeight = std::max(0, viewInfo.GetHeight());

   const auto LastTime = std::accumulate(tracks.begin(), tracks.end(),
      viewInfo.selectedRegion.t1(),
      [](double acc, const Track *track){
         // Iterate over pending changed tracks if present.
         track = track->SubstitutePendingChangedTrack().get();
         return std::max(acc, track->GetEndTime());
      });

   const double screen = viewInfo.GetScreenEndTime() - viewInfo.hpos;
   const double halfScreen = screen / 2.0;

   // If we can scroll beyond zero,
   // Add 1/2 of a screen of blank space to the end
   // and another 1/2 screen before the beginning
   // so that any point within the union of the selection and the track duration
   // may be scrolled to the midline.
   // May add even more to the end, so that you can always scroll a negative
   // starting time up to the left edge.
   const double lowerBound = ScrollingLowerBoundTime();
   const double additional = MayScrollBeyondZero()
      ? -lowerBound + std::max(halfScreen, screen - LastTime)
      : screen / 4.0;

   viewInfo.total = LastTime + additional;

   // Don't remove time from total that's still on the screen
   viewInfo.total = std::max(viewInfo.total, viewInfo.hpos + screen);

   // Scroll the view later if needed to respect the lower bound
   if (viewInfo.hpos < lowerBound) {
      viewInfo.hpos = lowerBound;
      rescroll = true;
   }

   // To compute new horizontal scrollbar settings
   viewInfo.sbarTotal = (wxInt64) (viewInfo.GetTotalWidth());
   viewInfo.sbarScreen = (wxInt64)(panelWidth);
   viewInfo.sbarH = (wxInt64) (viewInfo.GetBeforeScreenWidth());

   // PRL:  Can someone else find a more elegant solution to bug 812, than
   // introducing this boolean member variable?
   // Setting mVSbar earlier, in HandlXMLTag, didn't succeed in restoring
   // the vertical scrollbar to its saved position.  So defer that till now.
   // mbInitializingScrollbar should be true only at the start of the life
   // of an AudacityProject reopened from disk.
   if (!mbInitializingScrollbar)
      viewInfo.vpos = mVsbar->GetThumbPosition() * viewInfo.scrollStep;
   mbInitializingScrollbar = false;

   // Constrain new top of visible area
   viewInfo.vpos = std::clamp(viewInfo.vpos, 0, totalHeight - 1);

   // Decide whether the tracks are large enough to scroll for the zoom level
   // and heights
   bool oldhstate{};
   bool oldvstate{};
   bool newhstate =
      (viewInfo.GetScreenEndTime() - viewInfo.hpos) < viewInfo.total;
   bool newvstate = panelHeight < totalHeight;

   // Hide scrollbar thumbs and buttons if not scrollable
#ifdef __WXGTK__
   oldhstate = mHsbar->IsShown();
   oldvstate = mVsbar->IsShown();
   mHsbar->Show(newhstate);
   mVsbar->Show(panelHeight < totalHeight);
#else
   oldhstate = mHsbar->IsEnabled();
   oldvstate = mVsbar->IsEnabled();
   mHsbar->Enable(newhstate);
   mVsbar->Enable(panelHeight < totalHeight);
#endif

   // When not scrollable in either axis, align viewport to top or left and
   // repaint it later
   if (!newvstate && viewInfo.vpos != 0) {
      viewInfo.vpos = 0;

      refresh = true;
      rescroll = false;
   }
   if (!newhstate && viewInfo.sbarH != 0) {
      viewInfo.sbarH = 0;

      refresh = true;
      rescroll = false;
   }

   // wxScrollbar only supports int values but we need a greater range, so
   // we scale the scrollbar coordinates on demand. We only do this if we
   // would exceed the int range, so we can always use the maximum resolution
   // available.

   // Don't use the full 2^31 max int range but a bit less, so rounding
   // errors in calculations do not overflow max int
   wxInt64 maxScrollbarRange = (wxInt64)(2147483647 * 0.999);
   if (viewInfo.sbarTotal > maxScrollbarRange)
      viewInfo.sbarScale = ((double)maxScrollbarRange) / viewInfo.sbarTotal;
   else
      viewInfo.sbarScale = 1.0; // use maximum resolution

   {
      int scaledSbarH = (int)(viewInfo.sbarH * viewInfo.sbarScale);
      int scaledSbarScreen = (int)(viewInfo.sbarScreen * viewInfo.sbarScale);
      int scaledSbarTotal = (int)(viewInfo.sbarTotal * viewInfo.sbarScale);
      const int offset =
         (int)(floor(0.5 + viewInfo.sbarScale * PixelWidthBeforeTime(0.0)));

      mHsbar->SetScrollbar(scaledSbarH + offset, scaledSbarScreen, scaledSbarTotal,
         scaledSbarScreen, true);
   }

   // Vertical scrollbar
   mVsbar->SetScrollbar(viewInfo.vpos / viewInfo.scrollStep,
                        panelHeight / viewInfo.scrollStep,
                        totalHeight / viewInfo.scrollStep,
                        panelHeight / viewInfo.scrollStep, true);

   if (refresh || (rescroll &&
      (viewInfo.GetScreenEndTime() - viewInfo.hpos) < viewInfo.total))
      trackPanel.Refresh(false);

   CommandManager::Get(project).UpdateMenus();

   if (oldhstate != newhstate || oldvstate != newvstate) {
      UpdateLayout();
   }
}

void ProjectWindow::UpdateLayout()
{
   auto pProject = FindProject();
   if (!pProject)
      return;
   auto &project = *pProject;
   auto &trackPanel = GetProjectPanel( project );
   auto &toolManager = ToolManager::Get( project );

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
   SetMinSize( wxSize(250, (mainsz.y - sbpos.y) + tppos.y + 50));
#endif
   SetMinSize( wxSize(250, 250));
   SetMaxSize( wxSize(20000, 20000));
}

void ProjectWindow::HandleResize()
{
   // Activate events can fire during window teardown, so just
   // ignore them.
   if (mIsDeleting) {
      return;
   }

   CallAfter( [this]{

   if (mIsDeleting)
      return;

   UpdateScrollbarsForTracks();
   UpdateLayout();

   });
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

void ProjectWindow::Reset()
{
   wxRect defaultRect;
   GetDefaultWindowRect(&defaultRect);

   SetSize(defaultRect.width, defaultRect.height);
}

void ProjectWindow::UpdateStatusWidths()
{
   auto pProject = FindProject();
   if (!pProject)
      return;
   auto &project = *pProject;
   enum { nWidths = nStatusBarFields + 1 };
   int widths[ nWidths ]{ 0 };
   widths[ rateStatusBarField ] = 150;
   const auto statusBar = GetStatusBar();
   const auto &functions = ProjectStatus::GetStatusWidthFunctions();
   // Start from 1 not 0
   // Specifying a first column always of width 0 was needed for reasons
   // I forget now
   for ( int ii = 1; ii <= nStatusBarFields; ++ii ) {
      int &width = widths[ ii ];
      for ( const auto &function : functions ) {
         auto results =
            function( project, static_cast< StatusBarField >( ii ) );
         for ( const auto &string : results.first ) {
            int w;
            statusBar->GetTextExtent(string.Translation(), &w, nullptr);
            width = std::max<int>( width, w + results.second );
         }
      }
   }
   // The main status field is not fixed width
   widths[ mainStatusBarField ] = -1;
   statusBar->SetStatusWidths( nWidths, widths );
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
   wxWindow *focused = FindFocus();

   // Find all the floating toolbars, and show or hide them
   const auto &children = GetChildren();
   for(const auto &child : children) {
      if (auto frame = dynamic_cast<ToolFrame*>(child)) {
         if (!show) {
            frame->Hide();
         }
         else if (frame->GetBar() &&
                  frame->GetBar()->IsVisible() ) {
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

void ProjectWindow::OnIconize(wxIconizeEvent &event)
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
   if( !mIconized )
   {
      Raise();
   }
#endif

   // VisibileProjectCount seems to be just a counter for debugging.
   // It's not used outside this function.
   auto VisibleProjectCount = std::count_if(
      AllProjects{}.begin(), AllProjects{}.end(),
      []( const AllProjects::value_type &ptr ){
         return !GetProjectFrame( *ptr ).IsIconized();
      }
   );
   event.Skip();

   // This step is to fix part of Bug 2040, where the BackingPanel
   // size was not restored after we leave Iconized state.

   // Queue up a resize event using OnShow so that we 
   // refresh the track panel.  But skip this, if we're iconized.
   if( mIconized )
      return;
   wxShowEvent Evt;
   OnShow( Evt );
}

void ProjectWindow::OnMove(wxMoveEvent & event)
{
   if (!this->IsMaximized() && !this->IsIconized())
      SetNormalizedWindowState(this->GetRect());
   event.Skip();
}

void ProjectWindow::OnSize(wxSizeEvent & event)
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
      HandleResize();
      if (!this->IsMaximized() && !this->IsIconized())
         SetNormalizedWindowState(this->GetRect());
   }
   event.Skip();
}

void ProjectWindow::OnShow(wxShowEvent & event)
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
void ProjectWindow::OnToolBarUpdate(wxCommandEvent & event)
{
   HandleResize();

   event.Skip(false);             /* No need to propagate any further */
}

void ProjectWindow::OnUndoPushedModified()
{
   RedrawProject();
}

void ProjectWindow::OnUndoRedo()
{
   HandleResize();
   RedrawProject();
}

void ProjectWindow::OnUndoReset()
{
   HandleResize();
   // RedrawProject();  // Should we do this here too?
}

void ProjectWindow::OnProjectTitleChange(ProjectFileIOMessage message)
{
   if (message == ProjectFileIOMessage::ProjectTitleChange) {
      auto pProject = FindProject();
      if (!pProject)
         return;
      auto &project = *pProject;
      const auto &name = ProjectFileIO::Get(project).GetProjectTitle();
      if (name != GetTitle()) {
         SetTitle(name);
         SetName(name); // to make the nvda screen reader read the correct title
      }
   }
}

void ProjectWindow::OnScroll(wxScrollEvent & WXUNUSED(event))
{
   auto pProject = FindProject();
   if (!pProject)
      return;
   auto &project = *pProject;
   auto &viewInfo = ViewInfo::Get( project );
   const wxInt64 offset = PixelWidthBeforeTime(0.0);
   viewInfo.sbarH =
      (wxInt64)(mHsbar->GetThumbPosition() / viewInfo.sbarScale) - offset;
   DoScroll();

#ifndef __WXMAC__
   // Bug2179
   // This keeps the time ruler in sync with horizontal scrolling, without
   // making an undesirable compilation dependency of this source file on
   // the ruler
   wxTheApp->ProcessIdle();
#endif
}

void Viewport::DoScroll()
{
   auto pProject = LockProject();
   if (!pProject)
      return;
   auto &project = *pProject;
   auto &trackPanel = GetProjectPanel( project );
   auto &viewInfo = ViewInfo::Get( project );
   const double lowerBound = ScrollingLowerBoundTime();

   auto width = viewInfo.GetTracksUsableWidth();
   viewInfo.SetBeforeScreenWidth(viewInfo.sbarH, width, lowerBound);

   if (mpCallbacks && mpCallbacks->MayScrollBeyondZero()) {
      enum { SCROLL_PIXEL_TOLERANCE = 10 };
      if (std::abs(viewInfo.TimeToPosition(0.0, 0
                                   )) < SCROLL_PIXEL_TOLERANCE) {
         // Snap the scrollbar to 0
         viewInfo.hpos = 0;
         SetHorizontalThumb(0.0, false);
      }
   }

   const auto pos = mpCallbacks ? mpCallbacks->GetVerticalThumbPosition() : 0;
   viewInfo.vpos = pos * viewInfo.scrollStep;

   //mchinen: do not always set this project to be the active one.
   //a project may autoscroll while playing in the background
   //I think this is okay since OnMouseEvent has one of these.
   //SetActiveProject(this);

   if (!mAutoScrolling) {
      trackPanel.Refresh(false);
   }
}

void ProjectWindow::OnMenu(wxCommandEvent & event)
{
#ifdef __WXMSW__
   // Bug 1642: We can arrive here with bogus menu IDs, which we
   // proceed to process.  So if bogus, don't.
   // The bogus menu IDs are probably generated by controls on the TrackPanel, 
   // such as the Project Rate.
   // 17000 is the magic number at which we start our menu.
   // This code would probably NOT be OK on Mac, since we assign
   // some specific ID numbers.
   if( event.GetId() < 17000){
      event.Skip();
      return;
   }
#endif
   auto pProject = FindProject();
   if (!pProject)
      return;
   auto &project = *pProject;
   auto &commandManager = CommandManager::Get( project );
   bool handled = commandManager.HandleMenuID(
      event.GetId(), CommandManager::Get( project ).GetUpdateFlags(),
      false);

   if (handled)
      event.Skip(false);
   else{
      event.ResumePropagation( 999 );
      event.Skip(true);
   }
}

void ProjectWindow::OnUpdateUI(wxUpdateUIEvent & WXUNUSED(event))
{
   auto pProject = FindProject();
   if (!pProject)
      return;
   auto &project = *pProject;
   CommandManager::Get(project).UpdateMenus();
}

void ProjectWindow::OnActivate(wxActivateEvent & event)
{
   // Activate events can fire during window teardown, so just
   // ignore them.
   if (IsBeingDeleted()) {
      return;
   }

   auto pProject = FindProject();
   if (!pProject)
      return;
   auto &project = *pProject;

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
      auto &toolManager = ToolManager::Get( project );
      SetActiveProject( &project );
      if ( ! toolManager.RestoreFocus() )
         GetProjectPanel( project ).SetFocus();
   }
   event.Skip();
}

bool ProjectWindow::IsActive()
{
   return mActive;
}

void ProjectWindow::OnMouseEvent(wxMouseEvent & event)
{
   auto pProject = FindProject();
   if (!pProject)
      return;
   auto &project = *pProject;
   if (event.ButtonDown())
      SetActiveProject( &project );
}

void ProjectWindow::ZoomAfterImport(Track *pTrack)
{
   auto pProject = FindProject();
   if (!pProject)
      return;
   auto &project = *pProject;
   auto &tracks = TrackList::Get( project );
   auto &trackPanel = GetProjectPanel( project );

   ZoomFitHorizontally();

   trackPanel.SetFocus();
   if (!pTrack)
      pTrack = *tracks.Selected().begin();
   if (!pTrack)
      pTrack = *tracks.begin();
   if (pTrack) {
      TrackFocus::Get(project).Set(pTrack);
      pTrack->EnsureVisible();
   }
}

// Utility function called by other zoom methods
void ProjectWindow::Zoom(double level)
{
   auto pProject = FindProject();
   if (!pProject)
      return;
   auto &project = *pProject;
   auto &viewInfo = ViewInfo::Get( project );
   viewInfo.SetZoom(level);
   UpdateScrollbarsForTracks();
   // See if we can center the selection on screen, and have it actually fit.
   // tOnLeft is the amount of time we would need before the selection left edge to center it.
   float t0 = viewInfo.selectedRegion.t0();
   float t1 = viewInfo.selectedRegion.t1();
   float tAvailable = viewInfo.GetScreenEndTime() - viewInfo.hpos;
   float tOnLeft = (tAvailable - t0 + t1)/2.0;
   // Bug 1292 (Enh) is effectively a request to do this scrolling of  the selection into view.
   // If tOnLeft is positive, then we have room for the selection, so scroll to it.
   if (tOnLeft >= 0)
      SetHorizontalThumb(t0 - tOnLeft);
}

// Utility function called by other zoom methods
void ProjectWindow::ZoomBy(double multiplier)
{
   auto pProject = FindProject();
   if (!pProject)
      return;
   auto &project = *pProject;
   auto &viewInfo = ViewInfo::Get( project );
   viewInfo.ZoomBy(multiplier);
   UpdateScrollbarsForTracks();
}

void Viewport::ScrollToStart(bool extend)
{
   auto pProject = LockProject();
   if (!pProject)
      return;
   auto &project = *pProject;
   auto &viewInfo = ViewInfo::Get( project );
   viewInfo.selectedRegion.setT0(0, false);
   if (!extend)
      viewInfo.selectedRegion.setT1(0);

   SetHorizontalThumb(0);
}

void Viewport::ScrollToTop()
{
   if (mpCallbacks)
      mpCallbacks->SetVerticalThumbPosition(0);
}


///////////////////////////////////////////////////////////////////
// This method 'fast-forwards' the track, by setting the cursor to
// the end of the samples on the selected track and  scrolling the
//  window to fit the end on its right side (maintaining  current zoom).
// If shift is held down, it will extend the right edge of the
// selection to the end (holding left edge constant), otherwise it will
// move both left and right edge of selection to the end
///////////////////////////////////////////////////////////////////
void ProjectWindow::SkipEnd(bool shift)
{
   auto pProject = FindProject();
   if (!pProject)
      return;
   auto &project = *pProject;
   auto &tracks = TrackList::Get(project);
   auto &viewInfo = ViewInfo::Get(project);
   double len = tracks.GetEndTime();

   viewInfo.selectedRegion.setT1(len, false);
   if (!shift)
      viewInfo.selectedRegion.setT0(len);

   // Make sure the end of the track is visible
   ScrollIntoView(len);
}

void ProjectWindow::ScrollToBottom()
{
   auto pProject = FindProject();
   if (!pProject)
      return;
   auto &project = *pProject;
   auto &tracks = TrackList::Get(project);
   auto &trackPanel = GetProjectPanel(project);
   auto &viewInfo = ViewInfo::Get(project);

   auto range = tracks.Any();
   int trackHeight = 0;
   if (!range.empty()) {
      trackHeight = GetTrackHeight(**range.rbegin());
      --range.second;
   }
   int trackTop =
      range.sum([this](auto pTrack){ return GetTrackHeight(*pTrack); });
   const auto [width, height] = ViewportSize();
   const auto step = viewInfo.scrollStep;
   const int delta = ((trackTop + trackHeight - height) - viewInfo.vpos
      + step) / step;
   ScrollUpDown(delta);
   trackPanel.Refresh(false);
}

ProjectWindow::PlaybackScroller::PlaybackScroller(AudacityProject *project)
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

   if(!ProjectAudioIO::Get( *mProject ).IsAudioActive())
      return;
   else if (mMode == Mode::Refresh) {
      // PRL:  see comments in Scrubbing.cpp for why this is sometimes needed.
      // These unnecessary refreshes cause wheel rotation events to be delivered more uniformly
      // to the application, so scrub speed control is smoother.
      // (So I see at least with OS 10.10 and wxWidgets 3.0.2.)
      // Is there another way to ensure that than by refreshing?
      auto &trackPanel = GetProjectPanel( *mProject );
      trackPanel.Refresh(false);
   }
   else if (mMode != Mode::Off) {
      // Pan the view, so that we put the play indicator at some fixed
      // fraction of the window width.

      auto &viewInfo = ViewInfo::Get( *mProject );
      auto &trackPanel = GetProjectPanel( *mProject );
      const int posX = viewInfo.TimeToPosition(mRecentStreamTime);
      auto width = viewInfo.GetTracksUsableWidth();
      int deltaX;
      switch (mMode)
      {
         default:
            wxASSERT(false);
            /* fallthru */
         case Mode::Pinned:
            deltaX =
               posX - width * TracksPrefs::GetPinnedHeadPositionPreference();
            break;
         case Mode::Right:
            deltaX = posX - width;        break;
      }
      viewInfo.hpos =
         viewInfo.OffsetTimeByPixels(viewInfo.hpos, deltaX, true);
      if (!ProjectWindow::Get( *mProject ).MayScrollBeyondZero())
         // Can't scroll too far left
         viewInfo.hpos = std::max(0.0, viewInfo.hpos);
      trackPanel.Refresh(false);
   }
}

void ProjectWindow::ZoomInByFactor( double ZoomFactor )
{
   auto pProject = FindProject();
   if (!pProject)
      return;
   auto &project = *pProject;
   auto &viewInfo = ViewInfo::Get( project );

   // DMM: Here's my attempt to get logical zooming behavior
   // when there's a selection that's currently at least
   // partially on-screen

   const double endTime = viewInfo.GetScreenEndTime();
   const double duration = endTime - viewInfo.hpos;

   bool selectionIsOnscreen =
      (viewInfo.selectedRegion.t0() < endTime) &&
      (viewInfo.selectedRegion.t1() >= viewInfo.hpos);

   bool selectionFillsScreen =
      (viewInfo.selectedRegion.t0() < viewInfo.hpos) &&
      (viewInfo.selectedRegion.t1() > endTime);

   if (selectionIsOnscreen && !selectionFillsScreen) {
      // Start with the center of the selection
      double selCenter = (viewInfo.selectedRegion.t0() +
                          viewInfo.selectedRegion.t1()) / 2;

      // If the selection center is off-screen, pick the
      // center of the part that is on-screen.
      if (selCenter < viewInfo.hpos)
         selCenter = viewInfo.hpos +
                     (viewInfo.selectedRegion.t1() - viewInfo.hpos) / 2;
      if (selCenter > endTime)
         selCenter = endTime -
            (endTime - viewInfo.selectedRegion.t0()) / 2;

      // Zoom in
      ZoomBy(ZoomFactor);
      const double newDuration =
         viewInfo.GetScreenEndTime() - viewInfo.hpos;

      // Recenter on selCenter
      SetHorizontalThumb(selCenter - newDuration / 2);
      return;
   }


   double origLeft = viewInfo.hpos;
   double origWidth = duration;
   ZoomBy(ZoomFactor);

   const double newDuration =
      viewInfo.GetScreenEndTime() - viewInfo.hpos;
   double newh = origLeft + (origWidth - newDuration) / 2;

   // MM: Commented this out because it was confusing users
   /*
   // make sure that the *right-hand* end of the selection is
   // no further *left* than 1/3 of the way across the screen
   if (viewInfo.selectedRegion.t1() < newh + viewInfo.screen / 3)
      newh = viewInfo.selectedRegion.t1() - viewInfo.screen / 3;

   // make sure that the *left-hand* end of the selection is
   // no further *right* than 2/3 of the way across the screen
   if (viewInfo.selectedRegion.t0() > newh + viewInfo.screen * 2 / 3)
      newh = viewInfo.selectedRegion.t0() - viewInfo.screen * 2 / 3;
   */

   SetHorizontalThumb(newh);
}

void ProjectWindow::ZoomOutByFactor( double ZoomFactor )
{
   auto pProject = FindProject();
   if (!pProject)
      return;
   auto &project = *pProject;
   auto &viewInfo = ViewInfo::Get( project );

   //Zoom() may change these, so record original values:
   const double origLeft = viewInfo.hpos;
   const double origWidth = viewInfo.GetScreenEndTime() - origLeft;

   ZoomBy(ZoomFactor);
   const double newWidth = viewInfo.GetScreenEndTime() - viewInfo.hpos;

   const double newh = origLeft + (origWidth - newWidth) / 2;
   // newh = (newh > 0) ? newh : 0;
   SetHorizontalThumb(newh);
}

double ProjectWindow::GetZoomOfToFit() const
{
   auto pProject = FindProject();
   if (!pProject)
      return 1.0;
   auto &project = *pProject;
   auto &tracks = TrackList::Get(project);
   auto &viewInfo = ViewInfo::Get(project);

   const double end = tracks.GetEndTime();
   const double start = viewInfo.bScrollBeyondZero
      ? std::min(tracks.GetStartTime(), 0.0)
      : 0;
   const double len = end - start;

   if (len <= 0.0)
      return viewInfo.GetZoom();

   auto w = viewInfo.GetTracksUsableWidth();
   w -= 10;
   return w/len;
}

void ProjectWindow::ZoomFitHorizontally()
{
   auto pProject = FindProject();
   if (!pProject)
      return;
   auto &project = *pProject;
   auto &viewInfo = ViewInfo::Get(project);
   auto &tracks = TrackList::Get(project);
   auto &window = *this;

   const double start = viewInfo.bScrollBeyondZero
      ? std::min(tracks.GetStartTime(), 0.0)
      : 0;

   window.Zoom( window.GetZoomOfToFit() );
   window.SetHorizontalThumb(start);
}

void Viewport::ZoomFitVertically()
{
   if (!mpCallbacks)
      return;
   auto pProject = LockProject();
   if (!pProject)
      return;
   auto &project = *pProject;
   auto &viewInfo = ViewInfo::Get(project);
   auto &tracks = TrackList::Get(project);

   // Only nonminimized audio tracks will be resized
   // Assume all channels of the track have the same minimization state
   auto range = tracks.Any<AudioTrack>()
      - [this](const Track *pTrack){
         return mpCallbacks->IsTrackMinimized(*pTrack); };
   auto count = static_cast<int>(range.sum(&Track::NChannels));
   if (count == 0)
      return;

   // Find total height to apportion
   auto height = viewInfo.GetHeight();
   height -= 28;
   
   // The height of minimized and non-audio tracks cannot be apportioned
   const auto fn = [this](const Track *pTrack){
      return mpCallbacks->GetTrackHeight(*pTrack);
   };
   height -= tracks.Any().sum(fn) - range.sum(fn);
   height /= count;
   height = std::max<int>(mpCallbacks->MinimumTrackHeight(), height);
   for (auto t : range)
      mpCallbacks->SetChannelHeights(*t, height);

   ScrollToTop();
}

void Viewport::ExpandAllTracks()
{
   if (!mpCallbacks)
      return;
   auto pProject = LockProject();
   if (!pProject)
      return;
   auto &project = *pProject;
   auto &tracks = TrackList::Get(project);
   for (auto t : tracks)
      mpCallbacks->SetMinimized(*t, false);
}

void Viewport::CollapseAllTracks()
{
   if (!mpCallbacks)
      return;
   auto pProject = LockProject();
   if (!pProject)
      return;
   auto &project = *pProject;
   auto &tracks = TrackList::Get(project);
   for (auto t : tracks)
      mpCallbacks->SetMinimized(*t, true);
}

void ProjectWindow::OnViewportMessage(const ViewportMessage &message)
{
}

static ToolManager::TopPanelHook::Scope scope {
[]( wxWindow &window ){
   auto pProjectWindow = dynamic_cast< ProjectWindow* >( &window );
   return pProjectWindow ? pProjectWindow->GetTopPanel() : nullptr;
} };
