/**********************************************************************

Audacity: A Digital Audio Editor

ProjectWindow.cpp

Paul Licameli split from AudacityProject.cpp

**********************************************************************/

#include "ProjectWindow.h"

#include "AdornedRulerPanel.h"
#include "AllThemeResources.h"
#include "AudioIO.h"
#include "Menus.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "TrackPanel.h"
#include "ViewInfo.h"
#include "WaveClip.h"
#include "WaveTrack.h"
#include "prefs/ThemePrefs.h"
#include "prefs/TracksPrefs.h"
#include "toolbars/ControlToolBar.h"
#include "toolbars/SelectionBar.h"
#include "toolbars/SpectralSelectionBar.h"
#include "toolbars/ToolManager.h"
#include "tracks/ui/Scrubbing.h"
#include "widgets/wxPanelWrapper.h"
#include "widgets/WindowAccessible.h"

#include <wx/display.h>
#include <wx/scrolbar.h>
#include <wx/sizer.h>

#ifdef __WXGTK__
#include "../images/AudacityLogoAlpha.xpm"
#endif

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

   int width = 940;
   int height = 674;

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

   gPrefs->Read(wxT("/Window/Maximized"), pMaximized, false);
   gPrefs->Read(wxT("/Window/Iconized"), pIconized, false);

   wxRect windowRect;
   gPrefs->Read(wxT("/Window/X"), &windowRect.x, defaultRect.x);
   gPrefs->Read(wxT("/Window/Y"), &windowRect.y, defaultRect.y);
   gPrefs->Read(wxT("/Window/Width"), &windowRect.width, defaultRect.width);
   gPrefs->Read(wxT("/Window/Height"), &windowRect.height, defaultRect.height);

   wxRect normalRect;
   gPrefs->Read(wxT("/Window/Normal_X"), &normalRect.x, defaultRect.x);
   gPrefs->Read(wxT("/Window/Normal_Y"), &normalRect.y, defaultRect.y);
   gPrefs->Read(wxT("/Window/Normal_Width"), &normalRect.width, defaultRect.width);
   gPrefs->Read(wxT("/Window/Normal_Height"), &normalRect.height, defaultRect.height);

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

AudacityProject::AttachedWindows::RegisteredFactory sProjectWindowKey{
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

ProjectWindow &ProjectWindow::Get( AudacityProject &project )
{
   return project.AttachedWindows::Get< ProjectWindow >( sProjectWindowKey );
}

const ProjectWindow &ProjectWindow::Get( const AudacityProject &project )
{
   return Get( const_cast< AudacityProject & >( project ) );
}

ProjectWindow *ProjectWindow::Find( AudacityProject *pProject )
{
   return pProject
      ? pProject->AttachedWindows::Find< ProjectWindow >( sProjectWindowKey )
      : nullptr;
}

const ProjectWindow *ProjectWindow::Find( const AudacityProject *pProject )
{
   return Find( const_cast< AudacityProject * >( pProject ) );
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
                                 const wxSize & size, AudacityProject &project)
   : wxFrame(parent, id, _TS("Audacity"), pos, size)
   , mProject{ project }
{
   project.SetFrame( this );

   mNextWindowID = NextID;

   // Two sub-windows need to be made before Init(),
   // so that this construcator can complete, and then TrackPanel and
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

   wxWindow    * pPage;

#ifdef EXPERIMENTAL_NOTEBOOK
   // We are using a notebook (tabbed panel), so we create the notebook and add pages.
   GuiFactory Factory;
   wxNotebook  * pNotebook;
   mMainPanel = Factory.AddPanel(
      this, wxPoint( left, top ), wxSize( width, height ) );
   pNotebook  = Factory.AddNotebook( mMainPanel );
   /* i18n-hint: This is an experimental feature where the main panel in
      Audacity is put on a notebook tab, and this is the name on that tab.
      Other tabs in that notebook may have instruments, patch panels etc.*/
   pPage = Factory.AddPage( pNotebook, _("Main Mix"));
#else
   // Not using a notebook, so we place the track panel inside another panel,
   // this keeps the notebook code and normal code consistant and also
   // paves the way for adding additional windows inside the track panel.
   mMainPanel = safenew wxPanelWrapper(this, -1,
      wxDefaultPosition,
      wxDefaultSize,
      wxNO_BORDER);
   mMainPanel->SetSizer( safenew wxBoxSizer(wxVERTICAL) );
   mMainPanel->SetLabel("Main Panel");// Not localised.
   pPage = mMainPanel;
   // Set the colour here to the track panel background to avoid
   // flicker when Audacity starts up.
   // However, that leads to areas next to the horizontal scroller
   // being painted in background colour and not scroller background
   // colour, so suppress this for now.
   //pPage->SetBackgroundColour( theTheme.Colour( clrDark ));
#endif
   pPage->SetLayoutDirection(wxLayout_LeftToRight);

#ifdef EXPERIMENTAL_DA2
   pPage->SetBackgroundColour(theTheme.Colour( clrMedium ));
#endif

   mMainPage = pPage;

   mPlaybackScroller = std::make_unique<PlaybackScroller>( &project );
}

void ProjectWindow::Init()
{
   auto &project = mProject;

#ifdef EXPERIMENTAL_DA2
   SetBackgroundColour(theTheme.Colour( clrMedium ));
#endif
   // Note that the first field of the status bar is a dummy, and its width is set
   // to zero latter in the code. This field is needed for wxWidgets 2.8.12 because
   // if you move to the menu bar, the first field of the menu bar is cleared, which
   // is undesirable behaviour.
   // In addition, the help strings of menu items are by default sent to the first
   // field. Currently there are no such help strings, but it they were introduced, then
   // there would need to be an event handler to send them to the appropriate field.
   auto statusBar = CreateStatusBar(4);
#if wxUSE_ACCESSIBILITY
   // so that name can be set on a standard control
   statusBar->SetAccessible(safenew WindowAccessible(statusBar));
#endif
   statusBar->SetName(wxT("status_line"));     // not localized

   auto &viewInfo = ViewInfo::Get( project );

   // LLL:  Read this!!!
   //
   // Until the time (and cpu) required to refresh the track panel is
   // reduced, leave the following window creations in the order specified.
   // This will place the refresh of the track panel last, allowing all
   // the others to get done quickly.
   //
   // Near as I can tell, this is only a problem under Windows.
   //


   //
   // Create the ToolDock
   //
   ToolManager::Get( project ).LayoutToolBars();

   //
   // Create the horizontal ruler
   //
   auto &ruler = AdornedRulerPanel::Get( project );

   //
   // Create the TrackPanel and the scrollbars
   //

   {
      auto ubs = std::make_unique<wxBoxSizer>(wxVERTICAL);
      ubs->Add( ToolManager::Get( project ).GetTopDock(), 0, wxEXPAND | wxALIGN_TOP );
      ubs->Add(&ruler, 0, wxEXPAND);
      mTopPanel->SetSizer(ubs.release());
   }

   const auto pPage = GetMainPage();

   wxBoxSizer *bs;
   {
      auto ubs = std::make_unique<wxBoxSizer>(wxVERTICAL);
      bs = ubs.get();
      bs->Add(mTopPanel, 0, wxEXPAND | wxALIGN_TOP);
      bs->Add(pPage, 1, wxEXPAND);
      bs->Add( ToolManager::Get( project ).GetBotDock(), 0, wxEXPAND );
      SetAutoLayout(true);
      SetSizer(ubs.release());
   }
   bs->Layout();

   auto &trackPanel = TrackPanel::Get( project );

   // LLL: When Audacity starts or becomes active after returning from
   //      another application, the first window that can accept focus
   //      will be given the focus even if we try to SetFocus().  By
   //      creating the scrollbars after the TrackPanel, we resolve
   //      several focus problems.
   mHsbar = safenew ScrollBar(pPage, HSBarID, wxSB_HORIZONTAL);
   mVsbar = safenew ScrollBar(pPage, VSBarID, wxSB_VERTICAL);
#if wxUSE_ACCESSIBILITY
   // so that name can be set on a standard control
   mHsbar->SetAccessible(safenew WindowAccessible(mHsbar));
   mVsbar->SetAccessible(safenew WindowAccessible(mVsbar));
#endif
   mHsbar->SetLayoutDirection(wxLayout_LeftToRight);
   mHsbar->SetName(_("Horizontal Scrollbar"));
   mVsbar->SetName(_("Vertical Scrollbar"));
   // LLL: When Audacity starts or becomes active after returning from
   //      another application, the first window that can accept focus
   //      will be given the focus even if we try to SetFocus().  By
   //      making the TrackPanel that first window, we resolve several
   //      keyboard focus problems.
   pPage->MoveBeforeInTabOrder(mTopPanel);

   bs = (wxBoxSizer *)pPage->GetSizer();

   {
      // Top horizontal grouping
      auto hs = std::make_unique<wxBoxSizer>(wxHORIZONTAL);

      // Track panel
      hs->Add(&trackPanel, 1, wxEXPAND | wxALIGN_LEFT | wxALIGN_TOP);

      {
         // Vertical grouping
         auto vs = std::make_unique<wxBoxSizer>(wxVERTICAL);

         // Vertical scroll bar
         vs->Add(mVsbar, 1, wxEXPAND | wxALIGN_TOP);
         hs->Add(vs.release(), 0, wxEXPAND | wxALIGN_TOP);
      }

      bs->Add(hs.release(), 1, wxEXPAND | wxALIGN_LEFT | wxALIGN_TOP);
   }

   {
      // Bottom horizontal grouping
      auto hs = std::make_unique<wxBoxSizer>(wxHORIZONTAL);

      // Bottom scrollbar
      hs->Add(trackPanel.GetLeftOffset() - 1, 0);
      hs->Add(mHsbar, 1, wxALIGN_BOTTOM);
      hs->Add(mVsbar->GetSize().GetWidth(), 0);
      bs->Add(hs.release(), 0, wxEXPAND | wxALIGN_LEFT);
   }

   // Lay it out
   pPage->SetAutoLayout(true);
   pPage->Layout();

#ifdef EXPERIMENTAL_NOTEBOOK
   AddPages(this, Factory, pNotebook);
#endif

   mMainPanel->Layout();

   wxASSERT( trackPanel.GetProject() == &project );

   // MM: Give track panel the focus to ensure keyboard commands work
   trackPanel.SetFocus();

   FixScrollbars();
   ruler.SetLeftOffset(trackPanel.GetLeftOffset());  // bevel on AdornedRuler

   //
   // Set the Icon
   //

   // loads either the XPM or the windows resource, depending on the platform
#if !defined(__WXMAC__) && !defined(__WXX11__)
   {
#if defined(__WXMSW__)
      wxIcon ic{ wxICON(AudacityLogo) };
#elif defined(__WXGTK__)
      wxIcon ic{wxICON(AudacityLogoAlpha)};
#else
      wxIcon ic{};
      ic.CopyFromBitmap(theTheme.Bitmap(bmpAudacityLogo48x48));
#endif
      SetIcon(ic);
   }
#endif
   mIconized = false;

   int widths[] = {
      0,
      ControlToolBar::Get( project ).WidthForStatusBar(statusBar),
      -1,
      150
   };
   statusBar->SetStatusWidths(4, widths);
   wxString msg = wxString::Format(_("Welcome to Audacity version %s"),
                                   AUDACITY_VERSION_STRING);
   statusBar->SetStatusText(msg, mainStatusBarField);
   ControlToolBar::Get( project ).UpdateStatusBar( &project );

   wxTheApp->Bind(EVT_THEME_CHANGE, &ProjectWindow::OnThemeChange, this);

#ifdef EXPERIMENTAL_DA2
   ClearBackground();// For wxGTK.
#endif
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
   auto &project = mProject;
   auto &trackPanel = TrackPanel::Get( project );
   SetBackgroundColour(theTheme.Colour( clrMedium ));
   ClearBackground();// For wxGTK.
   trackPanel.ApplyUpdatedTheme();
}

void ProjectWindow::RedrawProject(const bool bForceWaveTracks /*= false*/)
{
   auto &project = mProject ;
   auto &tracks = TrackList::Get( project );
   auto &trackPanel = TrackPanel::Get( project );
   FixScrollbars();
   if (bForceWaveTracks)
   {
      for ( auto pWaveTrack : tracks.Any< WaveTrack >() )
         for (const auto &clip: pWaveTrack->GetClips())
            clip->MarkChanged();
   }
   trackPanel.Refresh(false);
}

void ProjectWindow::RefreshCursor()
{
   auto &project = mProject;
   auto &trackPanel = TrackPanel::Get( project );
   trackPanel.HandleCursorForPresentMouseState();
}

void ProjectWindow::OnThemeChange(wxCommandEvent& evt)
{
   evt.Skip();
   auto &project = mProject;
   this->ApplyUpdatedTheme();
   auto &toolManager = ToolManager::Get( project );
   for( int ii = 0; ii < ToolBarCount; ++ii )
   {
      ToolBar *pToolBar = toolManager.GetToolBar(ii);
      if( pToolBar )
         pToolBar->ReCreateButtons();
   }
   AdornedRulerPanel::Get( project ).ReCreateButtons();
}

void ProjectWindow::FinishAutoScroll()
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

///
/// This method handles general left-scrolling, either for drag-scrolling
/// or when the scrollbar is clicked to the left of the thumb
///
void ProjectWindow::OnScrollLeft()
{
   auto &project = mProject;
   auto &viewInfo = ViewInfo::Get( project );
   wxInt64 pos = mHsbar->GetThumbPosition();
   // move at least one scroll increment
   pos -= wxMax((wxInt64)(sbarHjump * viewInfo.sbarScale), 1);
   pos = wxMax(pos, 0);
   viewInfo.sbarH -= sbarHjump;
   viewInfo.sbarH = std::max(viewInfo.sbarH,
      -(wxInt64) PixelWidthBeforeTime(0.0));


   if (pos != mHsbar->GetThumbPosition()) {
      mHsbar->SetThumbPosition((int)pos);
      FinishAutoScroll();
   }
}
///
/// This method handles general right-scrolling, either for drag-scrolling
/// or when the scrollbar is clicked to the right of the thumb
///

void ProjectWindow::OnScrollRight()
{
   auto &project = mProject;
   auto &viewInfo = ViewInfo::Get( project );
   wxInt64 pos = mHsbar->GetThumbPosition();
   // move at least one scroll increment
   // use wxInt64 for calculation to prevent temporary overflow
   pos += wxMax((wxInt64)(sbarHjump * viewInfo.sbarScale), 1);
   wxInt64 max = mHsbar->GetRange() - mHsbar->GetThumbSize();
   pos = wxMin(pos, max);
   viewInfo.sbarH += sbarHjump;
   viewInfo.sbarH = std::min(viewInfo.sbarH,
      viewInfo.sbarTotal
         - (wxInt64) PixelWidthBeforeTime(0.0) - viewInfo.sbarScreen);

   if (pos != mHsbar->GetThumbPosition()) {
      mHsbar->SetThumbPosition((int)pos);
      FinishAutoScroll();
   }
}


///
///  This handles the event when the left direction button on the scrollbar is depresssed
///
void ProjectWindow::OnScrollLeftButton(wxScrollEvent & /*event*/)
{
   auto &project = mProject;
   auto &viewInfo = ViewInfo::Get( project );
   wxInt64 pos = mHsbar->GetThumbPosition();
   // move at least one scroll increment
   pos -= wxMax((wxInt64)(sbarHjump * viewInfo.sbarScale), 1);
   pos = wxMax(pos, 0);
   viewInfo.sbarH -= sbarHjump;
   viewInfo.sbarH = std::max(viewInfo.sbarH,
      - (wxInt64) PixelWidthBeforeTime(0.0));

   if (pos != mHsbar->GetThumbPosition()) {
      mHsbar->SetThumbPosition((int)pos);
      DoScroll();
   }
}

///
///  This handles  the event when the right direction button on the scrollbar is depresssed
///
void ProjectWindow::OnScrollRightButton(wxScrollEvent & /*event*/)
{
   auto &project = mProject;
   auto &viewInfo = ViewInfo::Get( project );
   wxInt64 pos = mHsbar->GetThumbPosition();
   // move at least one scroll increment
   // use wxInt64 for calculation to prevent temporary overflow
   pos += wxMax((wxInt64)(sbarHjump * viewInfo.sbarScale), 1);
   wxInt64 max = mHsbar->GetRange() - mHsbar->GetThumbSize();
   pos = wxMin(pos, max);
   viewInfo.sbarH += sbarHjump;
   viewInfo.sbarH = std::min(viewInfo.sbarH,
      viewInfo.sbarTotal
         - (wxInt64) PixelWidthBeforeTime(0.0) - viewInfo.sbarScreen);

   if (pos != mHsbar->GetThumbPosition()) {
      mHsbar->SetThumbPosition((int)pos);
      DoScroll();
   }
}


bool ProjectWindow::MayScrollBeyondZero() const
{
   auto &project = mProject;
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

double ProjectWindow::ScrollingLowerBoundTime() const
{
   auto &project = mProject;
   auto &tracks = TrackList::Get( project );
   auto &trackPanel = TrackPanel::Get( project );
   auto &viewInfo = ViewInfo::Get( project );
   if (!MayScrollBeyondZero())
      return 0;
   const double screen = trackPanel.GetScreenEndTime() - viewInfo.h;
   return std::min(tracks.GetStartTime(), -screen);
}

// PRL: Bug1197: we seem to need to compute all in double, to avoid differing results on Mac
// That's why ViewInfo::TimeRangeToPixelWidth was defined, with some regret.
double ProjectWindow::PixelWidthBeforeTime(double scrollto) const
{
   auto &project = mProject;
   auto &viewInfo = ViewInfo::Get( project );
   const double lowerBound = ScrollingLowerBoundTime();
   return
      // Ignoring fisheye is correct here
      viewInfo.TimeRangeToPixelWidth(scrollto - lowerBound);
}

void ProjectWindow::SetHorizontalThumb(double scrollto)
{
   auto &project = mProject;
   auto &viewInfo = ViewInfo::Get( project );
   const auto unscaled = PixelWidthBeforeTime(scrollto);
   const int max = mHsbar->GetRange() - mHsbar->GetThumbSize();
   const int pos =
      std::min(max,
         std::max(0,
            (int)(floor(0.5 + unscaled * viewInfo.sbarScale))));
   mHsbar->SetThumbPosition(pos);
   viewInfo.sbarH = floor(0.5 + unscaled - PixelWidthBeforeTime(0.0));
   viewInfo.sbarH = std::max(viewInfo.sbarH,
      - (wxInt64) PixelWidthBeforeTime(0.0));
   viewInfo.sbarH = std::min(viewInfo.sbarH,
      viewInfo.sbarTotal
         - (wxInt64) PixelWidthBeforeTime(0.0) - viewInfo.sbarScreen);
}

//
// This method, like the other methods prefaced with TP, handles TrackPanel
// 'callback'.
//
void ProjectWindow::TP_ScrollWindow(double scrollto)
{
   SetHorizontalThumb(scrollto);

   // Call our Scroll method which updates our ViewInfo variables
   // to reflect the positions of the scrollbars
   DoScroll();
}

//
// Scroll vertically. This is called for example by the mouse wheel
// handler in Track Panel. A positive argument makes the window
// scroll down, while a negative argument scrolls up.
//
bool ProjectWindow::TP_ScrollUpDown(int delta)
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

void ProjectWindow::FixScrollbars()
{
   auto &project = mProject;
   auto &tracks = TrackList::Get( project );
   auto &trackPanel = TrackPanel::Get( project );
   auto &viewInfo = ViewInfo::Get( project );

   bool refresh = false;
   bool rescroll = false;

   int totalHeight = (tracks.GetHeight() + 32);

   int panelWidth, panelHeight;
   trackPanel.GetTracksUsableArea(&panelWidth, &panelHeight);

   // (From Debian...at least I think this is the change cooresponding
   // to this comment)
   //
   // (2.) GTK critical warning "IA__gtk_range_set_range: assertion
   // 'min < max' failed" because of negative numbers as result of window
   // size checking. Added a sanity check that straightens up the numbers
   // in edge cases.
   if (panelWidth < 0) {
      panelWidth = 0;
   }
   if (panelHeight < 0) {
      panelHeight = 0;
   }

   auto LastTime = std::numeric_limits<double>::lowest();
   for (const Track *track : tracks) {
      // Iterate over pending changed tracks if present.
      track = track->SubstitutePendingChangedTrack().get();
      LastTime = std::max( LastTime, track->GetEndTime() );
   }
   LastTime =
      std::max(LastTime, viewInfo.selectedRegion.t1());

   const double screen =
      trackPanel.GetScreenEndTime() - viewInfo.h;
   const double halfScreen = screen / 2.0;

   // If we can scroll beyond zero,
   // Add 1/2 of a screen of blank space to the end
   // and another 1/2 screen before the beginning
   // so that any point within the union of the selection and the track duration
   // may be scrolled to the midline.
   // May add even more to the end, so that you can always scroll the starting time to zero.
   const double lowerBound = ScrollingLowerBoundTime();
   const double additional = MayScrollBeyondZero()
      ? -lowerBound + std::max(halfScreen, screen - LastTime)
      : screen / 4.0;

   viewInfo.total = LastTime + additional;

   // Don't remove time from total that's still on the screen
   viewInfo.total = std::max(viewInfo.total, viewInfo.h + screen);

   if (viewInfo.h < lowerBound) {
      viewInfo.h = lowerBound;
      rescroll = true;
   }

   viewInfo.sbarTotal = (wxInt64) (viewInfo.GetTotalWidth());
   viewInfo.sbarScreen = (wxInt64)(panelWidth);
   viewInfo.sbarH = (wxInt64) (viewInfo.GetBeforeScreenWidth());

   // PRL:  Can someone else find a more elegant solution to bug 812, than
   // introducing this boolean member variable?
   // Setting mVSbar earlier, int HandlXMLTag, didn't succeed in restoring
   // the vertical scrollbar to its saved position.  So defer that till now.
   // mbInitializingScrollbar should be true only at the start of the life
   // of an AudacityProject reopened from disk.
   if (!mbInitializingScrollbar) {
      viewInfo.vpos = mVsbar->GetThumbPosition() * viewInfo.scrollStep;
   }
   mbInitializingScrollbar = false;

   if (viewInfo.vpos >= totalHeight)
      viewInfo.vpos = totalHeight - 1;
   if (viewInfo.vpos < 0)
      viewInfo.vpos = 0;

   bool oldhstate;
   bool oldvstate;
   bool newhstate =
      (trackPanel.GetScreenEndTime() - viewInfo.h) < viewInfo.total;
   bool newvstate = panelHeight < totalHeight;

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

   if (panelHeight >= totalHeight && viewInfo.vpos != 0) {
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
         scaledSbarScreen, TRUE);
   }

   // Vertical scrollbar
   mVsbar->SetScrollbar(viewInfo.vpos / viewInfo.scrollStep,
                        panelHeight / viewInfo.scrollStep,
                        totalHeight / viewInfo.scrollStep,
                        panelHeight / viewInfo.scrollStep, TRUE);

   if (refresh || (rescroll &&
       (trackPanel.GetScreenEndTime() - viewInfo.h) < viewInfo.total)) {
      trackPanel.Refresh(false);
   }

   MenuManager::Get( project ).UpdateMenus( project );

   if (oldhstate != newhstate || oldvstate != newvstate) {
      UpdateLayout();
   }

   wxWeakRef< TrackPanel > pPanel = &TrackPanel::Get( project );
   CallAfter( [pPanel]{
      if ( pPanel )
         pPanel->HandleCursorForPresentMouseState();
   } );
}

void ProjectWindow::UpdateLayout()
{
   auto &project = mProject;
   auto &trackPanel = TrackPanel::Get( project );
   auto &toolManager = ToolManager::Get( project );

   // 1. Layout panel, to get widths of the docks.
   Layout();
   // 2. Layout toolbars to pack the toolbars correctly in docks which 
   // are now the correct width.
   toolManager.LayoutToolBars();
   // 3. Layout panel, to resize docks, in particular reducing the height 
   // of any empty docks, or increasing the height of docks that need it.
   Layout();

   // Retrieve size of this projects window
   wxSize mainsz = GetSize();

   // Retrieve position of the track panel to use as the size of the top
   // third of the window
   wxPoint tppos = ClientToScreen(trackPanel.GetParent()->GetPosition());

   // Retrieve position of bottom dock to use as the size of the bottom
   // third of the window
   wxPoint sbpos = ClientToScreen(toolManager.GetBotDock()->GetPosition());

   // The "+ 50" is the minimum height of the TrackPanel
   SetSizeHints(250, (mainsz.y - sbpos.y) + tppos.y + 50, 20000, 20000);
}

void ProjectWindow::HandleResize()
{
   // Activate events can fire during window teardown, so just
   // ignore them.
   if (mIsDeleting) {
      return;
   }

   FixScrollbars();

   UpdateLayout();
}


bool ProjectWindow::IsIconized() const
{
   return mIconized;
}

void ProjectWindow::OnIconize(wxIconizeEvent &event)
{
   //JKC: On Iconizing we get called twice.  Don't know
   // why but it does no harm.
   // Should we be returning true/false rather than
   // void return?  I don't know.
   mIconized = event.IsIconized();

   unsigned int i;

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

void ProjectWindow::OnScroll(wxScrollEvent & WXUNUSED(event))
{
   auto &project = mProject;
   auto &viewInfo = ViewInfo::Get( project );
   const wxInt64 offset = PixelWidthBeforeTime(0.0);
   viewInfo.sbarH =
      (wxInt64)(mHsbar->GetThumbPosition() / viewInfo.sbarScale) - offset;
   DoScroll();
}

void ProjectWindow::DoScroll()
{
   auto &project = mProject;
   auto &trackPanel = TrackPanel::Get( project );
   auto &viewInfo = ViewInfo::Get( project );
   const double lowerBound = ScrollingLowerBoundTime();

   int width;
   trackPanel.GetTracksUsableArea(&width, NULL);
   viewInfo.SetBeforeScreenWidth(viewInfo.sbarH, width, lowerBound);


   if (MayScrollBeyondZero()) {
      enum { SCROLL_PIXEL_TOLERANCE = 10 };
      if (std::abs(viewInfo.TimeToPosition(0.0, 0
                                   )) < SCROLL_PIXEL_TOLERANCE) {
         // Snap the scrollbar to 0
         viewInfo.h = 0;
         SetHorizontalThumb(0.0);
      }
   }

   viewInfo.vpos = mVsbar->GetThumbPosition() * viewInfo.scrollStep;

   //mchinen: do not always set this project to be the active one.
   //a project may autoscroll while playing in the background
   //I think this is okay since OnMouseEvent has one of these.
   //SetActiveProject(this);

   if (!mAutoScrolling) {
      trackPanel.Refresh(false);
   }

   wxWeakRef< TrackPanel > pPanel = &TrackPanel::Get( project );
   CallAfter( [pPanel]{
      if ( pPanel )
         pPanel->HandleCursorForPresentMouseState();
   } );
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
   auto &project = mProject;
   auto &commandManager = CommandManager::Get( project );
   bool handled = commandManager.HandleMenuID(
      event.GetId(), MenuManager::Get( project ).GetUpdateFlags( project ),
      NoFlagsSpecified);

   if (handled)
      event.Skip(false);
   else{
      event.ResumePropagation( 999 );
      event.Skip(true);
   }
}

void ProjectWindow::OnUpdateUI(wxUpdateUIEvent & WXUNUSED(event))
{
   auto &project = mProject;
   MenuManager::Get( project ).UpdateMenus( project );
}

void ProjectWindow::MacShowUndockedToolbars(bool show)
{
   (void)show;//compiler food
#ifdef __WXMAC__
   // Find all the floating toolbars, and show or hide them
   const auto &children = GetChildren();
   for(const auto &child : children) {
      if (auto frame = dynamic_cast<ToolFrame*>(child)) {
         if (!show)
            frame->Hide();
         else if (frame->GetBar() &&
                  frame->GetBar()->IsVisible())
            frame->Show();
      }
   }
#endif
}

void ProjectWindow::OnActivate(wxActivateEvent & event)
{
   // Activate events can fire during window teardown, so just
   // ignore them.
   if (IsBeingDeleted()) {
      return;
   }

   auto &project = mProject;
   
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
   if (!mActive) {
#ifdef __WXMAC__
      if (IsIconized())
         MacShowUndockedToolbars(false);
#endif
   }
   else {
      auto &toolManager = ToolManager::Get( project );
      SetActiveProject( &project );
      if ( ! toolManager.RestoreFocus() )
         TrackPanel::Get( project ).SetFocus();

#ifdef __WXMAC__
      MacShowUndockedToolbars(true);
#endif
   }
   event.Skip();
}

bool ProjectWindow::IsActive()
{
   return mActive;
}

void ProjectWindow::OnMouseEvent(wxMouseEvent & event)
{
   auto &project = mProject;
   if (event.ButtonDown())
      SetActiveProject( &project );
}

void ProjectWindow::ZoomAfterImport(Track *pTrack)
{
   auto &project = mProject;
   auto &trackPanel = TrackPanel::Get( project );

   ViewActions::DoZoomFit( project );

   trackPanel.SetFocus();
   RedrawProject();
   if (!pTrack)
      pTrack = trackPanel.GetFirstSelectedTrack();
   trackPanel.EnsureVisible(pTrack);
}

// Utility function called by other zoom methods
void ProjectWindow::Zoom(double level)
{
   auto &project = mProject;
   auto &viewInfo = ViewInfo::Get( project );
   viewInfo.SetZoom(level);
   FixScrollbars();
   // See if we can center the selection on screen, and have it actually fit.
   // tOnLeft is the amount of time we would need before the selection left edge to center it.
   float t0 = viewInfo.selectedRegion.t0();
   float t1 = viewInfo.selectedRegion.t1();
   float tAvailable = TrackPanel::Get( project ).GetScreenEndTime() - viewInfo.h;
   float tOnLeft = (tAvailable - t0 + t1)/2.0;
   // Bug 1292 (Enh) is effectively a request to do this scrolling of  the selection into view.
   // If tOnLeft is positive, then we have room for the selection, so scroll to it.
   if( tOnLeft >=0 )
      TP_ScrollWindow( t0-tOnLeft);
}

// Utility function called by other zoom methods
void ProjectWindow::ZoomBy(double multiplier)
{
   auto &project = mProject;
   auto &viewInfo = ViewInfo::Get( project );
   viewInfo.ZoomBy(multiplier);
   FixScrollbars();
}

///////////////////////////////////////////////////////////////////
// This method 'rewinds' the track, by setting the cursor to 0 and
// scrolling the window to fit 0 on the left side of it
// (maintaining  current zoom).
// If shift is held down, it will extend the left edge of the
// selection to 0 (holding right edge constant), otherwise it will
// move both left and right edge of selection to 0
///////////////////////////////////////////////////////////////////
void ProjectWindow::Rewind(bool shift)
{
   auto &project = mProject;
   auto &viewInfo = ViewInfo::Get( project );
   viewInfo.selectedRegion.setT0(0, false);
   if (!shift)
      viewInfo.selectedRegion.setT1(0);

   TP_ScrollWindow(0);
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
   auto &project = mProject;
   auto &tracks = TrackList::Get( project );
   auto &trackPanel = TrackPanel::Get( project );
   auto &viewInfo = ViewInfo::Get( project );
   double len = tracks.GetEndTime();

   viewInfo.selectedRegion.setT1(len, false);
   if (!shift)
      viewInfo.selectedRegion.setT0(len);

   // Make sure the end of the track is visible
   trackPanel.ScrollIntoView(len);
   trackPanel.Refresh(false);
}

void ProjectWindow::TP_DisplaySelection()
{
   auto &project = mProject;
   auto &ruler = AdornedRulerPanel::Get(project);
   auto &viewInfo = ViewInfo::Get( project );
   const auto &selectedRegion = viewInfo.selectedRegion;
   double audioTime;
   auto &playRegion = ViewInfo::Get( project ).playRegion;

   if (!gAudioIO->IsBusy() && playRegion.Locked())
      ruler.SetPlayRegion( selectedRegion.t0(), selectedRegion.t1() );
   else
      // Cause ruler redraw anyway, because we may be zooming or scrolling
      ruler.Refresh();

   if (gAudioIO->IsBusy())
      audioTime = gAudioIO->GetStreamTime();
   else
      audioTime = playRegion.GetStart();

   SelectionBar::Get( project ).SetTimes(selectedRegion.t0(),
                               selectedRegion.t1(), audioTime);
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   SpectralSelectionBar::Get( project ).SetFrequencies(
      selectedRegion.f0(), selectedRegion.f1());
#endif
}


// TrackPanel callback method
void ProjectWindow::TP_ScrollLeft()
{
   OnScrollLeft();
}

// TrackPanel callback method
void ProjectWindow::TP_ScrollRight()
{
   OnScrollRight();
}

// TrackPanel callback method
void ProjectWindow::TP_RedrawScrollbars()
{
   FixScrollbars();
}

void ProjectWindow::TP_HandleResize()
{
   HandleResize();
}

ProjectWindow::PlaybackScroller::PlaybackScroller(AudacityProject *project)
: mProject(project)
{
   ViewInfo::Get( *mProject ).Bind(EVT_TRACK_PANEL_TIMER,
      &PlaybackScroller::OnTimer,
      this);
}

void ProjectWindow::PlaybackScroller::OnTimer(wxCommandEvent &event)
{
   // Let other listeners get the notification
   event.Skip();

   auto cleanup = finally([&]{
      // Propagate the message to other listeners bound to this
      this->ProcessEvent( event );
   });

   if(!ProjectAudioIO::Get( *mProject ).IsAudioActive())
      return;
   else if (mMode == Mode::Refresh) {
      // PRL:  see comments in Scrubbing.cpp for why this is sometimes needed.
      // These unnecessary refreshes cause wheel rotation events to be delivered more uniformly
      // to the application, so scrub speed control is smoother.
      // (So I see at least with OS 10.10 and wxWidgets 3.0.2.)
      // Is there another way to ensure that than by refreshing?
      auto &trackPanel = TrackPanel::Get( *mProject );
      trackPanel.Refresh(false);
   }
   else if (mMode != Mode::Off) {
      // Pan the view, so that we put the play indicator at some fixed
      // fraction of the window width.

      auto &viewInfo = ViewInfo::Get( *mProject );
      auto &trackPanel = TrackPanel::Get( *mProject );
      const int posX = viewInfo.TimeToPosition(viewInfo.mRecentStreamTime);
      int width;
      trackPanel.GetTracksUsableArea(&width, NULL);
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
      viewInfo.h =
         viewInfo.OffsetTimeByPixels(viewInfo.h, deltaX, true);
      if (!ProjectWindow::Get( *mProject ).MayScrollBeyondZero())
         // Can't scroll too far left
         viewInfo.h = std::max(0.0, viewInfo.h);
      trackPanel.Refresh(false);
   }
}

void ProjectWindow::ZoomInByFactor( double ZoomFactor )
{
   auto &project = mProject;
   auto &trackPanel = TrackPanel::Get( project );
   auto &viewInfo = ViewInfo::Get( project );

   // LLL: Handling positioning differently when audio is
   // actively playing.  Don't do this if paused.
   if (gAudioIO->IsStreamActive(
         ProjectAudioIO::Get( project ).GetAudioIOToken()) &&
       !gAudioIO->IsPaused()){
      ZoomBy(ZoomFactor);
      trackPanel.ScrollIntoView(gAudioIO->GetStreamTime());
      trackPanel.Refresh(false);
      return;
   }

   // DMM: Here's my attempt to get logical zooming behavior
   // when there's a selection that's currently at least
   // partially on-screen

   const double endTime = trackPanel.GetScreenEndTime();
   const double duration = endTime - viewInfo.h;

   bool selectionIsOnscreen =
      (viewInfo.selectedRegion.t0() < endTime) &&
      (viewInfo.selectedRegion.t1() >= viewInfo.h);

   bool selectionFillsScreen =
      (viewInfo.selectedRegion.t0() < viewInfo.h) &&
      (viewInfo.selectedRegion.t1() > endTime);

   if (selectionIsOnscreen && !selectionFillsScreen) {
      // Start with the center of the selection
      double selCenter = (viewInfo.selectedRegion.t0() +
                          viewInfo.selectedRegion.t1()) / 2;

      // If the selection center is off-screen, pick the
      // center of the part that is on-screen.
      if (selCenter < viewInfo.h)
         selCenter = viewInfo.h +
                     (viewInfo.selectedRegion.t1() - viewInfo.h) / 2;
      if (selCenter > endTime)
         selCenter = endTime -
            (endTime - viewInfo.selectedRegion.t0()) / 2;

      // Zoom in
      ZoomBy(ZoomFactor);
      const double newDuration =
         trackPanel.GetScreenEndTime() - viewInfo.h;

      // Recenter on selCenter
      TP_ScrollWindow(selCenter - newDuration / 2);
      return;
   }


   double origLeft = viewInfo.h;
   double origWidth = duration;
   ZoomBy(ZoomFactor);

   const double newDuration =
      trackPanel.GetScreenEndTime() - viewInfo.h;
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

   TP_ScrollWindow(newh);
}

void ProjectWindow::ZoomOutByFactor( double ZoomFactor )
{
   auto &project = mProject;
   auto &trackPanel = TrackPanel::Get( project );
   auto &viewInfo = ViewInfo::Get( project );

   //Zoom() may change these, so record original values:
   const double origLeft = viewInfo.h;
   const double origWidth = trackPanel.GetScreenEndTime() - origLeft;

   ZoomBy(ZoomFactor);
   const double newWidth = trackPanel.GetScreenEndTime() - viewInfo.h;

   const double newh = origLeft + (origWidth - newWidth) / 2;
   // newh = (newh > 0) ? newh : 0;
   TP_ScrollWindow(newh);
}
