/**********************************************************************

  Audacity: A Digital Audio Editor

  Project.cpp

  Dominic Mazzoni
  Vaughan Johnson

*******************************************************************//**

\file Project.cpp
\brief Implements AudacityProject

*//****************************************************************//**

\class AudacityProject
\brief AudacityProject provides the main window, with tools and
tracks contained within it.

  In Audacity, the main window you work in is called a project.
  AudacityProjects can contain an arbitrary number of tracks of many
  different types, but if a project contains just one or two
  tracks then it can be saved in standard formats like WAV or AIFF.
  This window is the one that contains the menu bar (except on
  the Mac).

\attention The menu functions for AudacityProject, those for creating
the menu bars and acting on clicks, are found in file Menus.cpp

*//****************************************************************//**

\class ViewInfo
\brief ViewInfo is used mainly to hold the zooming, selection and
scroll information.  It also has some status flags.

*//*******************************************************************/

#include "Audacity.h" // for USE_* macros
#include "Project.h"

#include "ProjectAudioIO.h"
#include "ProjectFileIORegistry.h"
#include "ProjectSettings.h"

#include "Experimental.h"

#include <stdio.h>
#include <iostream>
#include <wx/wxprec.h>

#include <wx/setup.h> // for wxUSE_* macros

#include <wx/wxcrtvararg.h>
#include <wx/apptrait.h>

#include <wx/defs.h>
#include <wx/app.h>
#include <wx/dc.h>
#include <wx/dcmemory.h>
#include <wx/event.h>
#include <wx/ffile.h>
#include <wx/filedlg.h>
#include <wx/filefn.h>
#include <wx/filename.h>
#include <wx/intl.h>
#include <wx/log.h>
#include <wx/menu.h>
#include <wx/notebook.h>
#include <wx/progdlg.h>
#include <wx/scrolbar.h>
#include <wx/sizer.h>
#include <wx/statusbr.h>
#include <wx/string.h>
#include <wx/textfile.h>
#include <wx/timer.h>
#include <wx/display.h>

#if defined(__WXMAC__)
#if !wxCHECK_VERSION(3, 0, 0)
#include <CoreServices/CoreServices.h>
#include <wx/mac/private.h>
#endif
#endif

#include "AdornedRulerPanel.h"
#include "widgets/FileHistory.h"
#include "AutoRecovery.h"
#include "AColor.h"
#include "AudioIO.h"
#include "Dependencies.h"
#include "Diags.h"
#include "InconsistencyException.h"
#include "KeyboardCapture.h"
#include "Menus.h"
#include "Mix.h"
#include "NoteTrack.h"
#include "Prefs.h"
#include "SelectionState.h"
#include "Tags.h"
#include "TimeTrack.h"
#include "TrackPanel.h"
#include "WaveClip.h"
#include "ViewInfo.h"
#include "DirManager.h"
#include "prefs/PrefsDialog.h"
#include "widgets/LinkingHtmlWindow.h"
#include "widgets/AudacityMessageBox.h"
#include "widgets/ErrorDialog.h"
#include "widgets/Warning.h"
#include "xml/XMLFileReader.h"
#include "FileNames.h"
#include "ondemand/ODManager.h"
#include "ondemand/ODComputeSummaryTask.h"

#include "AllThemeResources.h"

#include "FileDialog.h"

#include "UndoManager.h"

#include "toolbars/ToolManager.h"
#include "toolbars/ControlToolBar.h"
#include "toolbars/MixerToolBar.h"
#include "toolbars/SelectionBar.h"
#include "toolbars/SpectralSelectionBar.h"

#include "tracks/ui/Scrubbing.h"

#include "prefs/ThemePrefs.h"
#include "export/Export.h"

#include "prefs/TracksPrefs.h"

#include "../images/AudacityLogoAlpha.xpm"

#if wxUSE_ACCESSIBILITY
#include "widgets/WindowAccessible.h"
#endif

#include "widgets/NumericTextCtrl.h"

wxDEFINE_EVENT(EVT_PROJECT_STATUS_UPDATE, wxCommandEvent);

size_t AllProjects::size() const
{
   return gAudacityProjects.size();
}

auto AllProjects::begin() const -> const_iterator
{
   return gAudacityProjects.begin();
}

auto AllProjects::end() const -> const_iterator
{
   return gAudacityProjects.end();
}

auto AllProjects::rbegin() const -> const_reverse_iterator
{
   return gAudacityProjects.rbegin();
}

auto AllProjects::rend() const -> const_reverse_iterator
{
   return gAudacityProjects.rend();
}

auto AllProjects::Remove( AudacityProject &project ) -> value_type
{
   ODLocker locker{ &Mutex() };
   auto start = begin(), finish = end(), iter = std::find_if(
      start, finish,
      [&]( const value_type &ptr ){ return ptr.get() == &project; }
   );
   if (iter == finish)
      return nullptr;
   auto result = *iter;
   gAudacityProjects.erase( iter );
   return result;
}

void AllProjects::Add( const value_type &pProject )
{
   ODLocker locker{ &Mutex() };
   gAudacityProjects.push_back( pProject );
}

bool AllProjects::sbClosing = false;

bool AllProjects::Close( bool force )
{
   ValueRestorer<bool> cleanup{ sbClosing, true };
   while (AllProjects{}.size())
   {
      // Closing the project has global side-effect
      // of deletion from gAudacityProjects
      if ( force )
      {
         GetProjectFrame( **AllProjects{}.begin() ).Close(true);
      }
      else
      {
         if (! GetProjectFrame( **AllProjects{}.begin() ).Close())
            return false;
      }
   }
   return true;
}

ODLock &AllProjects::Mutex()
{
   static ODLock theMutex;
   return theMutex;
};

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

int AudacityProject::mProjectCounter=0;// global counter.


//
// This small template class resembles a try-finally block
//
// It sets var to val_entry in the constructor and
// var to val_exit in the destructor.
//
template <typename T>
class VarSetter
{
public:
   VarSetter(T* var, T val_entry, T val_exit)
   {
      mVar = var;
      mValExit = val_exit;
      *var = val_entry;
   }

   ~VarSetter()
   {
      *mVar = mValExit;
   }
private:
   T* mVar;
   T mValExit;
};

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

/* Define Global Variables */
//This is a pointer to the currently-active project.
static AudacityProject *gActiveProject;
//This array holds onto all of the projects currently open
AllProjects::Container AllProjects::gAudacityProjects;

AUDACITY_DLL_API AudacityProject *GetActiveProject()
{
   return gActiveProject;
}

void SetActiveProject(AudacityProject * project)
{
   if ( gActiveProject != project ) {
      gActiveProject = project;
      KeyboardCapture::Capture( nullptr );
   }
   wxTheApp->SetTopWindow( FindProjectFrame( project ) );
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

static wxString CreateUniqueName()
{
   static int count = 0;
   return wxDateTime::Now().Format(wxT("%Y-%m-%d %H-%M-%S")) +
          wxString::Format(wxT(" N-%i"), ++count);
}

enum {
   FirstID = 1000,

   // Window controls

   HSBarID,
   VSBarID,

   NextID,
};

int AudacityProject::NextWindowID()
{
   return mNextWindowID++;
}


BEGIN_EVENT_TABLE(AudacityProject, wxFrame)
   EVT_MENU(wxID_ANY, AudacityProject::OnMenu)
   EVT_MOUSE_EVENTS(AudacityProject::OnMouseEvent)
   EVT_CLOSE(AudacityProject::OnCloseWindow)
   EVT_SIZE(AudacityProject::OnSize)
   EVT_SHOW(AudacityProject::OnShow)
   EVT_ICONIZE(AudacityProject::OnIconize)
   EVT_MOVE(AudacityProject::OnMove)
   EVT_ACTIVATE(AudacityProject::OnActivate)
   EVT_COMMAND_SCROLL_LINEUP(HSBarID, AudacityProject::OnScrollLeftButton)
   EVT_COMMAND_SCROLL_LINEDOWN(HSBarID, AudacityProject::OnScrollRightButton)
   EVT_COMMAND_SCROLL(HSBarID, AudacityProject::OnScroll)
   EVT_COMMAND_SCROLL(VSBarID, AudacityProject::OnScroll)
   // Fires for menu with ID #1...first menu defined
   EVT_UPDATE_UI(1, AudacityProject::OnUpdateUI)
   EVT_ICONIZE(AudacityProject::OnIconize)
   EVT_COMMAND(wxID_ANY, EVT_TOOLBAR_UPDATED, AudacityProject::OnToolBarUpdate)
   //mchinen:multithreaded calls - may not be threadsafe with CommandEvent: may have to change.
END_EVENT_TABLE()

AudacityProject::AudacityProject(wxWindow * parent, wxWindowID id,
                                 const wxPoint & pos,
                                 const wxSize & size)
   : wxFrame(parent, id, _TS("Audacity"), pos, size),
     mbLoadedFromAup( false )
{
   auto &project = *this;
   auto &window = project;

   mNextWindowID = NextID;

#ifdef EXPERIMENTAL_DA2
   SetBackgroundColour(theTheme.Colour( clrMedium ));
#endif
   // Note that the first field of the status bar is a dummy, and it's width is set
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
   mProjectNo = mProjectCounter++; // Bug 322

   mLastSavedTracks.reset();

   auto &viewInfo = ViewInfo::Get( *this );

   mLockPlayRegion = false;

   // LLL:  Read this!!!
   //
   // Until the time (and cpu) required to refresh the track panel is
   // reduced, leave the following window creations in the order specified.
   // This will place the refresh of the track panel last, allowing all
   // the others to get done quickly.
   //
   // Near as I can tell, this is only a problem under Windows.
   //


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

   {
      auto ubs = std::make_unique<wxBoxSizer>(wxVERTICAL);
      ubs->Add( ToolManager::Get( project ).GetTopDock(), 0, wxEXPAND | wxALIGN_TOP );
      ubs->Add(&ruler, 0, wxEXPAND);
      mTopPanel->SetSizer(ubs.release());
   }

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
 
   mPlaybackScroller = std::make_unique<PlaybackScroller>(this);

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

   wxASSERT( trackPanel.GetProject()==this);

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
   ControlToolBar::Get( project ).UpdateStatusBar(this);

   wxTheApp->Bind(EVT_THEME_CHANGE, &AudacityProject::OnThemeChange, this);

#ifdef EXPERIMENTAL_DA2
   ClearBackground();// For wxGTK.
#endif

   AttachedObjects::BuildAll();
   // But not for the attached windows.  They get built only on demand, such as
   // from menu items.
}

AudacityProject::~AudacityProject()
{
   // Tool manager gives us capture sometimes
   if(HasCapture())
      ReleaseMouse();
}

void AudacityProject::ApplyUpdatedTheme()
{
   auto &project = *this;
   auto &trackPanel = TrackPanel::Get( project );
   SetBackgroundColour(theTheme.Colour( clrMedium ));
   ClearBackground();// For wxGTK.
   trackPanel.ApplyUpdatedTheme();
}

void AudacityProject::UpdatePrefs()
{
   SetProjectTitle();
}

void AudacityProject::RedrawProject(const bool bForceWaveTracks /*= false*/)
{
   auto &project = *this;
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

void AudacityProject::RefreshCursor()
{
   auto &project = *this;
   auto &trackPanel = TrackPanel::Get( project );
   trackPanel.HandleCursorForPresentMouseState();
}

void AudacityProject::OnThemeChange(wxCommandEvent& evt)
{
   evt.Skip();
   auto &project = *this;
   ProjectWindow::Get( project ).ApplyUpdatedTheme();
   auto &toolManager = ToolManager::Get( project );
   for( int ii = 0; ii < ToolBarCount; ++ii )
   {
      ToolBar *pToolBar = toolManager.GetToolBar(ii);
      if( pToolBar )
         pToolBar->ReCreateButtons();
   }
   AdornedRulerPanel::Get( project ).ReCreateButtons();
}

wxString AudacityProject::GetProjectName() const
{
   wxString name = wxFileNameFromPath(mFileName);

   // Chop off the extension
   size_t len = name.length();
   if (len > 4 && name.Mid(len - 4) == wxT(".aup"))
      name = name.Mid(0, len - 4);

   return name;
}

// Pass a number in to show project number, or -1 not to.
void AudacityProject::SetProjectTitle( int number)
{
   wxString name = GetProjectName();

   // If we are showing project numbers, then we also explicitly show "<untitled>" if there
   // is none.
   if( number >= 0 ){
      /* i18n-hint: The %02i is the project number, the %s is the project name.*/
      name = wxString::Format( _TS("[Project %02i] Audacity \"%s\""), number+1 ,
         name.empty() ? "<untitled>" : (const char *)name );
   }
   // If we are not showing numbers, then <untitled> shows as 'Audacity'.
   else if( name.empty() )
   {
      mbLoadedFromAup = false;
      name = _TS("Audacity");
   }

   if (mIsRecovered)
   {
      name += wxT(" ");
      /* i18n-hint: E.g this is recovered audio that had been lost.*/
      name += _("(Recovered)");
   }

   SetTitle( name );
   SetName(name);       // to make the nvda screen reader read the correct title
}

void AudacityProject::FinishAutoScroll()
{
   // Set a flag so we don't have to generate two update events
   mAutoScrolling = true;

   // Call our Scroll method which updates our ViewInfo variables
   // to reflect the positions of the scrollbars
   DoScroll();

   mAutoScrolling = false;
}


///
/// This method handles general left-scrolling, either for drag-scrolling
/// or when the scrollbar is clicked to the left of the thumb
///
void AudacityProject::OnScrollLeft()
{
   auto &project = *this;
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

void AudacityProject::OnScrollRight()
{
   auto &project = *this;
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
void AudacityProject::OnScrollLeftButton(wxScrollEvent & /*event*/)
{
   auto &project = *this;
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
void AudacityProject::OnScrollRightButton(wxScrollEvent & /*event*/)
{
   auto &project = *this;
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


bool AudacityProject::MayScrollBeyondZero() const
{
   auto &project = *this;
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

double AudacityProject::ScrollingLowerBoundTime() const
{
   auto &project = *this;
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
double AudacityProject::PixelWidthBeforeTime(double scrollto) const
{
   auto &project = *this;
   auto &viewInfo = ViewInfo::Get( project );
   const double lowerBound = ScrollingLowerBoundTime();
   return
      // Ignoring fisheye is correct here
      viewInfo.TimeRangeToPixelWidth(scrollto - lowerBound);
}

void AudacityProject::SetHorizontalThumb(double scrollto)
{
   auto &project = *this;
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
void AudacityProject::TP_ScrollWindow(double scrollto)
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
bool AudacityProject::TP_ScrollUpDown(int delta)
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

void AudacityProject::FixScrollbars()
{
   auto &project = *this;
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

void AudacityProject::UpdateLayout()
{
   auto &project = *this;
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

void AudacityProject::HandleResize()
{
   // Activate events can fire during window teardown, so just
   // ignore them.
   if (mIsDeleting) {
      return;
   }

   auto &project = *this;

   FixScrollbars();

   UpdateLayout();
}

void AudacityProject::OnIconize(wxIconizeEvent &event)
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

void AudacityProject::OnMove(wxMoveEvent & event)
{
   if (!this->IsMaximized() && !this->IsIconized())
      SetNormalizedWindowState(this->GetRect());
   event.Skip();
}

void AudacityProject::OnSize(wxSizeEvent & event)
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

void AudacityProject::OnShow(wxShowEvent & event)
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
void AudacityProject::OnToolBarUpdate(wxCommandEvent & event)
{
   HandleResize();

   event.Skip(false);             /* No need to propagate any further */
}

void AudacityProject::OnScroll(wxScrollEvent & WXUNUSED(event))
{
   auto &project = *this;
   auto &viewInfo = ViewInfo::Get( project );
   const wxInt64 offset = PixelWidthBeforeTime(0.0);
   viewInfo.sbarH =
      (wxInt64)(mHsbar->GetThumbPosition() / viewInfo.sbarScale) - offset;
   DoScroll();
}

void AudacityProject::DoScroll()
{
   auto &project = *this;
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

void AudacityProject::OnMenu(wxCommandEvent & event)
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
   auto &project = *this;
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

void AudacityProject::OnUpdateUI(wxUpdateUIEvent & WXUNUSED(event))
{
   auto &project = *this;
   MenuManager::Get( project ).UpdateMenus( project );
}

void AudacityProject::MacShowUndockedToolbars(bool show)
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

void AudacityProject::OnActivate(wxActivateEvent & event)
{
   // Activate events can fire during window teardown, so just
   // ignore them.
   if (IsBeingDeleted()) {
      return;
   }

   auto &project = *this;
   
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
      SetActiveProject(this);
      if ( ! toolManager.RestoreFocus() )
         TrackPanel::Get( project ).SetFocus();

#ifdef __WXMAC__
      MacShowUndockedToolbars(true);
#endif
   }
   event.Skip();
}

bool AudacityProject::IsActive()
{
   return mActive;
}

void AudacityProject::OnMouseEvent(wxMouseEvent & event)
{
   if (event.ButtonDown())
      SetActiveProject(this);
}

static void RefreshAllTitles(bool bShowProjectNumbers )
{
   for ( auto pProject : AllProjects{} ) {
      if ( !GetProjectFrame( *pProject ).IsIconized() ) {
         pProject->SetProjectTitle(
            bShowProjectNumbers ? pProject->GetProjectNumber() : -1 );
      }
   }
}

TitleRestorer::TitleRestorer(AudacityProject * p )
{
   auto &window = GetProjectFrame( *p );
   if( window.IsIconized() )
      window.Restore();
   window.Raise(); // May help identifying the window on Mac

   // Construct this projects name and number.
   sProjName = p->GetProjectName();
   if (sProjName.empty()){
      sProjName = _("<untitled>");
      UnnamedCount = std::count_if(
         AllProjects{}.begin(), AllProjects{}.end(),
         []( const AllProjects::value_type &ptr ){
            return ptr->GetProjectName().empty();
         }
      );
      if( UnnamedCount > 1 ){
         sProjNumber.Printf( "[Project %02i] ", p->GetProjectNumber()+1 );
         RefreshAllTitles( true );
      } 
   } else {
      UnnamedCount = 0;
   }
}

TitleRestorer::~TitleRestorer() {
   if( UnnamedCount > 1 )
      RefreshAllTitles( false );
}

// Most of this string was duplicated 3 places. Made the warning consistent in this global.
// The %s is to be filled with the version string.
// PRL:  Do not statically allocate a string in _() !
static wxString gsLegacyFileWarning() { return
_("This file was saved by Audacity version %s. The format has changed. \
\n\nAudacity can try to open and save this file, but saving it in this \
\nversion will then prevent any 1.2 or earlier version opening it. \
\n\nAudacity might corrupt the file in opening it, so you should \
back it up first. \
\n\nOpen this file now?");
}

bool AudacityProject::WarnOfLegacyFile( )
{
   wxString msg;
   msg.Printf(gsLegacyFileWarning(), _("1.0 or earlier"));

   // Stop icon, and choose 'NO' by default.
   int action =
      AudacityMessageBox(msg,
                   _("Warning - Opening Old Project File"),
                   wxYES_NO | wxICON_STOP | wxNO_DEFAULT | wxCENTRE,
                   this);
   return (action != wxNO);
}

auto AudacityProject::ReadProjectFile( const FilePath &fileName )
  -> ReadProjectResults
{
   auto &project = *this;

   mFileName = fileName;
   mbLoadedFromAup = true;

   mRecoveryAutoSaveDataDir = wxT("");
   mIsRecovered = false;

   SetProjectTitle();

   const wxString autoSaveExt = wxT("autosave");
   if ( wxFileNameWrapper{ mFileName }.GetExt() == autoSaveExt )
   {
      AutoSaveFile asf;
      if (!asf.Decode(fileName))
      {
         auto message = AutoSaveFile::FailureMessage( fileName );
         AudacityMessageBox(
            message,
            _("Error decoding file"),
            wxOK | wxCENTRE, this);
         // Important: Prevent deleting any temporary files!
         DirManager::SetDontDeleteTempFiles();
         return { true };
      }
   }

   ///
   /// Parse project file
   ///

   XMLFileReader xmlFile;

   // 'Lossless copy' projects have dependencies. We need to always copy-in
   // these dependencies when converting to a normal project.
   wxString oldAction =
      gPrefs->Read(wxT("/FileFormats/CopyOrEditUncompressedData"), wxT("copy"));
   bool oldAsk =
      gPrefs->ReadBool(wxT("/Warnings/CopyOrEditUncompressedDataAsk"), true);
   if (oldAction != wxT("copy"))
      gPrefs->Write(wxT("/FileFormats/CopyOrEditUncompressedData"), wxT("copy"));
   if (oldAsk)
      gPrefs->Write(wxT("/Warnings/CopyOrEditUncompressedDataAsk"), (long) false);
   gPrefs->Flush();

   auto cleanup = finally( [&] {
      // and restore old settings if necessary.
      if (oldAction != wxT("copy"))
         gPrefs->Write(wxT("/FileFormats/CopyOrEditUncompressedData"), oldAction);
      if (oldAsk)
         gPrefs->Write(wxT("/Warnings/CopyOrEditUncompressedDataAsk"), (long) true);
      gPrefs->Flush();
   } );

   bool bParseSuccess = xmlFile.Parse(this, fileName);
   
   bool err = false;

   if (bParseSuccess) {
      // By making a duplicate set of pointers to the existing blocks
      // on disk, we add one to their reference count, guaranteeing
      // that their reference counts will never reach zero and thus
      // the version saved on disk will be preserved until the
      // user selects Save().

      mLastSavedTracks = TrackList::Create();

      auto &tracks = TrackList::Get( project );
      for (auto t : tracks.Any()) {
         if (t->GetErrorOpening())
         {
            wxLogWarning(
               wxT("Track %s had error reading clip values from project file."),
               t->GetName());
            err = true;
         }

         err = ( !t->LinkConsistencyCheck() ) || err;

         mLastSavedTracks->Add(t->Duplicate());
      }
   }

   return { false, bParseSuccess, err, xmlFile.GetErrorStr() };
}

void AudacityProject::EnqueueODTasks()
{
   //check the ODManager to see if we should add the tracks to the ODManager.
   //this flag would have been set in the HandleXML calls from above, if there were
   //OD***Blocks.
   if(ODManager::HasLoadedODFlag())
   {
      auto &project = *this;
      auto &tracks = TrackList::Get( project );

      std::vector<std::unique_ptr<ODTask>> newTasks;
      //std::vector<ODDecodeTask*> decodeTasks;
      unsigned int createdODTasks=0;
      for (auto wt : tracks.Any<WaveTrack>()) {
         //check the track for blocks that need decoding.
         //There may be more than one type e.g. FLAC/FFMPEG/lame
         unsigned int odFlags = wt->GetODFlags();

         //add the track to the already created tasks that correspond to the od flags in the wavetrack.
         for(unsigned int i=0;i<newTasks.size();i++) {
            if(newTasks[i]->GetODType() & odFlags)
               newTasks[i]->AddWaveTrack(wt);
         }

         //create whatever NEW tasks we need to.
         //we want at most one instance of each class for the project
         while((odFlags|createdODTasks) != createdODTasks)
         {
            std::unique_ptr<ODTask> newTask;
#ifdef EXPERIMENTAL_OD_FLAC
            if(!(createdODTasks&ODTask::eODFLAC) && (odFlags & ODTask::eODFLAC)) {
               newTask = std::make_unique<ODDecodeFlacTask>();
               createdODTasks = createdODTasks | ODTask::eODFLAC;
            }
            else
#endif
            if(!(createdODTasks&ODTask::eODPCMSummary) && (odFlags & ODTask::eODPCMSummary)) {
               newTask = std::make_unique<ODComputeSummaryTask>();
               createdODTasks = createdODTasks | ODTask::eODPCMSummary;
            }
            else {
               wxPrintf("unrecognized OD Flag in block file.\n");
               //TODO:ODTODO: display to user.  This can happen when we build audacity on a system that doesnt have libFLAC
               break;
            }
            if(newTask)
            {
               newTask->AddWaveTrack(wt);
               newTasks.push_back(std::move(newTask));
            }
         }
      }
      for(unsigned int i=0;i<newTasks.size();i++)
         ODManager::Instance()->AddNewTask(std::move(newTasks[i]));
   }
}

bool AudacityProject::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   auto &project = *this;
   auto &window = ProjectWindow::Get( project );
   auto &viewInfo = ViewInfo::Get( project );
   auto &dirManager = DirManager::Get( project );
   auto &settings = ProjectSettings::Get( project );
   bool bFileVersionFound = false;
   wxString fileVersion = _("<unrecognized version -- possibly corrupt project file>");
   wxString audacityVersion = _("<unrecognized version -- possibly corrupt project file>");
   int requiredTags = 0;
   long longVpos = 0;

   // loop through attrs, which is a null-terminated list of
   // attribute-value pairs
   while(*attrs) {
      const wxChar *attr = *attrs++;
      const wxChar *value = *attrs++;

      if (!value || !XMLValueChecker::IsGoodString(value))
         break;

      if (viewInfo.ReadXMLAttribute(attr, value)) {
         // We need to save vpos now and restore it below
         longVpos = std::max(longVpos, long(viewInfo.vpos));
         continue;
      }

      if (!wxStrcmp(attr, wxT("datadir")))
      {
         //
         // This is an auto-saved version whose data is in another directory
         //
         // Note: This attribute must currently be written and parsed before
         //       any other attributes
         //
         if ((value[0] != 0) && XMLValueChecker::IsGoodPathString(value))
         {
            // Remember that this is a recovered project
            mIsRecovered = true;
            mRecoveryAutoSaveDataDir = value;
         }
      }

      else if (!wxStrcmp(attr, wxT("version")))
      {
         fileVersion = value;
         bFileVersionFound = true;
         requiredTags++;
      }

      else if (!wxStrcmp(attr, wxT("audacityversion"))) {
         audacityVersion = value;
         requiredTags++;
      }

      else if (!wxStrcmp(attr, wxT("projname"))) {
         FilePath projName;
         FilePath projPath;

         if (mIsRecovered) {
            // Fake the filename as if we had opened the original file
            // (which was lost by the crash) rather than the one in the
            // auto save folder
            wxFileName realFileDir;
            realFileDir.AssignDir(mRecoveryAutoSaveDataDir);
            realFileDir.RemoveLastDir();

            wxString realFileName = value;
            if (realFileName.length() >= 5 &&
                realFileName.Right(5) == wxT("_data"))
            {
               realFileName = realFileName.Left(realFileName.length() - 5);
            }

            if (realFileName.empty())
            {
               // A previously unsaved project has been recovered, so fake
               // an unsaved project. The data files just stay in the temp
               // directory
               dirManager.SetLocalTempDir(mRecoveryAutoSaveDataDir);
               mFileName = wxT("");
               projName = wxT("");
               projPath = wxT("");
            } else
            {
               realFileName += wxT(".aup");
               projPath = realFileDir.GetFullPath();
               mFileName = wxFileName(projPath, realFileName).GetFullPath();
               mbLoadedFromAup = true;
               projName = value;
            }

            SetProjectTitle();
         } else {
            projName = value;
            projPath = wxPathOnly(mFileName);
         }

         if (!projName.empty())
         {
            // First try to load the data files based on the _data dir given in the .aup file
            // If this fails then try to use the filename of the .aup as the base directory
            // This is because unzipped projects e.g. those that get transfered between mac-pc
            // have encoding issues and end up expanding the wrong filenames for certain
            // international characters (such as capital 'A' with an umlaut.)
            if (!dirManager.SetProject(projPath, projName, false))
            {
               projName = GetProjectName() + wxT("_data");
               if (!dirManager.SetProject(projPath, projName, false)) {
                  AudacityMessageBox(wxString::Format(_("Couldn't find the project data folder: \"%s\""),
                                             projName),
                                             _("Error Opening Project"),
                                             wxOK | wxCENTRE, this);
                  return false;
               }
            }
         }

         requiredTags++;
      }

      else if (!wxStrcmp(attr, wxT("rate"))) {
         double rate;
         Internat::CompatibleToDouble(value, &rate);
         settings.SetRate( rate );
         SelectionBar::Get( project ).SetRate( rate );
      }

      else if (!wxStrcmp(attr, wxT("snapto"))) {
         settings.SetSnapTo(wxString(value) == wxT("on") ? true : false);
      }

      else if (!wxStrcmp(attr, wxT("selectionformat")))
         settings.SetSelectionFormat(
            NumericConverter::LookupFormat( NumericConverter::TIME, value) );

      else if (!wxStrcmp(attr, wxT("frequencyformat")))
         settings.SetFrequencySelectionFormatName(
            NumericConverter::LookupFormat( NumericConverter::FREQUENCY, value ) );

      else if (!wxStrcmp(attr, wxT("bandwidthformat")))
         settings.SetBandwidthSelectionFormatName(
            NumericConverter::LookupFormat( NumericConverter::BANDWIDTH, value ) );
   } // while

   if (longVpos != 0) {
      // PRL: It seems this must happen after SetSnapTo
       viewInfo.vpos = longVpos;
       window.mbInitializingScrollbar = true;
   }

   // Specifically detect newer versions of Audacity
   // WARNING: This will need review/revision if we ever have a version string
   // such as 1.5.10, i.e. with 2 digit numbers.
   // We're able to do a shortcut and use string comparison because we know
   // that does not happen.
   // TODO: Um.  We actually have released 0.98 and 1.3.14 so the comment
   // above is inaccurate.

   if (!bFileVersionFound ||
         (fileVersion.length() != 5) || // expecting '1.1.0', for example
         // JKC: I commentted out next line.  IsGoodInt is not for
         // checking dotted numbers.
         //!XMLValueChecker::IsGoodInt(fileVersion) ||
         (fileVersion > wxT(AUDACITY_FILE_FORMAT_VERSION)))
   {
      wxString msg;
      /* i18n-hint: %s will be replaced by the version number.*/
      msg.Printf(_("This file was saved using Audacity %s.\nYou are using Audacity %s. You may need to upgrade to a newer version to open this file."),
                 audacityVersion,
                 AUDACITY_VERSION_STRING);
      AudacityMessageBox(msg,
                   _("Can't open project file"),
                   wxOK | wxICON_EXCLAMATION | wxCENTRE, this);
      return false;
   }

   // NOTE: It looks as if there was some confusion about fileversion and audacityversion.
   // fileversion NOT being increased when file formats changed, so unfortunately we're
   // using audacityversion to rescue the situation.

   // KLUDGE: guess the true 'fileversion' by stripping away any '-beta-Rc' stuff on
   // audacityVersion.
   // It's fairly safe to do this as it has already been established that the
   // puported file version was five chars long.
   fileVersion = audacityVersion.Mid(0,5);

   bool bIsOld = fileVersion < wxT(AUDACITY_FILE_FORMAT_VERSION);
   bool bIsVeryOld = fileVersion < wxT("1.1.9" );
   // Very old file versions could even have the file version starting
   // with text: 'AudacityProject Version 0.95'
   // Atoi return zero in this case.
   bIsVeryOld |= wxAtoi( fileVersion )==0;
   // Specifically detect older versions of Audacity
   if ( bIsOld | bIsVeryOld ) {
      wxString msg;
      msg.Printf(gsLegacyFileWarning(), audacityVersion);

      int icon_choice = wxICON_EXCLAMATION;
      if( bIsVeryOld )
         // Stop icon, and choose 'NO' by default.
         icon_choice = wxICON_STOP | wxNO_DEFAULT;
      int action =
         AudacityMessageBox(msg,
                      _("Warning - Opening Old Project File"),
                      wxYES_NO | icon_choice | wxCENTRE,
                      this);
      if (action == wxNO)
         return false;
   }

   if (wxStrcmp(tag, wxT("audacityproject")) &&
       wxStrcmp(tag, wxT("project"))) {
      // If the tag name is not one of these two (the NEW name is
      // "project" with an Audacity namespace, but we don't detect
      // the namespace yet), then we don't know what the error is
      return false;
   }

   if (requiredTags < 3)
      return false;

   // All other tests passed, so we succeed
   return true;
}

XMLTagHandler *AudacityProject::HandleXMLChild(const wxChar *tag)
{
   auto fn = ProjectFileIORegistry::Lookup( tag );
   if (fn)
      return fn( *this );

   return nullptr;
}

void AudacityProject::WriteXMLHeader(XMLWriter &xmlFile) const
{
   xmlFile.Write(wxT("<?xml "));
   xmlFile.Write(wxT("version=\"1.0\" "));
   xmlFile.Write(wxT("standalone=\"no\" "));
   xmlFile.Write(wxT("?>\n"));

   xmlFile.Write(wxT("<!DOCTYPE "));
   xmlFile.Write(wxT("project "));
   xmlFile.Write(wxT("PUBLIC "));
   xmlFile.Write(wxT("\"-//audacityproject-1.3.0//DTD//EN\" "));
   xmlFile.Write(wxT("\"http://audacity.sourceforge.net/xml/audacityproject-1.3.0.dtd\" "));
   xmlFile.Write(wxT(">\n"));
}

void AudacityProject::WriteXML(XMLWriter &xmlFile, bool bWantSaveCopy)
// may throw
{
   auto &proj = *this;
   auto &tracks = TrackList::Get( proj );
   auto &viewInfo = ViewInfo::Get( proj );
   auto &dirManager = DirManager::Get( proj );
   auto &tags = Tags::Get( proj );
   const auto &settings = ProjectSettings::Get( proj );

   //TIMER_START( "AudacityProject::WriteXML", xml_writer_timer );
   // Warning: This block of code is duplicated in Save, for now...
   wxString project = mFileName;
   if (project.length() > 4 && project.Mid(project.length() - 4) == wxT(".aup"))
      project = project.Mid(0, project.length() - 4);
   wxString projName = wxFileNameFromPath(project) + wxT("_data");
   // End Warning -DMM

   xmlFile.StartTag(wxT("project"));
   xmlFile.WriteAttr(wxT("xmlns"), wxT("http://audacity.sourceforge.net/xml/"));

   if (mAutoSaving)
   {
      //
      // When auto-saving, remember full path to data files directory
      //
      // Note: This attribute must currently be written and parsed before
      //       all other attributes
      //
      xmlFile.WriteAttr(wxT("datadir"), dirManager.GetDataFilesDir());

      // Note that the code at the start assumes that if mFileName has a value
      // then the file has been saved.  This is not neccessarily true when
      // autosaving as it gets set by AddImportedTracks (presumably as a proposal).
      // I don't think that mDirManager.projName gets set without a save so check that.
      if( !IsProjectSaved() )
         projName = wxT("_data");
   }

   xmlFile.WriteAttr(wxT("projname"), projName);
   xmlFile.WriteAttr(wxT("version"), wxT(AUDACITY_FILE_FORMAT_VERSION));
   xmlFile.WriteAttr(wxT("audacityversion"), AUDACITY_VERSION_STRING);

   viewInfo.WriteXMLAttributes(xmlFile);
   xmlFile.WriteAttr(wxT("rate"), settings.GetRate());
   xmlFile.WriteAttr(wxT("snapto"), settings.GetSnapTo() ? wxT("on") : wxT("off"));
   xmlFile.WriteAttr(wxT("selectionformat"),
                     settings.GetSelectionFormat().Internal());
   xmlFile.WriteAttr(wxT("frequencyformat"),
                     settings.GetFrequencySelectionFormatName().Internal());
   xmlFile.WriteAttr(wxT("bandwidthformat"),
                     settings.GetBandwidthSelectionFormatName().Internal());

   tags.WriteXML(xmlFile);

   unsigned int ndx = 0;
   tracks.Any().Visit(
      [&](WaveTrack *pWaveTrack) {
         if (bWantSaveCopy) {
            if (!pWaveTrack->IsLeader())
               return;

            //vvv This should probably be a method, WaveTrack::WriteCompressedTrackXML().
            xmlFile.StartTag(wxT("import"));
            xmlFile.WriteAttr(wxT("filename"), mStrOtherNamesArray[ndx]); // Assumes mTracks order hasn't changed!

            // Don't store "channel" and "linked" tags because the importer can figure that out,
            // e.g., from stereo Ogg files.
            //    xmlFile.WriteAttr(wxT("channel"), t->GetChannel());
            //    xmlFile.WriteAttr(wxT("linked"), t->GetLinked());

            const auto offset =
               TrackList::Channels( pWaveTrack ).min( &WaveTrack::GetOffset );
            xmlFile.WriteAttr(wxT("offset"), offset, 8);
            xmlFile.WriteAttr(wxT("mute"), pWaveTrack->GetMute());
            xmlFile.WriteAttr(wxT("solo"), pWaveTrack->GetSolo());
            xmlFile.WriteAttr(wxT("height"), pWaveTrack->GetActualHeight());
            xmlFile.WriteAttr(wxT("minimized"), pWaveTrack->GetMinimized());

            // Don't store "rate" tag because the importer can figure that out.
            //    xmlFile.WriteAttr(wxT("rate"), pWaveTrack->GetRate());
            xmlFile.WriteAttr(wxT("gain"), (double)pWaveTrack->GetGain());
            xmlFile.WriteAttr(wxT("pan"), (double)pWaveTrack->GetPan());
            xmlFile.EndTag(wxT("import"));

            ndx++;
         }
         else {
            pWaveTrack->SetAutoSaveIdent(mAutoSaving ? ++ndx : 0);
            pWaveTrack->WriteXML(xmlFile);
         }
      },
      [&](Track *t) {
         t->WriteXML(xmlFile);
      }
   );

   if (!mAutoSaving)
   {
      // Only write closing bracket when not auto-saving, since we may add
      // recording log data to the end of the file later
      xmlFile.EndTag(wxT("project"));
   }
   //TIMER_STOP( xml_writer_timer );

}

#if 0
// I added this to "fix" bug #334.  At that time, we were on wxWidgets 2.8.12 and
// there was a window between the closing of the "Save" progress dialog and the
// end of the actual save where the user was able to close the project window and
// recursively enter the Save code (where they could inadvertently cause the issue
// described in #334).
//
// When we converted to wx3, this "disabler" caused focus problems when returning
// to the project after the save (bug #1172) because the focus and activate events
// weren't being dispatched and the focus would get lost.
//
// After some testing, it looks like the window described above no longer exists,
// so I've disabled the disabler.  However, I'm leaving it here in case we run
// into the problem in the future.  (even though it can't be used as-is)
class ProjectDisabler
{
public:
   ProjectDisabler(wxWindow *w)
   :  mWindow(w)
   {
      mWindow->GetEventHandler()->SetEvtHandlerEnabled(false);
   }
   ~ProjectDisabler()
   {
      mWindow->GetEventHandler()->SetEvtHandlerEnabled(true);
   }
private:
   wxWindow *mWindow;
};
#endif

bool AudacityProject::Save()
{
   // Prompt for file name?
   bool bPromptingRequired = !IsProjectSaved();

   if (bPromptingRequired)
      return SaveAs();

   return DoSave(false, false);
}


// Assumes AudacityProject::mFileName has been set to the desired path.
bool AudacityProject::DoSave (const bool fromSaveAs,
                              const bool bWantSaveCopy,
                              const bool bLossless /*= false*/)
{
   // See explanation above
   // ProjectDisabler disabler(this);
   auto &proj = *this;
   auto &window = GetProjectFrame( proj );
   auto &dirManager = DirManager::Get( proj );
   const auto &settings = ProjectSettings::Get( proj );

   wxASSERT_MSG(!bWantSaveCopy || fromSaveAs, "Copy Project SHOULD only be availabele from SaveAs");

   // Some confirmation dialogs
   if (!bWantSaveCopy)
   {
      auto &project = *this;
      auto &tracks = TrackList::Get( project );
      if ( ! tracks.Any() )
      {
         if ( UndoManager::Get( proj ).UnsavedChanges()
         && settings.EmptyCanBeDirty()) {
            int result = AudacityMessageBox(_("Your project is now empty.\nIf saved, the project will have no tracks.\n\nTo save any previously open tracks:\nClick 'No', Edit > Undo until all tracks\nare open, then File > Save Project.\n\nSave anyway?"),
                                      _("Warning - Empty Project"),
                                      wxYES_NO | wxICON_QUESTION, this);
            if (result == wxNO)
               return false;
         }
      }

      // If the user has recently imported dependencies, show
      // a dialog where the user can see audio files that are
      // aliased by this project.  The user may make the project
      // self-contained during this dialog, it modifies the project!
      if (mImportedDependencies)
      {
         bool bSuccess = ShowDependencyDialogIfNeeded(this, true);
         if (!bSuccess)
            return false;
         mImportedDependencies = false; // do not show again
      }
   }
   // End of confirmations

   //
   // Always save a backup of the original project file
   //

   wxString safetyFileName;
   if (wxFileExists(mFileName)) {

#ifdef __WXGTK__
      safetyFileName = mFileName + wxT("~");
#else
      safetyFileName = mFileName + wxT(".bak");
#endif

      if (wxFileExists(safetyFileName))
         wxRemoveFile(safetyFileName);

      if ( !wxRenameFile(mFileName, safetyFileName) ) {
         AudacityMessageBox(
            wxString::Format(
               _("Could not create safety file: %s"), safetyFileName ),
            _("Error"), wxICON_STOP, this);
         return false;
      }
   }

   bool success = true;
   FilePath project, projName, projPath;

   auto cleanup = finally( [&] {
      if (!safetyFileName.empty()) {
         if (wxFileExists(mFileName))
            wxRemove(mFileName);
         wxRename(safetyFileName, mFileName);
      }

      // mStrOtherNamesArray is a temporary array of file names, used only when
      // saving compressed
      if (!success) {
         AudacityMessageBox(wxString::Format(_("Could not save project. Perhaps %s \nis not writable or the disk is full."),
                                       project),
                      _("Error Saving Project"),
                      wxICON_ERROR, this);

         // Make the export of tracks succeed all-or-none.
         auto dir = project + wxT("_data");
         for ( auto &name : mStrOtherNamesArray )
            wxRemoveFile( dir + wxFileName::GetPathSeparator() + name);
         // This has effect only if the folder is empty
         wxFileName::Rmdir( dir );
      }
      // Success or no, we can forget the names
      mStrOtherNamesArray.clear();
   } );

   if (fromSaveAs) {
      // This block of code is duplicated in WriteXML, for now...
      project = mFileName;
      if (project.length() > 4 && project.Mid(project.length() - 4) == wxT(".aup"))
         project = project.Mid(0, project.length() - 4);
      projName = wxFileNameFromPath(project) + wxT("_data");
      projPath = wxPathOnly(project);

      if( !wxDir::Exists( projPath ) ){
         AudacityMessageBox(wxString::Format(
            _("Could not save project. Path not found. Try creating \ndirectory \"%s\" before saving project with this name."),
            projPath),
                      _("Error Saving Project"),
                      wxICON_ERROR, this);
         return (success = false);
      }

      if (bWantSaveCopy)
      {
         // Do this before saving the .aup, because we accumulate
         // mStrOtherNamesArray which affects the contents of the .aup

         // This populates the array mStrOtherNamesArray
         success = this->SaveCopyWaveTracks(project, bLossless);
      }

      if (!success)
         return false;
   }

   // Write the .aup now, before DirManager::SetProject,
   // because it's easier to clean up the effects of successful write of .aup
   // followed by failed SetProject, than the other way about.
   // And that cleanup is done by the destructor of saveFile, if PostCommit() is
   // not done.
   // (SetProject, when it fails, cleans itself up.)
   XMLFileWriter saveFile{ mFileName, _("Error Saving Project") };
   success = GuardedCall< bool >( [&] {
         WriteXMLHeader(saveFile);
         WriteXML(saveFile, bWantSaveCopy);
         // Flushes files, forcing space exhaustion errors before trying
         // SetProject():
         saveFile.PreCommit();
         return true;
      },
      MakeSimpleGuard(false),
      // Suppress the usual error dialog for failed write,
      // which is redundant here:
      [](void*){}
   );

   if (!success)
      return false;

   {
   std::vector<std::unique_ptr<WaveTrack::Locker>> lockers;
   Maybe<DirManager::ProjectSetter> pSetter;
   bool moving = true;

   if (fromSaveAs && !bWantSaveCopy) {
      // We are about to move files from the current directory to
      // the NEW directory.  We need to make sure files that belonged
      // to the last saved project don't get erased, so we "lock" them, so that
      // ProjectSetter's constructor copies instead of moves the files.
      // (Otherwise the NEW project would be fine, but the old one would
      // be empty of all of its files.)

      if (mLastSavedTracks) {
         moving = false;
         lockers.reserve(mLastSavedTracks->size());
         for (auto wt : mLastSavedTracks->Any<WaveTrack>())
            lockers.push_back(
               std::make_unique<WaveTrack::Locker>(wt));
      }

      // This renames the project directory, and moves or copies
      // all of our block files over.
      pSetter.create( dirManager, projPath, projName, true, moving );

      if (!pSetter->Ok()){
         success = false;
         return false;
      }
   }

   // Commit the writing of the .aup only now, after we know that the _data
   // folder also saved with no problems.
   // It is very unlikely that errors will happen:
   // only renaming and removing of files, not writes that might exhaust space.
   // So DO give a second dialog in case the unusual happens.
   success = success && GuardedCall< bool >( [&] {
         saveFile.PostCommit();
         return true;
   } );

   if (!success)
      return false;

   // SAVE HAS SUCCEEDED -- following are further no-fail commit operations.

   if (pSetter)
      pSetter->Commit();
   }

   if ( !bWantSaveCopy )
   {
      // Now that we have saved the file, we can DELETE the auto-saved version
      DeleteCurrentAutoSaveFile();

      if (mIsRecovered)
      {
         // This was a recovered file, that is, we have just overwritten the
         // old, crashed .aup file. There may still be orphaned blockfiles in
         // this directory left over from the crash, so we DELETE them now
         dirManager.RemoveOrphanBlockfiles();

         // Before we saved this, this was a recovered project, but now it is
         // a regular project, so remember this.
         mIsRecovered = false;
         mRecoveryAutoSaveDataDir = wxT("");
         SetProjectTitle();
      }
      else if (fromSaveAs)
      {
         // On save as, always remove orphaned blockfiles that may be left over
         // because the user is trying to overwrite another project
         dirManager.RemoveOrphanBlockfiles();
      }

      if (mLastSavedTracks)
         mLastSavedTracks->Clear();
      mLastSavedTracks = TrackList::Create();

      auto &tracks = TrackList::Get( proj );
      for ( auto t : tracks.Any() ) {
         mLastSavedTracks->Add(t->Duplicate());

         //only after the xml has been saved we can mark it saved.
         //thus is because the OD blockfiles change on  background thread while this is going on.
         //         if(const auto wt = track_cast<WaveTrack*>(dupT))
         //            wt->MarkSaved();
      }

      UndoManager::Get( proj ).StateSaved();
   }

   // If we get here, saving the project was successful, so we can DELETE
   // the .bak file (because it now does not fit our block files anymore
   // anyway).
   if (!safetyFileName.empty())
      wxRemoveFile(safetyFileName),
      // cancel the cleanup:
      safetyFileName = wxT("");

   window.GetStatusBar()->SetStatusText(wxString::Format(_("Saved %s"),
                                              mFileName), mainStatusBarField);

   return true;
}


bool AudacityProject::SaveCopyWaveTracks(const FilePath & strProjectPathName,
                                         const bool bLossless /*= false*/)
{
   auto &project = *this;
   auto &tracks = TrackList::Get( project );
   auto &trackFactory = TrackFactory::Get( project );

   wxString extension, fileFormat;
#ifdef USE_LIBVORBIS
   if (bLossless) {
      extension = wxT("wav");
      fileFormat = wxT("WAVFLT");
   } else {
      extension = wxT("ogg");
      fileFormat = wxT("OGG");
   }
#else
   extension = wxT("wav");
   fileFormat = wxT("WAVFLT");
#endif
   // Some of this is similar to code in ExportMultiple::ExportMultipleByTrack
   // but that code is really tied into the dialogs.

      // Copy the tracks because we're going to do some state changes before exporting.
      unsigned int numWaveTracks = 0;

   auto ppSavedTrackList = TrackList::Create();
   auto &pSavedTrackList = *ppSavedTrackList;

   auto trackRange = tracks.Any< WaveTrack >();
   for (auto pWaveTrack : trackRange)
   {
      numWaveTracks++;
      pSavedTrackList.Add( trackFactory.DuplicateWaveTrack( *pWaveTrack ) );
   }
   auto cleanup = finally( [&] {
      // Restore the saved track states and clean up.
      auto savedTrackRange = pSavedTrackList.Any<const WaveTrack>();
      auto ppSavedTrack = savedTrackRange.begin();
      for (auto ppTrack = trackRange.begin();

           *ppTrack && *ppSavedTrack;

           ++ppTrack, ++ppSavedTrack)
      {
         auto pWaveTrack = *ppTrack;
         auto pSavedWaveTrack = *ppSavedTrack;
         pWaveTrack->SetSelected(pSavedWaveTrack->GetSelected());
         pWaveTrack->SetMute(pSavedWaveTrack->GetMute());
         pWaveTrack->SetSolo(pSavedWaveTrack->GetSolo());

         pWaveTrack->SetGain(pSavedWaveTrack->GetGain());
         pWaveTrack->SetPan(pSavedWaveTrack->GetPan());
      }
   } );

   if (numWaveTracks == 0)
      // Nothing to save compressed => success. Delete the copies and go.
      return true;

   // Okay, now some bold state-faking to default values.
   for (auto pWaveTrack : trackRange)
   {
      pWaveTrack->SetSelected(false);
      pWaveTrack->SetMute(false);
      pWaveTrack->SetSolo(false);

      pWaveTrack->SetGain(1.0);
      pWaveTrack->SetPan(0.0);
   }

   FilePath strDataDirPathName = strProjectPathName + wxT("_data");
   if (!wxFileName::DirExists(strDataDirPathName) &&
         !wxFileName::Mkdir(strDataDirPathName, 0777, wxPATH_MKDIR_FULL))
      return false;
   strDataDirPathName += wxFileName::GetPathSeparator();

   // Export all WaveTracks to OGG.
   bool bSuccess = true;

   // This accumulates the names of the track files, to be written as
   // dependencies in the .aup file
   mStrOtherNamesArray.clear();

   Exporter theExporter;
   wxFileName uniqueTrackFileName;
   for (auto pTrack : (trackRange + &Track::IsLeader))
   {
      SelectionStateChanger changer{ SelectionState::Get( project ), tracks };
      auto channels = TrackList::Channels(pTrack);

      for (auto channel : channels)
         channel->SetSelected(true);
      uniqueTrackFileName = wxFileName(strDataDirPathName, pTrack->GetName(), extension);
      FileNames::MakeNameUnique(mStrOtherNamesArray, uniqueTrackFileName);
      const auto startTime = channels.min( &Track::GetStartTime );
      const auto endTime = channels.max( &Track::GetEndTime );
      bSuccess =
         theExporter.Process(this, channels.size(),
                              fileFormat, uniqueTrackFileName.GetFullPath(), true,
                              startTime, endTime);

      if (!bSuccess)
         // If only some exports succeed, the cleanup is not done here
         // but trusted to the caller
         break;
   }

   return bSuccess;
}

void AudacityProject::ZoomAfterImport(Track *pTrack)
{
   auto &project = *this;
   auto &trackPanel = TrackPanel::Get( project );

   ViewActions::DoZoomFit(*this);

   trackPanel.SetFocus();
   RedrawProject();
   if (!pTrack)
      pTrack = trackPanel.GetFirstSelectedTrack();
   trackPanel.EnsureVisible(pTrack);
}

bool AudacityProject::SaveAs(const wxString & newFileName, bool bWantSaveCopy /*= false*/, bool addToHistory /*= true*/)
{
   // This version of SaveAs is invoked only from scripting and does not
   // prompt for a file name
   auto oldFileName = mFileName;

   bool bOwnsNewAupName = mbLoadedFromAup && (mFileName==newFileName);
   //check to see if the NEW project file already exists.
   //We should only overwrite it if this project already has the same name, where the user
   //simply chose to use the save as command although the save command would have the effect.
   if( !bOwnsNewAupName && wxFileExists(newFileName)) {
      AudacityMessageDialog m(
         NULL,
         _("The project was not saved because the file name provided would overwrite another project.\nPlease try again and select an original name."),
         _("Error Saving Project"),
         wxOK|wxICON_ERROR);
      m.ShowModal();
      return false;
   }

   mFileName = newFileName;
   bool success = false;
   auto cleanup = finally( [&] {
      if (!success || bWantSaveCopy)
         // Restore file name on error
         mFileName = oldFileName;
   } );

   //Don't change the title, unless we succeed.
   //SetProjectTitle();

   success = DoSave(!bOwnsNewAupName || bWantSaveCopy, bWantSaveCopy);

   if (success && addToHistory) {
      FileHistory::Global().AddFileToHistory(mFileName);
   }
   if (!success || bWantSaveCopy) // bWantSaveCopy doesn't actually change current project.
   {
   }
   else {
      mbLoadedFromAup = true;
      SetProjectTitle();
   }

   return(success);
}


bool AudacityProject::SaveAs(bool bWantSaveCopy /*= false*/, bool bLossless /*= false*/)
{
   TitleRestorer Restorer(this); // RAII
   bool bHasPath = true;
   wxFileName filename(mFileName);
   // Save a copy of the project with 32-bit float tracks.
   if (bLossless)
      bWantSaveCopy = true;

   // Bug 1304: Set a default file path if none was given.  For Save/SaveAs
   if( !FileNames::IsPathAvailable( filename.GetPath( wxPATH_GET_VOLUME| wxPATH_GET_SEPARATOR) ) ){
      bHasPath = false;
      filename = FileNames::DefaultToDocumentsFolder(wxT("/SaveAs/Path"));
   }

   wxString title;
   wxString message;
   if (bWantSaveCopy)
   {
      if (bLossless)
      {
         title = wxString::Format(_("%sSave Lossless Copy of Project \"%s\" As..."),
                                           Restorer.sProjNumber,Restorer.sProjName);
         message = _("\
'Save Lossless Copy of Project' is for an Audacity project, not an audio file.\n\
For an audio file that will open in other apps, use 'Export'.\n\n\
\
Lossless copies of project are a good way to backup your project, \n\
with no loss of quality, but the projects are large.\n");
      }
      else
      {
         title = wxString::Format(_("%sSave Compressed Copy of Project \"%s\" As..."),
                                           Restorer.sProjNumber,Restorer.sProjName);
         message = _("\
'Save Compressed Copy of Project' is for an Audacity project, not an audio file.\n\
For an audio file that will open in other apps, use 'Export'.\n\n\
\
Compressed project files are a good way to transmit your project online, \n\
but they have some loss of fidelity.\n");
      }
   }
   else
   {
      title = wxString::Format(_("%sSave Project \"%s\" As..."),
                               Restorer.sProjNumber, Restorer.sProjName);
      message = _("\
'Save Project' is for an Audacity project, not an audio file.\n\
For an audio file that will open in other apps, use 'Export'.\n");
   }
   if (ShowWarningDialog(this, wxT("FirstProjectSave"), message, true) != wxID_OK)
   {
      return false;
   }

   bool bPrompt = (mBatchMode == 0) || (mFileName.empty());
   wxString fName;

   if (bPrompt) {
      // JKC: I removed 'wxFD_OVERWRITE_PROMPT' because we are checking
      // for overwrite ourselves later, and we disallow it.
      // We disallow overwrite because we would have to DELETE the many
      // smaller files too, or prompt to move them.
      fName = FileNames::SelectFile(FileNames::Operation::Export,
         title,
         filename.GetPath(),
         filename.GetFullName(),
         wxT("aup"),
         _("Audacity projects") + wxT(" (*.aup)|*.aup"),
         wxFD_SAVE | wxRESIZE_BORDER,
         this);

      if (fName.empty())
         return false;

      filename = fName;
   };

   filename.SetExt(wxT("aup"));
   fName = filename.GetFullPath();

   if ((bWantSaveCopy||!bPrompt) && filename.FileExists()) {
      // Saving a copy of the project should never overwrite an existing project.
      AudacityMessageDialog m(
         NULL,
         _("Saving a copy must not overwrite an existing saved project.\nPlease try again and select an original name."),
         _("Error Saving Copy of Project"),
         wxOK|wxICON_ERROR);
      m.ShowModal();
      return false;
   }

   bool bOwnsNewAupName = mbLoadedFromAup && (mFileName==fName);
   // Check to see if the project file already exists, and if it does
   // check that the project file 'belongs' to this project.
   // otherwise, prompt the user before overwriting.
   if (!bOwnsNewAupName && filename.FileExists()) {
      // Ensure that project of same name is not open in another window.
      // fName is the destination file.
      // mFileName is this project.
      // It is possible for mFileName == fName even when this project is not
      // saved to disk, and we then need to check the destination file is not
      // open in another window.
      int mayOverwrite = (mFileName == fName)? 2 : 1;
      for ( auto p : AllProjects{} ) {
         const wxFileName openProjectName(p->mFileName);
         if (openProjectName.SameAs(fName)) {
            mayOverwrite -= 1;
            if (mayOverwrite == 0)
               break;
         }
      }

      if (mayOverwrite > 0) {
         /* i18n-hint: In each case, %s is the name
          of the file being overwritten.*/
         wxString Message = wxString::Format(_("\
Do you want to overwrite the project:\n\"%s\"?\n\n\
If you select \"Yes\" the project\n\"%s\"\n\
will be irreversibly overwritten."), fName, fName);

         // For safety, there should NOT be an option to hide this warning.
         int result = AudacityMessageBox(Message,
                                         /* i18n-hint: Heading: A warning that a project is about to be overwritten.*/
                                         _("Overwrite Project Warning"),
                                         wxYES_NO | wxNO_DEFAULT | wxICON_WARNING,
                                         this);
         if (result != wxYES) {
            return false;
         }
      }
      else
      {
         // Overwrite disalowed. The destination project is open in another window.
         AudacityMessageDialog m(
            NULL,
            _("The project will not saved because the selected project is open in another window.\nPlease try again and select an original name."),
            _("Error Saving Project"),
            wxOK|wxICON_ERROR);
         m.ShowModal();
         return false;
      }
   }

   auto oldFileName = mFileName;
   mFileName = fName;
   bool success = false;
   auto cleanup = finally( [&] {
      if (!success || bWantSaveCopy)
         // Restore file name on error
         mFileName = oldFileName;
   } );

   success = DoSave(!bOwnsNewAupName || bWantSaveCopy, bWantSaveCopy, bLossless);

   if (success) {
      FileHistory::Global().AddFileToHistory(mFileName);
      if( !bHasPath )
      {
         gPrefs->Write( wxT("/SaveAs/Path"), filename.GetPath());
         gPrefs->Flush();
      }
   }
   if (!success || bWantSaveCopy) // bWantSaveCopy doesn't actually change current project.
   {
   }
   else {
      mbLoadedFromAup = true;
      SetProjectTitle();
   }


   return(success);
}

// Utility function called by other zoom methods
void AudacityProject::Zoom(double level)
{
   auto &project = *this;
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
void AudacityProject::ZoomBy(double multiplier)
{
   auto &project = *this;
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
void AudacityProject::Rewind(bool shift)
{
   auto &project = *this;
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
void AudacityProject::SkipEnd(bool shift)
{
   auto &project = *this;
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

// TrackPanel callback method
void AudacityProject::SetStatus(const wxString &msg)
{
   auto &project = *this;
   if ( msg != mLastMainStatusMessage ) {
      mLastMainStatusMessage = msg;
      wxCommandEvent evt{ EVT_PROJECT_STATUS_UPDATE };
      project.GetEventHandler()->ProcessEvent( evt );
   }
}

void AudacityProject::TP_DisplaySelection()
{
   auto &project = *this;
   auto &ruler = AdornedRulerPanel::Get(project);
   auto &viewInfo = ViewInfo::Get( project );
   const auto &selectedRegion = viewInfo.selectedRegion;
   double audioTime;

   if (!gAudioIO->IsBusy() && !mLockPlayRegion)
      ruler.SetPlayRegion( selectedRegion.t0(), selectedRegion.t1() );
   else
      // Cause ruler redraw anyway, because we may be zooming or scrolling
      ruler.Refresh();

   if (gAudioIO->IsBusy())
      audioTime = gAudioIO->GetStreamTime();
   else {
      double playEnd;
      GetPlayRegion(&audioTime, &playEnd);
   }

   SelectionBar::Get( project ).SetTimes(selectedRegion.t0(),
                               selectedRegion.t1(), audioTime);
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   SpectralSelectionBar::Get( project ).SetFrequencies(
      selectedRegion.f0(), selectedRegion.f1());
#endif
}


// TrackPanel callback method
void AudacityProject::TP_ScrollLeft()
{
   OnScrollLeft();
}

// TrackPanel callback method
void AudacityProject::TP_ScrollRight()
{
   OnScrollRight();
}

// TrackPanel callback method
void AudacityProject::TP_RedrawScrollbars()
{
   FixScrollbars();
}

void AudacityProject::TP_HandleResize()
{
   HandleResize();
}

void AudacityProject::GetPlayRegion(double* playRegionStart,
                                    double *playRegionEnd)
{
   auto &project = *this;
   AdornedRulerPanel::Get( project ).GetPlayRegion(
      playRegionStart, playRegionEnd);
}

void AudacityProject::AutoSave()
{
   //    SonifyBeginAutoSave(); // part of RBD's r10680 stuff now backed out

   // To minimize the possibility of race conditions, we first write to a
   // file with the extension ".tmp", then rename the file to .autosave
   wxString projName;

   if (mFileName.empty())
      projName = wxT("New Project");
   else
      projName = wxFileName(mFileName).GetName();

   wxString fn = wxFileName(FileNames::AutoSaveDir(),
      projName + wxString(wxT(" - ")) + CreateUniqueName()).GetFullPath();

   // PRL:  I found a try-catch and rewrote it,
   // but this guard is unnecessary because AutoSaveFile does not throw
   bool success = GuardedCall< bool >( [&]
   {
      VarSetter<bool> setter(&mAutoSaving, true, false);

      AutoSaveFile buffer;
      WriteXMLHeader(buffer);
      WriteXML(buffer, false);
      mStrOtherNamesArray.clear();

      wxFFile saveFile;
      saveFile.Open(fn + wxT(".tmp"), wxT("wb"));
      return buffer.Write(saveFile);
   } );

   if (!success)
      return;

   // Now that we have a NEW auto-save file, DELETE the old one
   DeleteCurrentAutoSaveFile();

   if (!mAutoSaveFileName.empty())
      return; // could not remove auto-save file

   if (!wxRenameFile(fn + wxT(".tmp"), fn + wxT(".autosave")))
   {
      AudacityMessageBox(
         wxString::Format( _("Could not create autosave file: %s"),
            fn + wxT(".autosave") ),
         _("Error"), wxICON_STOP, this);
      return;
   }

   mAutoSaveFileName += fn + wxT(".autosave");
   // no-op cruft that's not #ifdefed for NoteTrack
   // See above for further comments.
   //   SonifyEndAutoSave();
}

void AudacityProject::DeleteCurrentAutoSaveFile()
{
   if (!mAutoSaveFileName.empty())
   {
      if (wxFileExists(mAutoSaveFileName))
      {
         if (!wxRemoveFile(mAutoSaveFileName))
         {
            AudacityMessageBox(
               wxString::Format(
                  _("Could not remove old autosave file: %s"), mAutoSaveFileName ),
               _("Error"), wxICON_STOP, this);
            return;
         }
      }

      mAutoSaveFileName = wxT("");
   }
}


bool AudacityProject::IsProjectSaved() {
   auto &project = *this;
   auto &dirManager = DirManager::Get( project );
   // This is true if a project was opened from an .aup
   // Otherwise it becomes true only when a project is first saved successfully
   // in DirManager::SetProject
   return (!dirManager.GetProjectName().empty());
}

void AudacityProject::ResetProjectFileIO()
{
   // mLastSavedTrack code copied from OnCloseWindow.
   // Lock all blocks in all tracks of the last saved version, so that
   // the blockfiles aren't deleted on disk when we DELETE the blockfiles
   // in memory.  After it's locked, DELETE the data structure so that
   // there's no memory leak.
   CloseLock();

   //mLastSavedTracks = TrackList::Create();
   mFileName = "";
   mIsRecovered = false;
   mbLoadedFromAup = false;
   SetProjectTitle();
}

bool AudacityProject::SaveFromTimerRecording(wxFileName fnFile) {
   // MY: Will save the project to a NEW location a-la Save As
   // and then tidy up after itself.

   wxString sNewFileName = fnFile.GetFullPath();

   // MY: To allow SaveAs from Timer Recording we need to check what
   // the value of mFileName is before we change it.
   FilePath sOldFilename;
   if (IsProjectSaved()) {
      sOldFilename = mFileName;
   }

   // MY: If the project file already exists then bail out
   // and send populate the message string (pointer) so
   // we can tell the user what went wrong.
   if (wxFileExists(sNewFileName)) {
      return false;
   }

   mFileName = sNewFileName;
   bool bSuccess = false;
   auto cleanup = finally( [&] {
      if (!bSuccess)
         // Restore file name on error
         mFileName = sOldFilename;
   } );

   bSuccess = DoSave(true, false);

   if (bSuccess) {
      FileHistory::Global().AddFileToHistory(mFileName);
      mbLoadedFromAup = true;
      SetProjectTitle();
   }

   return bSuccess;
}

AudacityProject::PlaybackScroller::PlaybackScroller(AudacityProject *project)
: mProject(project)
{
   ViewInfo::Get( *mProject ).Bind(EVT_TRACK_PANEL_TIMER,
      &PlaybackScroller::OnTimer,
      this);
}

void AudacityProject::PlaybackScroller::OnTimer(wxCommandEvent &event)
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

void AudacityProject::ZoomInByFactor( double ZoomFactor )
{
   auto &project = *this;
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

void AudacityProject::ZoomOutByFactor( double ZoomFactor )
{
   auto &project = *this;
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

void AudacityProject::CloseLock()
{
   // Lock all blocks in all tracks of the last saved version, so that
   // the blockfiles aren't deleted on disk when we DELETE the blockfiles
   // in memory.  After it's locked, DELETE the data structure so that
   // there's no memory leak.
   if (mLastSavedTracks) {
      for (auto wt : mLastSavedTracks->Any<WaveTrack>())
         wt->CloseLock();

      mLastSavedTracks->Clear();
      mLastSavedTracks.reset();
   }
}
