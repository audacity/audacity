/**********************************************************************

Audacity: A Digital Audio Editor

ProjectManager.cpp

Paul Licameli split from AudacityProject.cpp

**********************************************************************/

#include "ProjectManager.h"



#include "ActiveProject.h"
#include "AdornedRulerPanel.h"
#include "AudioIO.h"
#include "Clipboard.h"
#include "FileNames.h"
#include "Menus.h"
#include "ModuleManager.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "ProjectAudioManager.h"
#include "ProjectFileIO.h"
#include "ProjectFileManager.h"
#include "ProjectHistory.h"
#include "ProjectWindows.h"
#include "ProjectRate.h"
#include "ProjectSettings.h"
#include "ProjectStatus.h"
#include "ProjectWindow.h"
#include "SelectUtilities.h"
#include "TrackPanel.h"
#include "TrackUtilities.h"
#include "UndoManager.h"
#include "WaveTrack.h"
#include "wxFileNameWrapper.h"
#include "import/Import.h"
#include "import/ImportMIDI.h"
#include "QualitySettings.h"
#include "toolbars/MeterToolBar.h"
#include "toolbars/ToolManager.h"
#include "AudacityMessageBox.h"
#include "widgets/FileHistory.h"
#include "WindowAccessible.h"

#include <wx/app.h>
#include <wx/scrolbar.h>
#include <wx/sizer.h>
#include <wx/splitter.h>

#ifdef __WXGTK__
#include "../images/AudacityLogoAlpha.xpm"
#endif

namespace {
   void SaveWindowPreferences(const wxRect& windowRect, const wxRect& normalRect,
                              bool isMaximized, bool isIconized) {
      ProjectWindowX.Write(windowRect.GetX());
      ProjectWindowY.Write(windowRect.GetY());
      ProjectWindowWidth.Write(windowRect.GetWidth());
      ProjectWindowHeight.Write(windowRect.GetHeight());
      ProjectWindowMaximized.Write(isMaximized);
      ProjectWindowNormalX.Write(normalRect.GetX());
      ProjectWindowNormalY.Write(normalRect.GetY());
      ProjectWindowNormalWidth.Write(normalRect.GetWidth());
      ProjectWindowNormalHeight.Write(normalRect.GetHeight());
      ProjectWindowIconized.Write(isIconized);
   }
}

const int AudacityProjectTimerID = 5200;

static AudacityProject::AttachedObjects::RegisteredFactory sProjectManagerKey {
   []( AudacityProject &project ) {
      return std::make_shared< ProjectManager >( project );
   }
};

ProjectManager &ProjectManager::Get( AudacityProject &project )
{
   return project.AttachedObjects::Get< ProjectManager >( sProjectManagerKey );
}

const ProjectManager &ProjectManager::Get( const AudacityProject &project )
{
   return Get( const_cast< AudacityProject & >( project ) );
}

ProjectManager::ProjectManager( AudacityProject &project )
   : mProject{ project }
   , mTimer{ std::make_unique<wxTimer>(this, AudacityProjectTimerID) }
{
   auto &window = ProjectWindow::Get( mProject );
   window.Bind( wxEVT_CLOSE_WINDOW, &ProjectManager::OnCloseWindow, this );
   mProjectStatusSubscription = ProjectStatus::Get(mProject)
      .Subscribe(*this, &ProjectManager::OnStatusChange);
   mProjectFileIOSubscription = ProjectFileIO::Get(mProject)
      .Subscribe(*this, &ProjectManager::OnReconnectionFailure);
}

ProjectManager::~ProjectManager() = default;

BEGIN_EVENT_TABLE( ProjectManager, wxEvtHandler )
   EVT_TIMER(AudacityProjectTimerID, ProjectManager::OnTimer)
END_EVENT_TABLE()

bool ProjectManager::sbWindowRectAlreadySaved = false;
bool ProjectManager::sbSkipPromptingForSave = false;

void ProjectManager::SaveWindowSize()
{
   if (sbWindowRectAlreadySaved)
   {
      return;
   }
   bool validWindowForSaveWindowSize = FALSE;
   ProjectWindow * validProject = nullptr;
   bool foundIconizedProject = FALSE;
   for ( auto pProject : AllProjects{} )
   {
      auto &window = ProjectWindow::Get( *pProject );
      if (!window.IsIconized()) {
         validWindowForSaveWindowSize = TRUE;
         validProject = &window;
         break;
      }
      else
         foundIconizedProject =  TRUE;
   }

   if (validWindowForSaveWindowSize)
   {
      wxRect windowRect = validProject->GetRect();
      wxRect normalRect = validProject->GetNormalizedWindowState();
      bool wndMaximized = validProject->IsMaximized();

      SaveWindowPreferences(windowRect, normalRect, wndMaximized, false);
   }
   else
   {
      if (foundIconizedProject) {
         validProject = &ProjectWindow::Get( **AllProjects{}.begin() );
         bool wndMaximized = validProject->IsMaximized();
         wxRect normalRect = validProject->GetNormalizedWindowState();

         // store only the normal rectangle because the itemized rectangle
         // makes no sense for an opening project window
         SaveWindowPreferences(normalRect, normalRect, wndMaximized, true);
      }
      else {
         // this would be a very strange case that might possibly occur on the Mac
         // Audacity would have to be running with no projects open
         // in this case we are going to write only the default values
         wxRect defWndRect;
         GetDefaultWindowRect(&defWndRect);
         SaveWindowPreferences(defWndRect, defWndRect, false, false);
      }
   }
   sbWindowRectAlreadySaved = true;
}

void InitProjectWindow( ProjectWindow &window )
{
   auto pProject = window.FindProject();
   if (!pProject)
      return;
   auto &project = *pProject;

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
   auto statusBar = window.CreateStatusBar(4);
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
   ToolManager::Get( project ).CreateWindows();
   ToolManager::Get( project ).LayoutToolBars();

   //
   // Create the TrackPanel and the scrollbars
   //

   auto topPanel = window.GetTopPanel();

   {
      auto ubs = std::make_unique<wxBoxSizer>(wxVERTICAL);
      ubs->Add( ToolManager::Get( project ).GetTopDock(), 0, wxEXPAND | wxALIGN_TOP );
      topPanel->SetSizer(ubs.release());
   }

   wxBoxSizer *bs;
   {
      auto ubs = std::make_unique<wxBoxSizer>(wxVERTICAL);
      bs = ubs.get();
      bs->Add(topPanel, 0, wxEXPAND | wxALIGN_TOP);
      bs->Add(window.GetContainerWindow(), 1, wxEXPAND);
      bs->Add( ToolManager::Get( project ).GetBotDock(), 0, wxEXPAND );
      window.SetAutoLayout(true);
      window.SetSizer(ubs.release());
   }
   bs->Layout();

   auto &trackPanel = TrackPanel::Get( project );

   // LLL: When Audacity starts or becomes active after returning from
   //      another application, the first window that can accept focus
   //      will be given the focus even if we try to SetFocus().  By
   //      making the TrackPanel that first window, we resolve several
   //      keyboard focus problems.
   window.GetContainerWindow()->MoveAfterInTabOrder(topPanel);

   const auto trackListWindow = window.GetTrackListWindow();

   //
   // Create the horizontal ruler
   //
   auto &ruler = AdornedRulerPanel::Get( project );

   bs = static_cast<wxBoxSizer*>(trackListWindow->GetSizer());

   auto vsBar = &window.GetVerticalScrollBar();
   auto hsBar = &window.GetHorizontalScrollBar();

   {
      // Top horizontal grouping
      auto hs = std::make_unique<wxBoxSizer>(wxHORIZONTAL);

      // Track panel
      hs->Add(&trackPanel, 1, wxEXPAND | wxALIGN_LEFT | wxALIGN_TOP);

      {
         // Vertical grouping
         auto vs = std::make_unique<wxBoxSizer>(wxVERTICAL);

         // Vertical scroll bar
         vs->Add(vsBar, 1, wxEXPAND | wxALIGN_TOP);
         hs->Add(vs.release(), 0, wxEXPAND | wxALIGN_TOP);
      }

      bs->Add(&ruler, 0, wxEXPAND | wxALIGN_TOP);
      bs->Add(hs.release(), 1, wxEXPAND | wxALIGN_LEFT | wxALIGN_TOP);
   }

   {
      // Bottom horizontal grouping
      auto hs = std::make_unique<wxBoxSizer>(wxHORIZONTAL);

      // Bottom scrollbar
      hs->Add(viewInfo.GetLeftOffset() - 1, 0);
      hs->Add(hsBar, 1, wxALIGN_BOTTOM);
      hs->Add(vsBar->GetSize().GetWidth(), 0);
      bs->Add(hs.release(), 0, wxEXPAND | wxALIGN_LEFT);
   }

   // Lay it out
   trackListWindow->SetAutoLayout(true);
   trackListWindow->Layout();

   wxASSERT( trackPanel.GetProject() == &project );

   // MM: Give track panel the focus to ensure keyboard commands work
   trackPanel.SetFocus();

   window.FixScrollbars();
   ruler.SetLeftOffset(viewInfo.GetLeftOffset());  // bevel on AdornedRuler

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
      window.SetIcon(ic);
   }
#endif

   window.UpdateStatusWidths();
   auto msg = XO("Welcome to Audacity version %s")
      .Format( AUDACITY_VERSION_STRING );
   ProjectManager::Get( project ).SetStatusText( msg, mainStatusBarField );

#ifdef EXPERIMENTAL_DA2
   ClearBackground();// For wxGTK.
#endif
}

AudacityProject *ProjectManager::New()
{
   wxRect wndRect;
   bool bMaximized = false;
   bool bIconized = false;
   GetNextWindowPlacement(&wndRect, &bMaximized, &bIconized);
   
   // Create and show a NEW project
   // Use a non-default deleter in the smart pointer!
   auto sp = AudacityProject::Create();
   AllProjects{}.Add( sp );
   auto p = sp.get();
   auto &project = *p;
   auto &projectHistory = ProjectHistory::Get( project );
   auto &projectManager = Get( project );
   auto &window = ProjectWindow::Get( *p );

   // Issue #2569
   // There is a dependency on the order of initialisation.
   // The menus must be created, and registered, before
   // InitProjectWindows can UpdateMenus.
   MenuManager::Get(project).CreateMenusAndCommands(project);

   InitProjectWindow( window );

   // wxGTK3 seems to need to require creating the window using default position
   // and then manually positioning it.
   window.SetPosition(wndRect.GetPosition());

   auto &projectFileManager = ProjectFileManager::Get( *p );

   // This may report an error.
   projectFileManager.OpenNewProject();

   projectHistory.InitialState();
   projectManager.RestartTimer();
   
   if(bMaximized) {
      window.Maximize(true);
   }
   else if (bIconized) {
      // if the user close down and iconized state we could start back up and iconized state
      // window.Iconize(TRUE);
   }
   
   //Initialise the Listeners
   auto gAudioIO = AudioIO::Get();
   gAudioIO->SetListener(
      ProjectAudioManager::Get( project ).shared_from_this() );

   //Set the NEW project as active:
   SetActiveProject(p);
   
   // Okay, GetActiveProject() is ready. Now we can get its CommandManager,
   // and add the shortcut keys to the tooltips.
   ToolManager::Get( *p ).RegenerateTooltips();
   
   ModuleManager::Get().Dispatch(ProjectInitialized);
   
   window.Show(true);
   
   return p;
}

void ProjectManager::OnReconnectionFailure(ProjectFileIOMessage message)
{
   if (message == ProjectFileIOMessage::ReconnectionFailure)
      ProjectWindow::Get(mProject).Close(true);
}

static bool sbClosingAll = false;

void ProjectManager::SetClosingAll(bool closing)
{
   sbClosingAll = closing;
}

void ProjectManager::OnCloseWindow(wxCloseEvent & event)
{
   auto &project = mProject;
   auto &projectFileIO = ProjectFileIO::Get( project );
   auto &projectFileManager = ProjectFileManager::Get( project );
   const auto &settings = ProjectSettings::Get( project );
   auto &projectAudioIO = ProjectAudioIO::Get( project );
   auto &tracks = TrackList::Get( project );
   auto &window = ProjectWindow::Get( project );
   auto gAudioIO = AudioIO::Get();

   // We are called for the wxEVT_CLOSE_WINDOW, wxEVT_END_SESSION, and
   // wxEVT_QUERY_END_SESSION, so we have to protect against multiple
   // entries.  This is a hack until the whole application termination
   // process can be reviewed and reworked.  (See bug #964 for ways
   // to exercise the bug that instigated this hack.)
   if (window.IsBeingDeleted())
   {
      event.Skip();
      return;
   }

   if (event.CanVeto() && (::wxIsBusy() || project.mbBusyImporting))
   {
      event.Veto();
      return;
   }

   // Check to see if we were playing or recording
   // audio, and if so, make sure Audio I/O is completely finished.
   // The main point of this is to properly push the state
   // and flush the tracks once we've completely finished
   // recording NEW state.
   // This code is derived from similar code in
   // AudacityProject::~AudacityProject() and TrackPanel::OnTimer().
   if (projectAudioIO.GetAudioIOToken()>0 &&
       gAudioIO->IsStreamActive(projectAudioIO.GetAudioIOToken())) {

      // We were playing or recording audio, but we've stopped the stream.
      ProjectAudioManager::Get( project ).Stop();

      projectAudioIO.SetAudioIOToken(0);
      window.RedrawProject();
   }
   else if (gAudioIO->IsMonitoring()) {
      gAudioIO->StopStream();
   }

   // MY: Use routine here so other processes can make same check
   bool bHasTracks = !tracks.empty();

   // We may not bother to prompt the user to save, if the
   // project is now empty.
   if (!sbSkipPromptingForSave 
      && event.CanVeto() 
      && (settings.EmptyCanBeDirty() || bHasTracks)) {
      if ( UndoManager::Get( project ).UnsavedChanges() ) {
         TitleRestorer Restorer( window, project );// RAII
         /* i18n-hint: The first %s numbers the project, the second %s is the project name.*/
         auto Title = XO("%sSave changes to %s?")
            .Format( Restorer.sProjNumber, Restorer.sProjName );
         auto Message = XO("Save project before closing?");
         if( !bHasTracks )
         {
          Message += XO("\nIf saved, the project will have no tracks.\n\nTo save any previously open tracks:\nCancel, Edit > Undo until all tracks\nare open, then File > Save Project.");
         }
         int result = AudacityMessageBox(
            Message,
            Title,
            wxYES_NO | wxCANCEL | wxICON_QUESTION,
            &window);

         if (result == wxCANCEL || (result == wxYES &&
              !GuardedCall<bool>( [&]{ return projectFileManager.Save(); } )
         )) {
            event.Veto();
            return;
         }
      }
   }
#ifdef __WXMAC__
   // Fix bug apparently introduced into 2.1.2 because of wxWidgets 3:
   // closing a project that was made full-screen (as by clicking the green dot
   // or command+/; not merely "maximized" as by clicking the title bar or
   // Zoom in the Window menu) leaves the screen black.
   // Fix it by un-full-screening.
   // (But is there a different way to do this? What do other applications do?
   //  I don't see full screen windows of Safari shrinking, but I do see
   //  momentary blackness.)
   window.ShowFullScreen(false);
#endif

   // This achieves auto save on close of project before other important
   // project state is destroyed
   window.Publish(ProjectWindowDestroyedMessage {});
   ModuleManager::Get().Dispatch(ProjectClosing);

   // Stop the timer since there's no need to update anything anymore
   mTimer.reset();

   // DMM: Save the size of the last window the user closes
   //
   // LL: Save before doing anything else to the window that might make
   //     its size change.
   SaveWindowSize();

   window.SetIsBeingDeleted();

   // Mac: we never quit as the result of a close.
   // Other systems: we quit only when the close is the result of an external
   // command (on Windows, those are taskbar closes, "X" box, Alt+F4, etc.)
   bool quitOnClose;
#ifdef __WXMAC__
   quitOnClose = false;
#else
   quitOnClose = !projectFileManager.GetMenuClose();
#endif

   // DanH: If we're definitely about to quit, clear the clipboard.
   auto &clipboard = Clipboard::Get();
   if ((AllProjects{}.size() == 1) &&
      (quitOnClose || sbClosingAll))
      clipboard.Clear();
   else {
      auto clipboardProject = clipboard.Project().lock();
      if ( clipboardProject.get() == &mProject ) {
         // Closing the project from which content was cut or copied.
         // For 3.0.0, clear the clipboard, because accessing clipboard contents
         // would depend on a database connection to the closing project, but
         // that connection should closed now so that the project file can be
         // freely moved.
         // Notes:
         // 1) maybe clipboard contents could be saved by migrating them to
         // another temporary database, but that extra effort is beyond the
         // scope of 3.0.0.
         // 2) strictly speaking this is necessary only when the clipboard
         // contains WaveTracks.
         clipboard.Clear();
      }
   }

   // JKC: For Win98 and Linux do not detach the menu bar.
   // We want wxWidgets to clean it up for us.
   // TODO: Is there a Mac issue here??
   // SetMenuBar(NULL);

   // Compact the project.
   projectFileManager.CompactProjectOnClose();

   // Set (or not) the bypass flag to indicate that deletes that would happen during
   // the UndoManager::ClearStates() below are not necessary.
   projectFileIO.SetBypass();

   {
      // This can reduce reference counts of sample blocks in the project's
      // tracks.
      UndoManager::Get( project ).ClearStates();

      // Delete all the tracks to free up memory
      tracks.Clear();
   }

   // Some of the AdornedRulerPanel functions refer to the TrackPanel, so destroy this
   // before the TrackPanel is destroyed. This change was needed to stop Audacity
   // crashing when running with Jaws on Windows 10 1703.
   AdornedRulerPanel::Destroy( project );

   // Destroy the TrackPanel early so it's not around once we start
   // deleting things like tracks and such out from underneath it.
   // Check validity of mTrackPanel per bug 584 Comment 1.
   // Deeper fix is in the Import code, but this failsafes against crash.
   TrackPanel::Destroy( project );
   // Finalize the tool manager before the children since it needs
   // to save the state of the toolbars.
   ToolManager::Get( project ).Destroy();

   window.DestroyChildren();

   // Close project only now, because TrackPanel might have been holding
   // some shared_ptr to WaveTracks keeping SampleBlocks alive.
   // We're all done with the project file, so close it now
   projectFileManager.CloseProject();

   WaveTrackFactory::Destroy( project );

   // Remove self from the global array, but defer destruction of self
   auto pSelf = AllProjects{}.Remove( project );
   wxASSERT( pSelf );

   if (GetActiveProject().lock().get() == &project) {
      // Find a NEW active project
      if ( !AllProjects{}.empty() ) {
         SetActiveProject(AllProjects{}.begin()->get());
      }
      else {
         SetActiveProject(nullptr);
      }
   }

   // Since we're going to be destroyed, make sure we're not to
   // receive audio notifications anymore.
   // PRL:  Maybe all this is unnecessary now that the listener is managed
   // by a weak pointer.
   if ( gAudioIO->GetListener().get() == &ProjectAudioManager::Get( project ) ) {
      auto active = GetActiveProject().lock();
      gAudioIO->SetListener(
         active
            ? ProjectAudioManager::Get( *active ).shared_from_this()
            : nullptr
      );
   }

   if (AllProjects{}.empty() && !sbClosingAll) {

#if !defined(__WXMAC__)
      if (quitOnClose) {
         // Simulate the application Exit menu item
         wxCommandEvent evt{ wxEVT_MENU, wxID_EXIT };
         wxTheApp->AddPendingEvent( evt );
      }
      else {
         sbWindowRectAlreadySaved = false;
         // For non-Mac, always keep at least one project window open
         (void) New();
      }
#endif
   }

   window.Destroy();

   // Destroys this
   pSelf.reset();
}

// static method, can be called outside of a project
void ProjectManager::OpenFiles(AudacityProject *proj)
{
   auto selectedFiles =
      ProjectFileManager::ShowOpenDialog(FileNames::Operation::Open);
   if (selectedFiles.size() == 0) {
      Importer::SetLastOpenType({});
      return;
   }

   //first sort selectedFiles.
   selectedFiles.Sort(FileNames::CompareNoCase);

   auto cleanup = finally( [] {
      Importer::SetLastOpenType({});
   } );

   for (const auto &fileName : selectedFiles) {
      // Make sure it isn't already open.
      if (ProjectFileManager::IsAlreadyOpen(fileName))
         continue; // Skip ones that are already open.

      proj = OpenProject( proj, fileName,
         true /* addtohistory */, false /* reuseNonemptyProject */ );
   }
}

bool ProjectManager::SafeToOpenProjectInto(AudacityProject &proj)
{
   // DMM: If the project is dirty, that means it's been touched at
   // all, and it's not safe to open a fresh project directly in its
   // place.  Only if the project is brandnew clean and the user
   // hasn't done any action at all is it safe for Open to take place
   // inside the current project.
   //
   // If you try to Open a fresh project inside the current window when
   // there are no tracks, but there's an Undo history, etc, then
   // bad things can happen, including orphan blocks, or tracks
   // referring to non-existent blocks
   if (
      ProjectHistory::Get( proj ).GetDirty() ||
      !TrackList::Get( proj ).empty()
   )
      return false;
   // This project is clean; it's never been touched.  Therefore
   // all relevant member variables are in their initial state,
   // and it's okay to open a NEW project inside its window.
   return true;
}

ProjectManager::ProjectChooser::~ProjectChooser()
{
   if (mpUsedProject) {
      if (mpUsedProject == mpGivenProject) {
         // Ensure that it happens here: don't wait for the application level
         // exception handler, because the exception may be intercepted
         ProjectHistory::Get(*mpGivenProject).RollbackState();
         // Any exception now continues propagating
      }
      else
         GetProjectFrame( *mpUsedProject ).Close(true);
   }
}

AudacityProject &
ProjectManager::ProjectChooser::operator() ( bool openingProjectFile )
{
   if (mpGivenProject) {
      // Always check before opening a project file (for safety);
      // May check even when opening other files
      // (to preserve old behavior; as with the File > Open command specifying
      // multiple files of whatever types, so that each gets its own window)
      bool checkReuse = (openingProjectFile || !mReuseNonemptyProject);
      if (!checkReuse || SafeToOpenProjectInto(*mpGivenProject))
         return *(mpUsedProject = mpGivenProject);
   }
   return *(mpUsedProject = New());
}

void ProjectManager::ProjectChooser::Commit()
{
   mpUsedProject = nullptr;
}

AudacityProject *ProjectManager::OpenProject(
   AudacityProject *pGivenProject, const FilePath &fileNameArg,
   bool addtohistory, bool reuseNonemptyProject)
{
   ProjectManager::ProjectChooser chooser{ pGivenProject, reuseNonemptyProject };
   if (auto pProject = ProjectFileManager::OpenFile(
      std::ref(chooser), fileNameArg, addtohistory )) {
      chooser.Commit();

      auto &projectFileIO = ProjectFileIO::Get( *pProject );
      if( projectFileIO.IsRecovered() ) {
         auto &window = ProjectWindow::Get( *pProject );
         window.Zoom( window.GetZoomOfToFit() );
         // "Project was recovered" replaces "Create new project" in Undo History.
         auto &undoManager = UndoManager::Get( *pProject );
         undoManager.RemoveStates(0, 1);
      }
      return pProject;
   }
   return nullptr;
}

// This is done to empty out the tracks, but without creating a new project.
void ProjectManager::ResetProjectToEmpty() {
   auto &project = mProject;
   auto &projectFileIO = ProjectFileIO::Get( project );
   auto &projectFileManager = ProjectFileManager::Get( project );
   auto &projectHistory = ProjectHistory::Get( project );
   auto &viewInfo = ViewInfo::Get( project );

   SelectUtilities::DoSelectAll( project );
   TrackUtilities::DoRemoveTracks( project );

   WaveTrackFactory::Reset( project );

   // InitialState will reset UndoManager
   projectHistory.InitialState();
   projectHistory.SetDirty(false);

   projectFileManager.CloseProject();
   projectFileManager.OpenProject();
}

void ProjectManager::RestartTimer()
{
   if (mTimer) {
      // mTimer->Stop(); // not really needed
      mTimer->Start( 3000 ); // Update messages as needed once every 3 s.
   }
}

void ProjectManager::OnTimer(wxTimerEvent& WXUNUSED(event))
{
   auto &project = mProject;
   auto &projectAudioIO = ProjectAudioIO::Get( project );
   auto meterToolBars = MeterToolBar::GetToolBars( project );

   for (auto& meterToolBar : meterToolBars)
      meterToolBar.get().UpdateControls();
   
   auto gAudioIO = AudioIO::Get();
   // gAudioIO->GetNumCaptureChannels() should only be positive
   // when we are recording.
   if (projectAudioIO.GetAudioIOToken() > 0 && gAudioIO->GetNumCaptureChannels() > 0) {
      wxLongLong freeSpace = ProjectFileIO::Get(project).GetFreeDiskSpace();
      if (freeSpace >= 0) {

         int iRecordingMins = GetEstimatedRecordingMinsLeftOnDisk(gAudioIO->GetNumCaptureChannels());
         auto sMessage = XO("Disk space remaining for recording: %s")
            .Format( GetHoursMinsString(iRecordingMins) );

         // Do not change mLastMainStatusMessage
         SetStatusText(sMessage, mainStatusBarField);
      }
   }

   // As also with the TrackPanel timer:  wxTimer may be unreliable without
   // some restarts
   RestartTimer();
}

void ProjectManager::OnStatusChange(StatusBarField field)
{
   auto &project = mProject;

   // Be careful to null-check the window.  We might get to this function
   // during shut-down, but a timer hasn't been told to stop sending its
   // messages yet.
   auto pWindow = ProjectWindow::Find( &project );
   if ( !pWindow )
      return;
   auto &window = *pWindow;

   window.UpdateStatusWidths();

   const auto &msg = ProjectStatus::Get( project ).Get( field );
   SetStatusText( msg, field );
   
   if ( field == mainStatusBarField )
      // When recording, let the NEW status message stay at least as long as
      // the timer interval (if it is not replaced again by this function),
      // before replacing it with the message about remaining disk capacity.
      RestartTimer();
}

void ProjectManager::SetStatusText( const TranslatableString &text, int number )
{
   auto &project = mProject;
   auto pWindow = ProjectWindow::Find( &project );
   if ( !pWindow )
      return;
   auto &window = *pWindow;
   window.GetStatusBar()->SetStatusText(text.Translation(), number);
}

TranslatableString ProjectManager::GetHoursMinsString(int iMinutes)
{
   if (iMinutes < 1)
      // Less than a minute...
      return XO("Less than 1 minute");

   // Calculate
   int iHours = iMinutes / 60;
   int iMins = iMinutes % 60;

   auto sHours = XP( "%d hour", "%d hours", 0 )( iHours );

   auto sMins = XP( "%d minute", "%d minutes", 0 )( iMins );

   /* i18n-hint: A time in hours and minutes. Only translate the "and". */
   return XO("%s and %s.").Format( sHours, sMins );
}

// This routine will give an estimate of how many
// minutes of recording time we have available.
// The calculations made are based on the user's current
// preferences.
int ProjectManager::GetEstimatedRecordingMinsLeftOnDisk(long lCaptureChannels) {
   auto &project = mProject;

   // Obtain the current settings
   auto oCaptureFormat = QualitySettings::SampleFormatChoice();
   if (lCaptureChannels == 0)
      lCaptureChannels = AudioIORecordChannels.Read();

   // Find out how much free space we have on disk
   wxLongLong lFreeSpace = ProjectFileIO::Get( project ).GetFreeDiskSpace();
   if (lFreeSpace < 0) {
      return 0;
   }

   // Calculate the remaining time
   double dRecTime = 0.0;
   double bytesOnDiskPerSample = SAMPLE_SIZE_DISK(oCaptureFormat);
   dRecTime = lFreeSpace.GetHi() * 4294967296.0 + lFreeSpace.GetLo();
   dRecTime /= bytesOnDiskPerSample;   
   dRecTime /= lCaptureChannels;
   dRecTime /= ProjectRate::Get( project ).GetRate();

   // Convert to minutes before returning
   int iRecMins = (int)round(dRecTime / 60.0);
   return iRecMins;
}
