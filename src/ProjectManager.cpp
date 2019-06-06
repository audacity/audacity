/**********************************************************************

Audacity: A Digital Audio Editor

ProjectManager.cpp

Paul Licameli split from AudacityProject.cpp

**********************************************************************/

#include "ProjectManager.h"

#include "AdornedRulerPanel.h"
#include "AudioIO.h"
#include "AutoRecovery.h"
#include "BlockFile.h"
#include "Clipboard.h"
#include "DirManager.h"
#include "FileNames.h"
#include "LabelTrack.h"
#include "Legacy.h"
#include "Menus.h"
#include "MissingAliasFileDialog.h"
#include "ModuleManager.h"
#include "PlatformCompatibility.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "ProjectFileIO.h"
#include "ProjectFileIORegistry.h"
#include "ProjectFSCK.h"
#include "ProjectSettings.h"
#include "ProjectWindow.h"
#include "Sequence.h"
#include "Snap.h"
#include "Tags.h"
#include "TrackPanel.h"
#include "UndoManager.h"
#include "ViewInfo.h"
#include "WaveTrack.h"
#include "WaveClip.h"
#include "wxFileNameWrapper.h"
#include "commands/CommandContext.h"
#include "effects/EffectManager.h"
#include "import/Import.h"
#include "ondemand/ODComputeSummaryTask.h"
#include "ondemand/ODManager.h"
#include "prefs/QualityPrefs.h"
#include "toolbars/ControlToolBar.h"
#include "toolbars/MixerToolBar.h"
#include "toolbars/SelectionBar.h"
#include "toolbars/SpectralSelectionBar.h"
#include "toolbars/ToolManager.h"
#include "widgets/AudacityMessageBox.h"
#include "widgets/FileHistory.h"
#include "widgets/ErrorDialog.h"
#include "widgets/Warning.h"

#include <wx/dataobj.h>
#include <wx/dnd.h>
#include <wx/evtloop.h>

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
   mProject.Bind(EVT_PROJECT_STATUS_UPDATE,
      &ProjectManager::OnStatusChange, this);
}

ProjectManager::~ProjectManager() = default;

// PRL:  This event type definition used to be in AudacityApp.h, which created
// a bad compilation dependency.  The event was never emitted anywhere.  I
// preserve it and its handler here but I move it to remove the dependency.
// Asynchronous open
wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API,
                         EVT_OPEN_AUDIO_FILE, wxCommandEvent);
wxDEFINE_EVENT(EVT_OPEN_AUDIO_FILE, wxCommandEvent);

BEGIN_EVENT_TABLE( ProjectManager, wxEvtHandler )
   EVT_COMMAND(wxID_ANY, EVT_OPEN_AUDIO_FILE, ProjectManager::OnOpenAudioFile)
   EVT_TIMER(AudacityProjectTimerID, ProjectManager::OnTimer)
END_EVENT_TABLE()

bool ProjectManager::sbWindowRectAlreadySaved = false;

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
      gPrefs->Write(wxT("/Window/X"), windowRect.GetX());
      gPrefs->Write(wxT("/Window/Y"), windowRect.GetY());
      gPrefs->Write(wxT("/Window/Width"), windowRect.GetWidth());
      gPrefs->Write(wxT("/Window/Height"), windowRect.GetHeight());
      gPrefs->Write(wxT("/Window/Maximized"), wndMaximized);
      gPrefs->Write(wxT("/Window/Normal_X"), normalRect.GetX());
      gPrefs->Write(wxT("/Window/Normal_Y"), normalRect.GetY());
      gPrefs->Write(wxT("/Window/Normal_Width"), normalRect.GetWidth());
      gPrefs->Write(wxT("/Window/Normal_Height"), normalRect.GetHeight());
      gPrefs->Write(wxT("/Window/Iconized"), FALSE);
   }
   else
   {
      if (foundIconizedProject) {
         validProject = &ProjectWindow::Get( **AllProjects{}.begin() );
         bool wndMaximized = validProject->IsMaximized();
         wxRect normalRect = validProject->GetNormalizedWindowState();
         // store only the normal rectangle because the itemized rectangle
         // makes no sense for an opening project window
         gPrefs->Write(wxT("/Window/X"), normalRect.GetX());
         gPrefs->Write(wxT("/Window/Y"), normalRect.GetY());
         gPrefs->Write(wxT("/Window/Width"), normalRect.GetWidth());
         gPrefs->Write(wxT("/Window/Height"), normalRect.GetHeight());
         gPrefs->Write(wxT("/Window/Maximized"), wndMaximized);
         gPrefs->Write(wxT("/Window/Normal_X"), normalRect.GetX());
         gPrefs->Write(wxT("/Window/Normal_Y"), normalRect.GetY());
         gPrefs->Write(wxT("/Window/Normal_Width"), normalRect.GetWidth());
         gPrefs->Write(wxT("/Window/Normal_Height"), normalRect.GetHeight());
         gPrefs->Write(wxT("/Window/Iconized"), TRUE);
      }
      else {
         // this would be a very strange case that might possibly occur on the Mac
         // Audacity would have to be running with no projects open
         // in this case we are going to write only the default values
         wxRect defWndRect;
         GetDefaultWindowRect(&defWndRect);
         gPrefs->Write(wxT("/Window/X"), defWndRect.GetX());
         gPrefs->Write(wxT("/Window/Y"), defWndRect.GetY());
         gPrefs->Write(wxT("/Window/Width"), defWndRect.GetWidth());
         gPrefs->Write(wxT("/Window/Height"), defWndRect.GetHeight());
         gPrefs->Write(wxT("/Window/Maximized"), FALSE);
         gPrefs->Write(wxT("/Window/Normal_X"), defWndRect.GetX());
         gPrefs->Write(wxT("/Window/Normal_Y"), defWndRect.GetY());
         gPrefs->Write(wxT("/Window/Normal_Width"), defWndRect.GetWidth());
         gPrefs->Write(wxT("/Window/Normal_Height"), defWndRect.GetHeight());
         gPrefs->Write(wxT("/Window/Iconized"), FALSE);
      }
   }
   gPrefs->Flush();
   sbWindowRectAlreadySaved = true;
}

#if wxUSE_DRAG_AND_DROP
class FileObject final : public wxFileDataObject
{
public:
   FileObject()
   {
   }

   bool IsSupportedFormat(const wxDataFormat & format, Direction WXUNUSED(dir = Get)) const
      // PRL:  This function does NOT override any inherited virtual!  What does it do?
   {
      if (format.GetType() == wxDF_FILENAME) {
         return true;
      }

#if defined(__WXMAC__)
#if !wxCHECK_VERSION(3, 0, 0)
      if (format.GetFormatId() == kDragPromisedFlavorFindFile) {
         return true;
      }
#endif
#endif

      return false;
   }
};

class DropTarget final : public wxFileDropTarget
{
public:
   DropTarget(AudacityProject *proj)
   {
      mProject = proj;

      // SetDataObject takes ownership
      SetDataObject(safenew FileObject());
   }

   ~DropTarget()
   {
   }

#if defined(__WXMAC__)
#if !wxCHECK_VERSION(3, 0, 0)
   bool GetData() override
   {
      bool foundSupported = false;
      bool firstFileAdded = false;
      OSErr result;

      UInt16 items = 0;
      CountDragItems((DragReference)m_currentDrag, &items);

      for (UInt16 index = 1; index <= items; index++) {

         DragItemRef theItem = 0;
         GetDragItemReferenceNumber((DragReference)m_currentDrag, index, &theItem);

         UInt16 flavors = 0;
         CountDragItemFlavors((DragReference)m_currentDrag, theItem , &flavors ) ;

         for (UInt16 flavor = 1 ;flavor <= flavors; flavor++) {

            FlavorType theType = 0;
            result = GetFlavorType((DragReference)m_currentDrag, theItem, flavor, &theType);
            if (theType != kDragPromisedFlavorFindFile && theType != kDragFlavorTypeHFS) {
               continue;
            }
            foundSupported = true;

            Size dataSize = 0;
            GetFlavorDataSize((DragReference)m_currentDrag, theItem, theType, &dataSize);

            ArrayOf<char> theData{ dataSize };
            GetFlavorData((DragReference)m_currentDrag, theItem, theType, (void*) theData.get(), &dataSize, 0L);

            wxString name;
            if (theType == kDragPromisedFlavorFindFile) {
               name = wxMacFSSpec2MacFilename((FSSpec *)theData.get());
            }
            else if (theType == kDragFlavorTypeHFS) {
               name = wxMacFSSpec2MacFilename(&((HFSFlavor *)theData.get())->fileSpec);
            }

            if (!firstFileAdded) {
               // reset file list
               ((wxFileDataObject*)GetDataObject())->SetData(0, "");
               firstFileAdded = true;
            }

            ((wxFileDataObject*)GetDataObject())->AddFile(name);

            // We only want to process one flavor
            break;
         }
      }
      return foundSupported;
   }
#endif

   bool OnDrop(wxCoord x, wxCoord y) override
   {
      // bool foundSupported = false;
#if !wxCHECK_VERSION(3, 0, 0)
      bool firstFileAdded = false;
      OSErr result;

      UInt16 items = 0;
      CountDragItems((DragReference)m_currentDrag, &items);

      for (UInt16 index = 1; index <= items; index++) {

         DragItemRef theItem = 0;
         GetDragItemReferenceNumber((DragReference)m_currentDrag, index, &theItem);

         UInt16 flavors = 0;
         CountDragItemFlavors((DragReference)m_currentDrag, theItem , &flavors ) ;

         for (UInt16 flavor = 1 ;flavor <= flavors; flavor++) {

            FlavorType theType = 0;
            result = GetFlavorType((DragReference)m_currentDrag, theItem, flavor, &theType);
            if (theType != kDragPromisedFlavorFindFile && theType != kDragFlavorTypeHFS) {
               continue;
            }
            return true;
         }
      }
#endif
      return CurrentDragHasSupportedFormat();
   }

#endif

   bool OnDropFiles(wxCoord WXUNUSED(x), wxCoord WXUNUSED(y), const wxArrayString& filenames) override
   {
      // Experiment shows that this function can be reached while there is no
      // catch block above in wxWidgets.  So stop all exceptions here.
      return GuardedCall< bool > ( [&] {
         //sort by OD non OD.  load Non OD first so user can start editing asap.
         wxArrayString sortednames(filenames);
         sortednames.Sort(CompareNoCaseFileName);

         ODManager::Pauser pauser;

         auto cleanup = finally( [&] {
            ProjectWindow::Get( *mProject ).HandleResize(); // Adjust scrollers for NEW track sizes.
         } );

         for (const auto &name : sortednames) {
#ifdef USE_MIDI
            if (Importer::IsMidi(name))
               FileActions::DoImportMIDI(mProject, name);
            else
#endif
               ProjectManager::Get( *mProject ).Import(name);
         }

         auto &window = ProjectWindow::Get( *mProject );
         window.ZoomAfterImport(nullptr);

         return true;
      } );
   }

private:
   AudacityProject *mProject;
};

#endif

AudacityProject *ProjectManager::New()
{
   wxRect wndRect;
   bool bMaximized = false;
   bool bIconized = false;
   GetNextWindowPlacement(&wndRect, &bMaximized, &bIconized);
   
   // Create and show a NEW project
   // Use a non-default deleter in the smart pointer!
   auto sp = std::make_shared< AudacityProject >();
   AllProjects{}.Add( sp );
   auto p = sp.get();
   auto &project = *p;
   auto &projectManager = Get( project );
   auto &window = ProjectWindow::Get( *p );
   window.Init();

   ControlToolBar::Get( project ).SetFocus();

   MissingAliasFilesDialog::SetShouldShow(true);
   MenuManager::Get( project ).CreateMenusAndCommands( project );
   
   projectManager.InitialState();
   projectManager.RestartTimer();
   
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
   
   //Initialise the Listeners
   gAudioIO->SetListener( &projectManager );
   SelectionBar::Get( project ).SetListener( &projectManager );
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   SpectralSelectionBar::Get( project ).SetListener( &projectManager );
#endif
   
#if wxUSE_DRAG_AND_DROP
   // We can import now, so become a drag target
   //   SetDropTarget(safenew AudacityDropTarget(this));
   //   mTrackPanel->SetDropTarget(safenew AudacityDropTarget(this));
   
   // SetDropTarget takes ownership
   TrackPanel::Get( project ).SetDropTarget( safenew DropTarget( &project ) );
#endif
   
   //Set the NEW project as active:
   SetActiveProject(p);
   
   // Okay, GetActiveProject() is ready. Now we can get its CommandManager,
   // and add the shortcut keys to the tooltips.
   ToolManager::Get( *p ).RegenerateTooltips();
   
   ModuleManager::Get().Dispatch(ProjectInitialized);
   
   window.Show(true);
   
   return p;
}

AudioIOStartStreamOptions
DefaultPlayOptions( AudacityProject &project )
{
   auto &projectAudioIO = ProjectAudioIO::Get( project );
   AudioIOStartStreamOptions options { &project,
      ProjectSettings::Get( project ).GetRate() };
   options.captureMeter = projectAudioIO.GetCaptureMeter();
   options.playbackMeter = projectAudioIO.GetPlaybackMeter();
   options.timeTrack = TrackList::Get( project ).GetTimeTrack();
   options.listener = &ProjectManager::Get( project );
   return options;
}

AudioIOStartStreamOptions
DefaultSpeedPlayOptions( AudacityProject &project )
{
   auto &projectAudioIO = ProjectAudioIO::Get( project );
   auto PlayAtSpeedRate = gAudioIO->GetBestRate(
      false,     //not capturing
      true,      //is playing
      ProjectSettings::Get( project ).GetRate()  //suggested rate
   );
   AudioIOStartStreamOptions options{ &project, PlayAtSpeedRate };
   options.captureMeter = projectAudioIO.GetCaptureMeter();
   options.playbackMeter = projectAudioIO.GetPlaybackMeter();
   options.timeTrack = TrackList::Get( project ).GetTimeTrack();
   options.listener = &ProjectManager::Get( project );
   return options;
}

bool ProjectManager::SnapSelection()
{
   auto &project = mProject;
   auto &settings = ProjectSettings::Get( project );
   auto &window = ProjectWindow::Get( project );
   auto snapTo = settings.GetSnapTo();
   if (snapTo != SNAP_OFF) {
      auto &viewInfo = ViewInfo::Get( project );
      SelectedRegion &selectedRegion = viewInfo.selectedRegion;
      NumericConverter nc(NumericConverter::TIME,
         settings.GetSelectionFormat(), 0, settings.GetRate());
      const bool nearest = (snapTo == SNAP_NEAREST);

      const double oldt0 = selectedRegion.t0();
      const double oldt1 = selectedRegion.t1();

      nc.ValueToControls(oldt0, nearest);
      nc.ControlsToValue();
      const double t0 = nc.GetValue();

      nc.ValueToControls(oldt1, nearest);
      nc.ControlsToValue();
      const double t1 = nc.GetValue();

      if (t0 != oldt0 || t1 != oldt1) {
         selectedRegion.setTimes(t0, t1);
         window.TP_DisplaySelection();
         return true;
      }
   }

   return false;
}

double ProjectManager::AS_GetRate()
{
   auto &project = mProject;
   auto &settings = ProjectSettings::Get( project );
   return settings.GetRate();
}

// Typically this came from the SelectionToolbar and does not need to 
// be communicated back to it.
void ProjectManager::AS_SetRate(double rate)
{
   auto &project = mProject;
   auto &settings = ProjectSettings::Get( project );
   settings.SetRate( rate );
}

int ProjectManager::AS_GetSnapTo()
{
   auto &project = mProject;
   auto &settings = ProjectSettings::Get( project );
   return settings.GetSnapTo();
}

void ProjectManager::AS_SetSnapTo(int snap)
{
   auto &project = mProject;
   auto &settings = ProjectSettings::Get( project );
   auto &window = ProjectWindow::Get( project );

   settings.SetSnapTo( snap );

// LLL: TODO - what should this be changed to???
// GetCommandManager()->Check(wxT("Snap"), mSnapTo);
   gPrefs->Write(wxT("/SnapTo"), snap);
   gPrefs->Flush();

   SnapSelection();

   window.RedrawProject();

   SelectionBar::Get( project ).SetSnapTo(snap);
}

const NumericFormatSymbol & ProjectManager::AS_GetSelectionFormat()
{
   auto &project = mProject;
   auto &settings = ProjectSettings::Get( project );
   return settings.GetSelectionFormat();
}

void ProjectManager::AS_SetSelectionFormat(const NumericFormatSymbol & format)
{
   auto &project = mProject;
   auto &settings = ProjectSettings::Get( project );
   settings.SetSelectionFormat( format );

   gPrefs->Write(wxT("/SelectionFormat"), format.Internal());
   gPrefs->Flush();

   if (SnapSelection())
      TrackPanel::Get( project ).Refresh(false);

   SelectionBar::Get( project ).SetSelectionFormat(format);
}

void ProjectManager::AS_ModifySelection(double &start, double &end, bool done)
{
   auto &project = mProject;
   auto &trackPanel = TrackPanel::Get( project );
   auto &viewInfo = ViewInfo::Get( project );
   viewInfo.selectedRegion.setTimes(start, end);
   trackPanel.Refresh(false);
   if (done) {
      ModifyState(false);
   }
}

double ProjectManager::SSBL_GetRate() const
{
   auto &project = mProject;
   auto &settings = ProjectSettings::Get( project );
   auto &tracks = TrackList::Get( project );
   // Return maximum of project rate and all track rates.
   return std::max( settings.GetRate(),
      tracks.Any<const WaveTrack>().max( &WaveTrack::GetRate ) );
}

const NumericFormatSymbol & ProjectManager::SSBL_GetFrequencySelectionFormatName()
{
   auto &project = mProject;
   auto &settings = ProjectSettings::Get( project );
   return settings.GetFrequencySelectionFormatName();
}

void ProjectManager::SSBL_SetFrequencySelectionFormatName(
   const NumericFormatSymbol & formatName)
{
   auto &project = mProject;
   auto &settings = ProjectSettings::Get( project );

   settings.SetFrequencySelectionFormatName( formatName );

   gPrefs->Write(wxT("/FrequencySelectionFormatName"),
                 formatName.Internal());
   gPrefs->Flush();

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   SpectralSelectionBar::Get( project ).SetFrequencySelectionFormatName(formatName);
#endif
}

const NumericFormatSymbol &
ProjectManager::SSBL_GetBandwidthSelectionFormatName()
{
   auto &project = mProject;
   auto &settings = ProjectSettings::Get( project );
   return settings.GetBandwidthSelectionFormatName();
}

void ProjectManager::SSBL_SetBandwidthSelectionFormatName(
   const NumericFormatSymbol & formatName)
{
   auto &project = mProject;
   auto &settings = ProjectSettings::Get( project );

   settings.SetBandwidthSelectionFormatName( formatName );

   gPrefs->Write(wxT("/BandwidthSelectionFormatName"),
      formatName.Internal());
   gPrefs->Flush();

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   SpectralSelectionBar::Get( project ).SetBandwidthSelectionFormatName(formatName);
#endif
}

void ProjectManager::SSBL_ModifySpectralSelection(
   double &bottom, double &top, bool done)
{
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   auto &project = mProject;
   auto &trackPanel = TrackPanel::Get( project );
   auto &viewInfo = ViewInfo::Get( project );

   double nyq = SSBL_GetRate() / 2.0;
   if (bottom >= 0.0)
      bottom = std::min(nyq, bottom);
   if (top >= 0.0)
      top = std::min(nyq, top);
   viewInfo.selectedRegion.setFrequencies(bottom, top);
   trackPanel.Refresh(false);
   if (done) {
      ModifyState(false);
   }
#else
   bottom; top; done;
#endif
}

// LL: All objects that have a reference to the DirManager should
//     be deleted before the final mDirManager->Deref() in this
//     routine.  Failing to do so can cause unwanted recursion
//     and/or attempts to DELETE objects twice.
void ProjectManager::OnCloseWindow(wxCloseEvent & event)
{
   auto &project = mProject;
   auto &projectFileIO = ProjectFileIO::Get( project );
   const auto &settings = ProjectSettings::Get( project );
   auto &projectAudioIO = ProjectAudioIO::Get( project );
   auto &tracks = TrackList::Get( project );
   auto &window = ProjectWindow::Get( project );

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
      wxCommandEvent dummyEvent;
      ControlToolBar::Get( project ).OnStop(dummyEvent);

      window.FixScrollbars();
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
   if (event.CanVeto() && (settings.EmptyCanBeDirty() || bHasTracks)) {
      if ( UndoManager::Get( project ).UnsavedChanges() ) {
         TitleRestorer Restorer( window, project );// RAII
         /* i18n-hint: The first %s numbers the project, the second %s is the project name.*/
         wxString Title =  wxString::Format(_("%sSave changes to %s?"), Restorer.sProjNumber, Restorer.sProjName);
         wxString Message = _("Save project before closing?");
         if( !bHasTracks )
         {
          Message += _("\nIf saved, the project will have no tracks.\n\nTo save any previously open tracks:\nCancel, Edit > Undo until all tracks\nare open, then File > Save Project.");
         }
         int result = AudacityMessageBox( Message,
                                    Title,
                                   wxYES_NO | wxCANCEL | wxICON_QUESTION,
                                   &window);

         if (result == wxCANCEL || (result == wxYES &&
              !GuardedCall<bool>( [&]{ return projectFileIO.Save(); } )
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

   ModuleManager::Get().Dispatch(ProjectClosing);

   // Stop the timer since there's no need to update anything anymore
   mTimer.reset();

   // The project is now either saved or the user doesn't want to save it,
   // so there's no need to keep auto save info around anymore
   projectFileIO.DeleteCurrentAutoSaveFile();

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
   quitOnClose = !mMenuClose;
#endif

   // DanH: If we're definitely about to quit, clear the clipboard.
   //       Doing this after Deref'ing the DirManager causes problems.
   if ((AllProjects{}.size() == 1) &&
      (quitOnClose || AllProjects::Closing()))
      Clipboard::Get().Clear();

   // JKC: For Win98 and Linux do not detach the menu bar.
   // We want wxWidgets to clean it up for us.
   // TODO: Is there a Mac issue here??
   // SetMenuBar(NULL);

   projectFileIO.CloseLock();

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

   TrackFactory::Destroy( project );

   // Delete all the tracks to free up memory and DirManager references.
   tracks.Clear();

   // This must be done before the following Deref() since it holds
   // references to the DirManager.
   UndoManager::Get( project ).ClearStates();

   // MM: Tell the DirManager it can now DELETE itself
   // if it finds it is no longer needed. If it is still
   // used (f.e. by the clipboard), it will recognize this
   // and will destroy itself later.
   //
   // LL: All objects with references to the DirManager should
   //     have been deleted before this.
   DirManager::Destroy( project );

   // Remove self from the global array, but defer destruction of self
   auto pSelf = AllProjects{}.Remove( project );
   wxASSERT( pSelf );

   if (GetActiveProject() == &project) {
      // Find a NEW active project
      if ( !AllProjects{}.empty() ) {
         SetActiveProject(AllProjects{}.begin()->get());
      }
      else {
         SetActiveProject(NULL);
      }
   }

   // Since we're going to be destroyed, make sure we're not to
   // receive audio notifications anymore.
   if (gAudioIO->GetListener() == this) {
      auto active = GetActiveProject();
      gAudioIO->SetListener(
         active ? &ProjectManager::Get( *active ) : nullptr
      );
   }

   if (AllProjects{}.empty() && !AllProjects::Closing()) {

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

// PRL: I preserve this handler function for an event that was never sent, but
// I don't know the intention.

void ProjectManager::OnOpenAudioFile(wxCommandEvent & event)
{
   auto &project = mProject;
   auto &window = GetProjectFrame( project );
   const wxString &cmd = event.GetString();

   if (!cmd.empty())
      OpenFile(cmd);

   window.RequestUserAttention();
}

// static method, can be called outside of a project
wxArrayString ProjectManager::ShowOpenDialog(const wxString &extraformat, const wxString &extrafilter)
{
   FormatList l;
   wxString filter;  ///< List of file format names and extensions, separated
   /// by | characters between _formats_ and extensions for each _format_, i.e.
   /// format1name | *.ext | format2name | *.ex1;*.ex2
   wxString all;  ///< One long list of all supported file extensions,
   /// semicolon separated

   if (!extraformat.empty())
   {  // additional format specified
      all = extrafilter + wxT(';');
      // add it to the "all supported files" filter string
   }

   // Construct the filter
   Importer::Get().GetSupportedImportFormats(&l);

   for (const auto &format : l) {
      /* this loop runs once per supported _format_ */
      const Format *f = &format;

      wxString newfilter = f->formatName + wxT("|");
      // bung format name into string plus | separator
      for (size_t i = 0; i < f->formatExtensions.size(); i++) {
         /* this loop runs once per valid _file extension_ for file containing
          * the current _format_ */
         if (!newfilter.Contains(wxT("*.") + f->formatExtensions[i] + wxT(";")))
            newfilter += wxT("*.") + f->formatExtensions[i] + wxT(";");
         if (!all.Contains(wxT("*.") + f->formatExtensions[i] + wxT(";")))
            all += wxT("*.") + f->formatExtensions[i] + wxT(";");
      }
      newfilter.RemoveLast(1);
      filter += newfilter;
      filter += wxT("|");
   }
   all.RemoveLast(1);
   filter.RemoveLast(1);

   // For testing long filters
#if 0
   wxString test = wxT("*.aaa;*.bbb;*.ccc;*.ddd;*.eee");
   all = test + wxT(';') + test + wxT(';') + test + wxT(';') +
         test + wxT(';') + test + wxT(';') + test + wxT(';') +
         test + wxT(';') + test + wxT(';') + test + wxT(';') +
         all;
#endif

   /* i18n-hint: The vertical bars and * are essential here.*/
   wxString mask = _("All files|*|All supported files|") +
                   all + wxT("|"); // "all" and "all supported" entries
   if (!extraformat.empty())
   {  // append caller-defined format if supplied
      mask +=  extraformat + wxT("|") + extrafilter + wxT("|");
   }
   mask += filter;   // put the names and extensions of all the importer formats
   // we built up earlier into the mask

   // Retrieve saved path and type
   auto path = FileNames::FindDefaultPath(FileNames::Operation::Open);
   wxString type = gPrefs->Read(wxT("/DefaultOpenType"),mask.BeforeFirst(wxT('|')));

   // Convert the type to the filter index
   int index = mask.First(type + wxT("|"));
   if (index == wxNOT_FOUND) {
      index = 0;
   }
   else {
      index = mask.Left(index).Freq(wxT('|')) / 2;
      if (index < 0) {
         index = 0;
      }
   }

   // Construct and display the file dialog
   wxArrayString selected;

   FileDialogWrapper dlog(NULL,
                   _("Select one or more files"),
                   path,
                   wxT(""),
                   mask,
                   wxFD_OPEN | wxFD_MULTIPLE | wxRESIZE_BORDER);

   dlog.SetFilterIndex(index);

   int dialogResult = dlog.ShowModal();

   // Convert the filter index to type and save
   index = dlog.GetFilterIndex();
   for (int i = 0; i < index; i++) {
      mask = mask.AfterFirst(wxT('|')).AfterFirst(wxT('|'));
   }
   gPrefs->Write(wxT("/DefaultOpenType"), mask.BeforeFirst(wxT('|')));
   gPrefs->Write(wxT("/LastOpenType"), mask.BeforeFirst(wxT('|')));
   gPrefs->Flush();

   if (dialogResult == wxID_OK) {
      // Return the selected files
      dlog.GetPaths(selected);
   }
   return selected;
}

// static method, can be called outside of a project
bool ProjectManager::IsAlreadyOpen(const FilePath &projPathName)
{
   const wxFileName newProjPathName(projPathName);
   auto start = AllProjects{}.begin(), finish = AllProjects{}.end(),
   iter = std::find_if( start, finish,
      [&]( const AllProjects::value_type &ptr ){
         return newProjPathName.SameAs(wxFileNameWrapper{ ptr->GetFileName() });
      } );
   if (iter != finish) {
      wxString errMsg =
      wxString::Format(_("%s is already open in another window."),
                       newProjPathName.GetName());
      wxLogError(errMsg);
      AudacityMessageBox(errMsg, _("Error Opening Project"), wxOK | wxCENTRE);
      return true;
   }
   return false;
}

// static method, can be called outside of a project
void ProjectManager::OpenFiles(AudacityProject *proj)
{
   /* i18n-hint: This string is a label in the file type filter in the open
    * and save dialogues, for the option that only shows project files created
    * with Audacity. Do not include pipe symbols or .aup (this extension will
    * now be added automatically for the Save Projects dialogues).*/
   auto selectedFiles =
      ProjectManager::ShowOpenDialog(_("Audacity projects"), wxT("*.aup"));
   if (selectedFiles.size() == 0) {
      gPrefs->Write(wxT("/LastOpenType"),wxT(""));
      gPrefs->Flush();
      return;
   }

   //sort selected files by OD status.
   //For the open menu we load OD first so user can edit asap.
   //first sort selectedFiles.
   selectedFiles.Sort(CompareNoCaseFileName);
   ODManager::Pauser pauser;

   auto cleanup = finally( [] {
      gPrefs->Write(wxT("/LastOpenType"),wxT(""));
      gPrefs->Flush();
   } );

   for (size_t ff = 0; ff < selectedFiles.size(); ff++) {
      const wxString &fileName = selectedFiles[ff];

      // Make sure it isn't already open.
      if (IsAlreadyOpen(fileName))
         continue; // Skip ones that are already open.

      FileNames::UpdateDefaultPath(FileNames::Operation::Open, fileName);

      // DMM: If the project is dirty, that means it's been touched at
      // all, and it's not safe to open a NEW project directly in its
      // place.  Only if the project is brand-NEW clean and the user
      // hasn't done any action at all is it safe for Open to take place
      // inside the current project.
      //
      // If you try to Open a NEW project inside the current window when
      // there are no tracks, but there's an Undo history, etc, then
      // bad things can happen, including data files moving to the NEW
      // project directory, etc.
      if ( proj && (
         Get( *proj ).mDirty ||
         !TrackList::Get( *proj ).empty()
      ) )
         proj = nullptr;

      // This project is clean; it's never been touched.  Therefore
      // all relevant member variables are in their initial state,
      // and it's okay to open a NEW project inside this window.
      proj = OpenProject( proj, fileName );
   }
}

AudacityProject *ProjectManager::OpenProject(
   AudacityProject *pProject, const FilePath &fileNameArg, bool addtohistory)
{
   AudacityProject *pNewProject = nullptr;
   if ( ! pProject )
      pProject = pNewProject = New();
   auto cleanup = finally( [&] {
      if( pNewProject )
         GetProjectFrame( *pNewProject ).Close(true);
   } );
   Get( *pProject ).OpenFile( fileNameArg, addtohistory );
   pNewProject = nullptr;
   auto &projectFileIO = ProjectFileIO::Get( *pProject );
   if( projectFileIO.IsRecovered() )
      ProjectWindow::Get( *pProject ).Zoom(
         ViewActions::GetZoomOfToFit( *pProject ) );

   return pProject;
}

XMLTagHandler *
ProjectManager::RecordingRecoveryFactory( AudacityProject &project ) {
   auto &projectManager = Get( project );
   auto &ptr = projectManager.mRecordingRecoveryHandler;
   if (!ptr)
      ptr =
         std::make_unique<RecordingRecoveryHandler>( &project );
   return ptr.get();
}

ProjectFileIORegistry::Entry
ProjectManager::sRecoveryFactory{
   wxT("recordingrecovery"), RecordingRecoveryFactory
};

// XML handler for <import> tag
class ImportXMLTagHandler final : public XMLTagHandler
{
 public:
   ImportXMLTagHandler(AudacityProject* pProject) { mProject = pProject; }

   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs) override;
   XMLTagHandler *HandleXMLChild(const wxChar * WXUNUSED(tag))  override
      { return NULL; }

   // Don't want a WriteXML method because ImportXMLTagHandler is not a WaveTrack.
   // <import> tags are instead written by AudacityProject::WriteXML.
   //    void WriteXML(XMLWriter &xmlFile) /* not override */ { wxASSERT(false); }

 private:
   AudacityProject* mProject;
};

bool ImportXMLTagHandler::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   if (wxStrcmp(tag, wxT("import")) || attrs==NULL || (*attrs)==NULL || wxStrcmp(*attrs++, wxT("filename")))
       return false;
   wxString strAttr = *attrs;
   if (!XMLValueChecker::IsGoodPathName(strAttr))
   {
      // Maybe strAttr is just a fileName, not the full path. Try the project data directory.
      wxFileNameWrapper fileName{
         DirManager::Get( *mProject ).GetProjectDataDir(), strAttr };
      if (XMLValueChecker::IsGoodFileName(strAttr, fileName.GetPath(wxPATH_GET_VOLUME)))
         strAttr = fileName.GetFullPath();
      else
      {
         wxLogWarning(wxT("Could not import file: %s"), strAttr);
         return false;
      }
   }

   WaveTrackArray trackArray;

   // Guard this call so that C++ exceptions don't propagate through
   // the expat library
   GuardedCall(
      [&] {
         ProjectManager::Get( *mProject ).Import(strAttr, &trackArray); },
      [&] (AudacityException*) { trackArray.clear(); }
   );

   if (trackArray.empty())
      return false;

   // Handle other attributes, now that we have the tracks.
   attrs++;
   const wxChar** pAttr;
   bool bSuccess = true;

   for (size_t i = 0; i < trackArray.size(); i++)
   {
      // Most of the "import" tag attributes are the same as for "wavetrack" tags,
      // so apply them via WaveTrack::HandleXMLTag().
      bSuccess = trackArray[i]->HandleXMLTag(wxT("wavetrack"), attrs);

      // "offset" tag is ignored in WaveTrack::HandleXMLTag except for legacy projects,
      // so handle it here.
      double dblValue;
      pAttr = attrs;
      while (*pAttr)
      {
         const wxChar *attr = *pAttr++;
         const wxChar *value = *pAttr++;
         const wxString strValue = value;
         if (!wxStrcmp(attr, wxT("offset")) &&
               XMLValueChecker::IsGoodString(strValue) &&
               Internat::CompatibleToDouble(strValue, &dblValue))
            trackArray[i]->SetOffset(dblValue);
      }
   }
   return bSuccess;
};

XMLTagHandler *
ProjectManager::ImportHandlerFactory( AudacityProject &project ) {
   auto &projectManager = Get( project );
   auto &ptr = projectManager.mImportXMLTagHandler;
   if (!ptr)
      ptr =
         std::make_unique<ImportXMLTagHandler>( &project );
   return ptr.get();
}

ProjectFileIORegistry::Entry
ProjectManager::sImportHandlerFactory{
   wxT("import"), ImportHandlerFactory
};

// FIXME:? TRAP_ERR This should return a result that is checked.
//    See comment in AudacityApp::MRUOpen().
void ProjectManager::OpenFile(const FilePath &fileNameArg, bool addtohistory)
{
   auto &project = mProject;
   auto &projectFileIO = ProjectFileIO::Get( project );
   auto &tracks = TrackList::Get( project );
   auto &trackPanel = TrackPanel::Get( project );
   auto &dirManager = DirManager::Get( project );
   auto &window = ProjectWindow::Get( project );

   // On Win32, we may be given a short (DOS-compatible) file name on rare
   // occassions (e.g. stuff like "C:\PROGRA~1\AUDACI~1\PROJEC~1.AUP"). We
   // convert these to long file name first.
   auto fileName = PlatformCompatibility::ConvertSlashInFileName(
      PlatformCompatibility::GetLongFileName(fileNameArg));

   // Make sure it isn't already open.
   // Vaughan, 2011-03-25: This was done previously in AudacityProject::OpenFiles()
   //    and AudacityApp::MRUOpen(), but if you open an aup file by double-clicking it
   //    from, e.g., Win Explorer, it would bypass those, get to here with no check,
   //    then open a NEW project from the same data with no warning.
   //    This was reported in http://bugzilla.audacityteam.org/show_bug.cgi?id=137#c17,
   //    but is not really part of that bug. Anyway, prevent it!
   if (IsAlreadyOpen(fileName))
      return;


   // Data loss may occur if users mistakenly try to open ".aup.bak" files
   // left over from an unsuccessful save or by previous versions of Audacity.
   // So we always refuse to open such files.
   if (fileName.Lower().EndsWith(wxT(".aup.bak")))
   {
      AudacityMessageBox(
         _("You are trying to open an automatically created backup file.\nDoing this may result in severe data loss.\n\nPlease open the actual Audacity project file instead."),
         _("Warning - Backup File Detected"),
         wxOK | wxCENTRE, &window);
      return;
   }

   if (!::wxFileExists(fileName)) {
      AudacityMessageBox(
         wxString::Format( _("Could not open file: %s"), fileName ),
         ("Error Opening File"),
         wxOK | wxCENTRE, &window);
      return;
   }

   // We want to open projects using wxTextFile, but if it's NOT a project
   // file (but actually a WAV file, for example), then wxTextFile will spin
   // for a long time searching for line breaks.  So, we look for our
   // signature at the beginning of the file first:

   char buf[16];
   {
      wxFFile ff(fileName, wxT("rb"));
      if (!ff.IsOpened()) {
         AudacityMessageBox(
            wxString::Format( _("Could not open file: %s"), fileName ),
            _("Error opening file"),
            wxOK | wxCENTRE, &window);
         return;
      }
      int numRead = ff.Read(buf, 15);
      if (numRead != 15) {
         AudacityMessageBox(wxString::Format(_("File may be invalid or corrupted: \n%s"),
            fileName), _("Error Opening File or Project"),
            wxOK | wxCENTRE, &window);
         ff.Close();
         return;
      }
      buf[15] = 0;
   }

   wxString temp = LAT1CTOWX(buf);

   if (temp == wxT("AudacityProject")) {
      // It's an Audacity 1.0 (or earlier) project file.
      // If they bail out, return and do no more.
      if( !projectFileIO.WarnOfLegacyFile() )
         return;
      // Convert to the NEW format.
      bool success = ConvertLegacyProjectFile(wxFileName{ fileName });
      if (!success) {
         AudacityMessageBox(_("Audacity was unable to convert an Audacity 1.0 project to the new project format."),
                      _("Error Opening Project"),
                      wxOK | wxCENTRE, &window);
         return;
      }
      else {
         temp = wxT("<?xml ");
      }
   }

   // FIXME: //v Surely we could be smarter about this, like checking much earlier that this is a .aup file.
   if (temp.Mid(0, 6) != wxT("<?xml ")) {
      // If it's not XML, try opening it as any other form of audio

#ifdef EXPERIMENTAL_DRAG_DROP_PLUG_INS
      // Is it a plug-in?
      if (PluginManager::Get().DropFile(fileName)) {
         MenuCreator::RebuildAllMenuBars();
      }
      else
      // No, so import.
#endif

      {
#ifdef USE_MIDI
         if (Importer::IsMidi(fileName))
            FileActions::DoImportMIDI( &project, fileName );
         else
#endif
            Import( fileName );

         window.ZoomAfterImport(nullptr);
      }

      return;
   }

   // The handlers may be created during ReadProjectFile and are not needed
   // after this function exits.
   auto cleanupHandlers = finally( [this]{
      mImportXMLTagHandler.reset();
      mRecordingRecoveryHandler.reset();
   } );

   auto results = projectFileIO.ReadProjectFile( fileName );

   if ( results.decodeError )
      return;

   const bool bParseSuccess = results.parseSuccess;
   const wxString &errorStr = results.errorString;
   const bool err = results.trackError;

   if (bParseSuccess) {
      auto &settings = ProjectSettings::Get( project );
      AS_SetSnapTo(settings.GetSnapTo());
      AS_SetSelectionFormat(settings.GetSelectionFormat());
      SSBL_SetFrequencySelectionFormatName(
         settings.GetFrequencySelectionFormatName());
      SSBL_SetBandwidthSelectionFormatName(
         settings.GetBandwidthSelectionFormatName());

      InitialState();
      trackPanel.SetFocusedTrack( *tracks.Any().begin() );
      window.HandleResize();
      trackPanel.Refresh(false);
      trackPanel.Update(); // force any repaint to happen now,
      // else any asynch calls into the blockfile code will not have
      // finished logging errors (if any) before the call to ProjectFSCK()

      if (addtohistory)
         FileHistory::Global().AddFileToHistory(fileName);
   }

   // Use a finally block here, because there are calls to Save() below which
   // might throw.
   bool closed = false;
   auto cleanup = finally( [&] {
      //release the flag.
      ODManager::UnmarkLoadedODFlag();

      if (! closed ) {
         if ( bParseSuccess ) {
            // This is a no-fail:
            dirManager.FillBlockfilesCache();
            projectFileIO.EnqueueODTasks();
         }

         // For an unknown reason, OSX requires that the project window be
         // raised if a recovery took place.
         window.CallAfter( [&] { window.Raise(); } );
      }
   } );
   
   if (bParseSuccess) {
      bool saved = false;

      if (projectFileIO.IsRecovered())
      {
         // This project has been recovered, so write a NEW auto-save file
         // now and then DELETE the old one in the auto-save folder. Note that
         // at this point mFileName != fileName, because when opening a
         // recovered file mFileName is faked to point to the original file
         // which has been recovered, not the one in the auto-save folder.
         ::ProjectFSCK(dirManager, err, true); // Correct problems in auto-recover mode.

         // PushState calls AutoSave(), so no longer need to do so here.
         PushState(_("Project was recovered"), _("Recover"));

         if (!wxRemoveFile(fileName))
            AudacityMessageBox(_("Could not remove old auto save file"),
                         _("Error"), wxICON_STOP, &window);
      }
      else
      {
         // This is a regular project, check it and ask user
         int status = ::ProjectFSCK(dirManager, err, false);
         if (status & FSCKstatus_CLOSE_REQ)
         {
            // Vaughan, 2010-08-23: Note this did not do a real close.
            // It could cause problems if you get this, say on missing alias files,
            // then try to open a project with, e.g., missing blockfiles.
            // It then failed in SetProject, saying it cannot find the files,
            // then never go through ProjectFSCK to give more info.
            // Going through OnClose() may be overkill, but it's safe.
            /*
               // There was an error in the load/check and the user
               // explictly opted to close the project.
               mTracks->Clear(true);
               mFileName = wxT("");
               SetProjectTitle();
               mTrackPanel->Refresh(true);
               */
            closed = true;
            SetMenuClose(true);
            window.Close();
            return;
         }
         else if (status & FSCKstatus_CHANGED)
         {
            // Mark the wave tracks as changed and redraw.
            for ( auto wt : tracks.Any<WaveTrack>() )
               // Only wave tracks have a notion of "changed".
               for (const auto &clip: wt->GetClips())
                  clip->MarkChanged();

            trackPanel.Refresh(true);

            // Vaughan, 2010-08-20: This was bogus, as all the actions in ProjectFSCK
            // that return FSCKstatus_CHANGED cannot be undone.
            //    this->PushState(_("Project checker repaired file"), _("Project Repair"));

            if (status & FSCKstatus_SAVE_AUP)
               projectFileIO.Save(), saved = true;
         }
      }

      if (mImportXMLTagHandler) {
         if (!saved)
            // We processed an <import> tag, so save it as a normal project,
            // with no <import> tags.
            projectFileIO.Save();
      }
   }
   else {
      // Vaughan, 2011-10-30:
      // See first topic at http://bugzilla.audacityteam.org/show_bug.cgi?id=451#c16.
      // Calling mTracks->Clear() with deleteTracks true results in data loss.

      // PRL 2014-12-19:
      // I made many changes for wave track memory management, but only now
      // read the above comment.  I may have invalidated the fix above (which
      // may have spared the files at the expense of leaked memory).  But
      // here is a better way to accomplish the intent, doing like what happens
      // when the project closes:
      for ( auto pTrack : tracks.Any< WaveTrack >() )
         pTrack->CloseLock();

      tracks.Clear(); //tracks.Clear(true);

      project.SetFileName( wxT("") );
      projectFileIO.SetProjectTitle();

      wxLogError(wxT("Could not parse file \"%s\". \nError: %s"), fileName, errorStr);

      wxString url = wxT("FAQ:Errors_on_opening_or_recovering_an_Audacity_project");

      // Certain errors have dedicated help.
      // On April-4th-2018, we did not request translation of the XML errors.
      // If/when we do, we will need _() around the comparison strings.
      if( errorStr.Contains( ("not well-formed (invalid token)") ) )
         url = "Error:_not_well-formed_(invalid_token)_at_line_x";
      else if( errorStr.Contains(("reference to invalid character number") ))
         url = "Error_Opening_Project:_Reference_to_invalid_character_number_at_line_x";
      else if( errorStr.Contains(("mismatched tag") ))
         url += "#mismatched";

// These two errors with FAQ entries are reported elsewhere, not here....
//#[[#import-error|Error Importing: Aup is an Audacity Project file. Use the File > Open command]]
//#[[#corrupt|Error Opening File or Project: File may be invalid or corrupted]]

// If we did want to handle every single parse error, these are they....
/*
    XML_L("out of memory"),
    XML_L("syntax error"),
    XML_L("no element found"),
    XML_L("not well-formed (invalid token)"),
    XML_L("unclosed token"),
    XML_L("partial character"),
    XML_L("mismatched tag"),
    XML_L("duplicate attribute"),
    XML_L("junk after document element"),
    XML_L("illegal parameter entity reference"),
    XML_L("undefined entity"),
    XML_L("recursive entity reference"),
    XML_L("asynchronous entity"),
    XML_L("reference to invalid character number"),
    XML_L("reference to binary entity"),
    XML_L("reference to external entity in attribute"),
    XML_L("XML or text declaration not at start of entity"),
    XML_L("unknown encoding"),
    XML_L("encoding specified in XML declaration is incorrect"),
    XML_L("unclosed CDATA section"),
    XML_L("error in processing external entity reference"),
    XML_L("document is not standalone"),
    XML_L("unexpected parser state - please send a bug report"),
    XML_L("entity declared in parameter entity"),
    XML_L("requested feature requires XML_DTD support in Expat"),
    XML_L("cannot change setting once parsing has begun"),
    XML_L("unbound prefix"),
    XML_L("must not undeclare prefix"),
    XML_L("incomplete markup in parameter entity"),
    XML_L("XML declaration not well-formed"),
    XML_L("text declaration not well-formed"),
    XML_L("illegal character(s) in public id"),
    XML_L("parser suspended"),
    XML_L("parser not suspended"),
    XML_L("parsing aborted"),
    XML_L("parsing finished"),
    XML_L("cannot suspend in external parameter entity"),
    XML_L("reserved prefix (xml) must not be undeclared or bound to another namespace name"),
    XML_L("reserved prefix (xmlns) must not be declared or undeclared"),
    XML_L("prefix must not be bound to one of the reserved namespace names")
*/

      ShowErrorDialog(
         &window,
         _("Error Opening Project"),
         errorStr,
         url);
   }
}

std::vector< std::shared_ptr< Track > >
ProjectManager::AddImportedTracks(const FilePath &fileName,
                                   TrackHolders &&newTracks)
{
   auto &project = mProject;
   auto &projectFileIO = ProjectFileIO::Get( project );
   auto &tracks = TrackList::Get( project );

   std::vector< std::shared_ptr< Track > > results;

   SelectActions::SelectNone( project );

   bool initiallyEmpty = tracks.empty();
   double newRate = 0;
   wxString trackNameBase = fileName.AfterLast(wxFILE_SEP_PATH).BeforeLast('.');
   int i = -1;

   // Must add all tracks first (before using Track::IsLeader)
   for (auto &group : newTracks) {
      if (group.empty()) {
         wxASSERT(false);
         continue;
      }
      auto first = group.begin()->get();
      auto nChannels = group.size();
      for (auto &uNewTrack : group) {
         auto newTrack = tracks.Add( uNewTrack );
         results.push_back(newTrack->SharedPointer());
      }
      tracks.GroupChannels(*first, nChannels);
   }
   newTracks.clear();
      
   // Now name them

   // Add numbers to track names only if there is more than one (mono or stereo)
   // track (not necessarily, more than one channel)
   const bool useSuffix =
      make_iterator_range( results.begin() + 1, results.end() )
         .any_of( []( decltype(*results.begin()) &pTrack )
            { return pTrack->IsLeader(); } );

   for (const auto &newTrack : results) {
      if ( newTrack->IsLeader() )
         // Count groups only
         ++i;

      newTrack->SetSelected(true);

      if ( useSuffix )
         newTrack->SetName(trackNameBase + wxString::Format(wxT(" %d" ), i + 1));
      else
         newTrack->SetName(trackNameBase);

      newTrack->TypeSwitch( [&](WaveTrack *wt) {
         if (newRate == 0)
            newRate = wt->GetRate();

         // Check if NEW track contains aliased blockfiles and if yes,
         // remember this to show a warning later
         if(WaveClip* clip = wt->GetClipByIndex(0)) {
            BlockArray &blocks = clip->GetSequence()->GetBlockArray();
            if (blocks.size())
            {
               SeqBlock& block = blocks[0];
               if (block.f->IsAlias())
               {
                  projectFileIO.SetImportedDependencies( true );
               }
            }
         }
      });
   }

   // Automatically assign rate of imported file to whole project,
   // if this is the first file that is imported
   if (initiallyEmpty && newRate > 0) {
      auto &settings = ProjectSettings::Get( project );
      settings.SetRate( newRate );
      SelectionBar::Get( project ).SetRate( newRate );
   }

   PushState(wxString::Format(_("Imported '%s'"), fileName),
       _("Import"));

#if defined(__WXGTK__)
   // See bug #1224
   // The track panel hasn't we been fully created, so the DoZoomFit() will not give
   // expected results due to a window width of zero.  Should be safe to yield here to
   // allow the creattion to complete.  If this becomes a problem, it "might" be possible
   // to queue a dummy event to trigger the DoZoomFit().
   wxEventLoopBase::GetActive()->YieldFor(wxEVT_CATEGORY_UI | wxEVT_CATEGORY_USER_INPUT);
#endif

   if (initiallyEmpty && !projectFileIO.IsProjectSaved() ) {
      wxString name = fileName.AfterLast(wxFILE_SEP_PATH).BeforeLast(wxT('.'));
      project.SetFileName(
         ::wxPathOnly(fileName) + wxFILE_SEP_PATH + name + wxT(".aup") );
      projectFileIO.SetLoadedFromAup( false );
      projectFileIO.SetProjectTitle();
   }

   // Moved this call to higher levels to prevent flicker redrawing everything on each file.
   //   HandleResize();

   return results;
}

// If pNewTrackList is passed in non-NULL, it gets filled with the pointers to NEW tracks.
bool ProjectManager::Import(
   const FilePath &fileName, WaveTrackArray* pTrackArray /*= NULL*/)
{
   auto &project = mProject;
   auto &dirManager = DirManager::Get( project );
   auto oldTags = Tags::Get( project ).shared_from_this();
   TrackHolders newTracks;
   wxString errorMessage;

   {
      // Backup Tags, before the import.  Be prepared to roll back changes.
      bool committed = false;
      auto cleanup = finally([&]{
         if ( !committed )
            Tags::Set( project, oldTags );
      });
      auto newTags = oldTags->Duplicate();
      Tags::Set( project, newTags );

      bool success = Importer::Get().Import(fileName,
                                            &TrackFactory::Get( project ),
                                            newTracks,
                                            newTags.get(),
                                            errorMessage);

      if (!errorMessage.empty()) {
         // Error message derived from Importer::Import
         // Additional help via a Help button links to the manual.
         ShowErrorDialog(&GetProjectFrame( project ), _("Error Importing"),
                         errorMessage, wxT("Importing_Audio"));
      }
      if (!success)
         return false;

      FileHistory::Global().AddFileToHistory(fileName);

      // no more errors, commit
      committed = true;
   }

   // for LOF ("list of files") files, do not import the file as if it
   // were an audio file itself
   if (fileName.AfterLast('.').IsSameAs(wxT("lof"), false)) {
      // PRL: don't redundantly do the steps below, because we already
      // did it in case of LOF, because of some weird recursion back to this
      // same function.  I think this should be untangled.

      // So Undo history push is not bypassed, despite appearances.
      return false;
   }

   // PRL: Undo history is incremented inside this:
   auto newSharedTracks = AddImportedTracks(fileName, std::move(newTracks));

   if (pTrackArray) {
      for (const auto &newTrack : newSharedTracks) {
         newTrack->TypeSwitch( [&](WaveTrack *wt) {
            pTrackArray->push_back( wt->SharedPointer< WaveTrack >() );
         });
      }
   }

   int mode = gPrefs->Read(wxT("/AudioFiles/NormalizeOnLoad"), 0L);
   if (mode == 1) {
      //TODO: All we want is a SelectAll()
      SelectActions::SelectNone( project );
      SelectActions::SelectAllIfNone( project );
      const CommandContext context( project );
      PluginActions::DoEffect(
         EffectManager::Get().GetEffectByIdentifier(wxT("Normalize")),
         context,
         PluginActions::kConfigured);
   }

   // This is a no-fail:
   dirManager.FillBlockfilesCache();
   return true;
}
//
// Undo/History methods
//

void ProjectManager::InitialState()
{
   auto &project = mProject;
   auto &tracks = TrackList::Get( project );
   auto &viewInfo = ViewInfo::Get( project );
   auto &undoManager = UndoManager::Get( project );
   auto &tags = Tags::Get( project );

   undoManager.ClearStates();

   undoManager.PushState(
      &tracks, viewInfo.selectedRegion, tags.shared_from_this(),
      _("Created new project"), wxT(""));

   undoManager.StateSaved();

   auto &menuManager = MenuManager::Get( project );
   menuManager.ModifyUndoMenuItems( project );
   menuManager.UpdateMenus( project );
}

bool ProjectManager::UndoAvailable()
{
   auto &project = mProject;
   auto &tracks = TrackList::Get( project );
   auto &undoManager = UndoManager::Get( project );
   return undoManager.UndoAvailable() &&
       !tracks.HasPendingTracks();
}

bool ProjectManager::RedoAvailable()
{
   auto &project = mProject;
   auto &tracks = TrackList::Get( project );
   auto &undoManager = UndoManager::Get( project );
   return undoManager.RedoAvailable() &&
      !tracks.HasPendingTracks();
}

void ProjectManager::PushState(const wxString &desc, const wxString &shortDesc)
{
   PushState(desc, shortDesc, UndoPush::AUTOSAVE);
}

void ProjectManager::PushState(const wxString &desc,
                                const wxString &shortDesc,
                                UndoPush flags )
{
   auto &project = mProject;
   auto &projectFileIO = ProjectFileIO::Get( project );
   const auto &settings = ProjectSettings::Get( project );
   auto &tracks = TrackList::Get( project );
   auto &viewInfo = ViewInfo::Get( project );
   auto &undoManager = UndoManager::Get( project );
   auto &tags = Tags::Get( project );
   undoManager.PushState(
      &tracks, viewInfo.selectedRegion, tags.shared_from_this(),
      desc, shortDesc, flags);

   mDirty = true;

   auto &menuManager = MenuManager::Get( project );
   menuManager.ModifyUndoMenuItems( project );
   menuManager.UpdateMenus( project );

   if (settings.GetTracksFitVerticallyZoomed())
      ViewActions::DoZoomFitV( project );
   if((flags & UndoPush::AUTOSAVE) != UndoPush::MINIMAL)
      projectFileIO.AutoSave();

   TrackPanel::Get( project ).HandleCursorForPresentMouseState();
}

void ProjectManager::RollbackState()
{
   auto &project = mProject;
   auto &undoManager = UndoManager::Get( project );
   SetStateTo( undoManager.GetCurrentState() );
}

void ProjectManager::ModifyState(bool bWantsAutoSave)
{
   auto &project = mProject;
   auto &projectFileIO = ProjectFileIO::Get( project );
   auto &tracks = TrackList::Get( project );
   auto &viewInfo = ViewInfo::Get( project );
   auto &undoManager = UndoManager::Get( project );
   auto &tags = Tags::Get( project );
   undoManager.ModifyState(
      &tracks, viewInfo.selectedRegion, tags.shared_from_this());
   if (bWantsAutoSave)
      projectFileIO.AutoSave();
   TrackPanel::Get( project ).HandleCursorForPresentMouseState();
}

// LL:  Is there a memory leak here as "l" and "t" are not deleted???
// Vaughan, 2010-08-29: No, as "l" is a TrackList* of an Undo stack state.
//    Need to keep it and its tracks "t" available for Undo/Redo/SetStateTo.
void ProjectManager::PopState(const UndoState &state)
{
   auto &project = mProject;
   auto &projectFileIO = ProjectFileIO::Get( project );
   auto &dstTracks = TrackList::Get( project );
   auto &viewInfo = ViewInfo::Get( project );
   auto &window = ProjectWindow::Get( project );

   viewInfo.selectedRegion = state.selectedRegion;

   // Restore tags
   Tags::Set( project, state.tags );

   TrackList *const tracks = state.tracks.get();

   dstTracks.Clear();
   bool odUsed = false;
   std::unique_ptr<ODComputeSummaryTask> computeTask;

   for (auto t : tracks->Any())
   {
      auto copyTrack = dstTracks.Add(t->Duplicate());

      //add the track to OD if the manager exists.  later we might do a more rigorous check...
      copyTrack->TypeSwitch( [&](WaveTrack *wt) {
         //if the ODManager hasn't been initialized, there's no chance this track has OD blocks since this
         //is a "Redo" operation.
         //TODO: update this to look like the update loop in OpenFile that handles general purpose ODTasks.
         //BUT, it is too slow to go thru every blockfile and check the odtype, so maybe put a flag in wavetrack
         //that gets unset on OD Completion, (and we could also update the drawing there too.)  The hard part is that
         //we would need to watch every possible way a OD Blockfile could get inserted into a wavetrack and change the
         //flag there.
         if(ODManager::IsInstanceCreated())
         {
            if(!odUsed)
            {
               computeTask = std::make_unique<ODComputeSummaryTask>();
               odUsed=true;
            }
            // PRL:  Is it correct to add all tracks to one task, even if they
            // are not partnered channels?  Rather than
            // make one task for each?
            computeTask->AddWaveTrack(wt);
         }
      });
   }

   //add the task.
   if(odUsed)
      ODManager::Instance()->AddNewTask(std::move(computeTask));

   window.HandleResize();

   MenuManager::Get( project ).UpdateMenus( project );

   projectFileIO.AutoSave();
}

void ProjectManager::SetStateTo(unsigned int n)
{
   auto &project = mProject;
   auto &trackPanel = TrackPanel::Get( project );
   auto &undoManager = UndoManager::Get( project );
   auto &window = ProjectWindow::Get( project );

   undoManager.SetStateTo(n,
      [this]( const UndoState &state ){ PopState(state); } );

   window.HandleResize();
   trackPanel.SetFocusedTrack(NULL);
   trackPanel.Refresh(false);
   MenuManager::Get( project ).ModifyUndoMenuItems( project );
}

// This is done to empty out the tracks, but without creating a new project.
void ProjectManager::ResetProjectToEmpty() {
   auto &project = mProject;
   auto &projectFileIO = ProjectFileIO::Get( project );
   auto &viewInfo = ViewInfo::Get( project );

   SelectActions::DoSelectAll( project );
   TrackActions::DoRemoveTracks( project );

   // A new DirManager.
   DirManager::Reset( project );
   TrackFactory::Reset( project );

   projectFileIO.ResetProjectFileIO();

   mDirty = false;
   auto &undoManager = UndoManager::Get( project );
   undoManager.ClearStates();
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
   auto &window = GetProjectFrame( project );
   auto &dirManager = DirManager::Get( project );
   auto mixerToolBar = &MixerToolBar::Get( project );
   mixerToolBar->UpdateControls();
   
   auto &statusBar = *window.GetStatusBar();

   // gAudioIO->GetNumCaptureChannels() should only be positive
   // when we are recording.
   if (projectAudioIO.GetAudioIOToken() > 0 && gAudioIO->GetNumCaptureChannels() > 0) {
      wxLongLong freeSpace = dirManager.GetFreeDiskSpace();
      if (freeSpace >= 0) {
         wxString sMessage;

         int iRecordingMins = GetEstimatedRecordingMinsLeftOnDisk(gAudioIO->GetNumCaptureChannels());
         sMessage.Printf(_("Disk space remaining for recording: %s"), GetHoursMinsString(iRecordingMins));

         // Do not change mLastMainStatusMessage
         statusBar.SetStatusText(sMessage, mainStatusBarField);
      }
   }
   else if(ODManager::IsInstanceCreated())
   {
      //if we have some tasks running, we should say something about it.
      int numTasks = ODManager::Instance()->GetTotalNumTasks();
      if(numTasks)
      {
         wxString msg;
         float ratioComplete= ODManager::Instance()->GetOverallPercentComplete();

         if(ratioComplete>=1.0f)
         {
            //if we are 100 percent complete and there is still a task in the queue, we should wake the ODManager
            //so it can clear it.
            //signal the od task queue loop to wake up so it can remove the tasks from the queue and the queue if it is empty.
            ODManager::Instance()->SignalTaskQueueLoop();


            msg = _("On-demand import and waveform calculation complete.");
            statusBar.SetStatusText(msg, mainStatusBarField);

         }
         else if(numTasks>1)
            msg.Printf(_("Import(s) complete. Running %d on-demand waveform calculations. Overall %2.0f%% complete."),
              numTasks,ratioComplete*100.0);
         else
            msg.Printf(_("Import complete. Running an on-demand waveform calculation. %2.0f%% complete."),
             ratioComplete*100.0);


         statusBar.SetStatusText(msg, mainStatusBarField);
      }
   }

   // As also with the TrackPanel timer:  wxTimer may be unreliable without
   // some restarts
   RestartTimer();
}

void ProjectManager::OnStatusChange( wxCommandEvent & )
{
   auto &project = mProject;
   auto &window = GetProjectFrame( project );
   const auto &msg = project.GetStatus();
   window.GetStatusBar()->SetStatusText(msg, mainStatusBarField);
   
   // When recording, let the NEW status message stay at least as long as
   // the timer interval (if it is not replaced again by this function),
   // before replacing it with the message about remaining disk capacity.
   RestartTimer();
}

void ProjectManager::OnAudioIORate(int rate)
{
   auto &project = mProject;
   auto &window = GetProjectFrame( project );
   wxString display;
   if (rate > 0) {
      display = wxString::Format(_("Actual Rate: %d"), rate);
   }
   else
      // clear the status field
      ;

   int x, y;
   auto statusBar = window.GetStatusBar();
   statusBar->GetTextExtent(display, &x, &y);
   int widths[] = {
      0,
      ControlToolBar::Get( project ).WidthForStatusBar(statusBar),
      -1,
      x+50
   };
   statusBar->SetStatusWidths(4, widths);
   statusBar->SetStatusText(display, rateStatusBarField);
}

void ProjectManager::OnAudioIOStartRecording()
{
   auto &projectFileIO = ProjectFileIO::Get( mProject );
   // Before recording is started, auto-save the file. The file will have
   // empty tracks at the bottom where the recording will be put into
   projectFileIO.AutoSave();
}

// This is called after recording has stopped and all tracks have flushed.
void ProjectManager::OnAudioIOStopRecording()
{
   auto &project = mProject;
   auto &dirManager = DirManager::Get( project );
   auto &projectAudioIO = ProjectAudioIO::Get( project );
   auto &projectFileIO = ProjectFileIO::Get( project );
   auto &window = ProjectWindow::Get( project );

   // Only push state if we were capturing and not monitoring
   if (projectAudioIO.GetAudioIOToken() > 0)
   {
      auto &tracks = TrackList::Get( project );
      auto &intervals = gAudioIO->LostCaptureIntervals();
      if (intervals.size()) {
         // Make a track with labels for recording errors
         auto uTrack = TrackFactory::Get( project ).NewLabelTrack();
         auto pTrack = uTrack.get();
         tracks.Add( uTrack );
         /* i18n-hint:  A name given to a track, appearing as its menu button.
          The translation should be short or else it will not display well.
          At most, about 11 Latin characters.
          Dropout is a loss of a short sequence of audio sample data from the
          recording */
         pTrack->SetName(_("Dropouts"));
         long counter = 1;
         for (auto &interval : intervals)
            pTrack->AddLabel(
               SelectedRegion{ interval.first,
                  interval.first + interval.second },
               wxString::Format(wxT("%ld"), counter++),
               -2 );
         ShowWarningDialog(&window, wxT("DropoutDetected"), _("\
Recorded audio was lost at the labeled locations. Possible causes:\n\
\n\
Other applications are competing with Audacity for processor time\n\
\n\
You are saving directly to a slow external storage device\n\
"
         ),
         false,
         _("Turn off dropout detection"));
      }

      // Add to history
      PushState(_("Recorded Audio"), _("Record"));

      // Reset timer record 
      if (IsTimerRecordCancelled())
      {
         EditActions::DoUndo( project );
         ResetTimerRecordCancelled();
      }

      // Refresh the project window
      window.FixScrollbars();
      window.RedrawProject();
   }

   // Write all cached files to disk, if any
   dirManager.WriteCacheToDisk();

   // Now we auto-save again to get the project to a "normal" state again.
   projectFileIO.AutoSave();
}

void ProjectManager::OnAudioIONewBlockFiles(
   const AutoSaveFile & blockFileLog)
{
   auto &project = mProject;
   auto &projectFileIO = ProjectFileIO::Get( project );
   // New blockfiles have been created, so add them to the auto-save file
   const auto &autoSaveFileName = projectFileIO.GetAutoSaveFileName();
   if ( !autoSaveFileName.empty() )
   {
      wxFFile f{ autoSaveFileName, wxT("ab") };
      if (!f.IsOpened())
         return; // Keep recording going, there's not much we can do here
      blockFileLog.Append(f);
      f.Close();
   }
}

wxString ProjectManager::GetHoursMinsString(int iMinutes)
{
   wxString sFormatted;

   if (iMinutes < 1) {
      // Less than a minute...
      sFormatted = _("Less than 1 minute");
      return sFormatted;
   }

   // Calculate
   int iHours = iMinutes / 60;
   int iMins = iMinutes % 60;

   auto sHours =
      wxString::Format( wxPLURAL("%d hour", "%d hours", iHours), iHours );
   auto sMins =
      wxString::Format( wxPLURAL("%d minute", "%d minutes", iMins), iMins );

   /* i18n-hint: A time in hours and minutes. Only translate the "and". */
   sFormatted.Printf( _("%s and %s."), sHours, sMins);
   return sFormatted;
}

// This routine will give an estimate of how many
// minutes of recording time we have available.
// The calculations made are based on the user's current
// preferences.
int ProjectManager::GetEstimatedRecordingMinsLeftOnDisk(long lCaptureChannels) {
   auto &project = mProject;

   // Obtain the current settings
   auto oCaptureFormat = QualityPrefs::SampleFormatChoice();
   if (lCaptureChannels == 0) {
      gPrefs->Read(wxT("/AudioIO/RecordChannels"), &lCaptureChannels, 2L);
   }

   // Find out how much free space we have on disk
   wxLongLong lFreeSpace = DirManager::Get( project ).GetFreeDiskSpace();
   if (lFreeSpace < 0) {
      return 0;
   }

   // Calculate the remaining time
   double dRecTime = 0.0;
   double bytesOnDiskPerSample = SAMPLE_SIZE_DISK(oCaptureFormat);
   dRecTime = lFreeSpace.GetHi() * 4294967296.0 + lFreeSpace.GetLo();
   dRecTime /= bytesOnDiskPerSample;   
   dRecTime /= lCaptureChannels;
   dRecTime /= ProjectSettings::Get( project ).GetRate();

   // Convert to minutes before returning
   int iRecMins = (int)round(dRecTime / 60.0);
   return iRecMins;
}
