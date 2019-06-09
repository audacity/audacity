/**********************************************************************

Audacity: A Digital Audio Editor

ProjectFileManager.cpp

Paul Licameli split from AudacityProject.cpp

**********************************************************************/

#include "ProjectFileManager.h"

#include "Experimental.h"

#include <wx/crt.h> // for wxPrintf
#include <wx/frame.h>
#include "AutoRecovery.h"
#include "Dependencies.h"
#include "DirManager.h"
#include "FileNames.h"
#include "Project.h"
#include "ProjectFileIO.h"
#include "ProjectSettings.h"
#include "SelectionState.h"
#include "UndoManager.h"
#include "WaveTrack.h"
#include "wxFileNameWrapper.h"
#include "export/Export.h"
#include "ondemand/ODComputeSummaryTask.h"
#include "ondemand/ODManager.h"
#include "ondemand/ODTask.h"
#include "widgets/AudacityMessageBox.h"
#include "widgets/ErrorDialog.h"
#include "widgets/FileHistory.h"
#include "widgets/Warning.h"
#include "xml/XMLFileReader.h"

static const AudacityProject::AttachedObjects::RegisteredFactory sFileManagerKey{
   []( AudacityProject &parent ){
      auto result = std::make_shared< ProjectFileManager >( parent );
      return result;
   }
};

ProjectFileManager &ProjectFileManager::Get( AudacityProject &project )
{
   return project.AttachedObjects::Get< ProjectFileManager >( sFileManagerKey );
}

const ProjectFileManager &ProjectFileManager::Get( const AudacityProject &project )
{
   return Get( const_cast< AudacityProject & >( project ) );
}

ProjectFileManager::ProjectFileManager( AudacityProject &project )
: mProject{ project }
{
}

ProjectFileManager::~ProjectFileManager() = default;

auto ProjectFileManager::ReadProjectFile( const FilePath &fileName )
  -> ReadProjectResults
{
   auto &project = mProject;
   auto &projectFileIO = ProjectFileIO::Get( project );
   auto &window = GetProjectFrame( project );

   project.SetFileName( fileName );
   projectFileIO.SetLoadedFromAup( true );
   projectFileIO.SetIsRecovered( false );
   projectFileIO.SetProjectTitle();

   const wxString autoSaveExt = wxT("autosave");
   if ( wxFileNameWrapper{ fileName }.GetExt() == autoSaveExt )
   {
      AutoSaveFile asf;
      if (!asf.Decode(fileName))
      {
         auto message = AutoSaveFile::FailureMessage( fileName );
         AudacityMessageBox(
            message,
            _("Error decoding file"),
            wxOK | wxCENTRE, &window );
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

   bool bParseSuccess = xmlFile.Parse(&projectFileIO, fileName);
   
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

void ProjectFileManager::EnqueueODTasks()
{
   //check the ODManager to see if we should add the tracks to the ODManager.
   //this flag would have been set in the HandleXML calls from above, if there were
   //OD***Blocks.
   if(ODManager::HasLoadedODFlag())
   {
      auto &project = mProject;
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

bool ProjectFileManager::Save()
{
   // Prompt for file name?
   bool bPromptingRequired = !ProjectFileIO::Get( mProject ).IsProjectSaved();

   if (bPromptingRequired)
      return SaveAs();

   return DoSave(false, false);
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

// Assumes AudacityProject::mFileName has been set to the desired path.
bool ProjectFileManager::DoSave (const bool fromSaveAs,
                              const bool bWantSaveCopy,
                              const bool bLossless /*= false*/)
{
   // See explanation above
   // ProjectDisabler disabler(this);
   auto &proj = mProject;
   const auto &fileName = proj.GetFileName();
   auto &window = GetProjectFrame( proj );
   auto &dirManager = DirManager::Get( proj );
   auto &projectFileIO = ProjectFileIO::Get( proj );
   const auto &settings = ProjectSettings::Get( proj );

   wxASSERT_MSG(!bWantSaveCopy || fromSaveAs, "Copy Project SHOULD only be availabele from SaveAs");

   // Some confirmation dialogs
   if (!bWantSaveCopy)
   {
      auto &tracks = TrackList::Get( proj );
      if ( ! tracks.Any() )
      {
         if ( UndoManager::Get( proj ).UnsavedChanges()
         && settings.EmptyCanBeDirty()) {
            int result = AudacityMessageBox(_("Your project is now empty.\nIf saved, the project will have no tracks.\n\nTo save any previously open tracks:\nClick 'No', Edit > Undo until all tracks\nare open, then File > Save Project.\n\nSave anyway?"),
                                      _("Warning - Empty Project"),
                                      wxYES_NO | wxICON_QUESTION, &window);
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
         bool bSuccess = ShowDependencyDialogIfNeeded(&proj, true);
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
   if (wxFileExists(fileName)) {

#ifdef __WXGTK__
      safetyFileName = fileName + wxT("~");
#else
      safetyFileName = fileName + wxT(".bak");
#endif

      if (wxFileExists(safetyFileName))
         wxRemoveFile(safetyFileName);

      if ( !wxRenameFile(fileName, safetyFileName) ) {
         AudacityMessageBox(
            wxString::Format(
               _("Could not create safety file: %s"), safetyFileName ),
            _("Error"), wxICON_STOP, &window);
         return false;
      }
   }

   bool success = true;
   FilePath project, projName, projPath;
   FilePaths strOtherNamesArray;

   auto cleanup = finally( [&] {
      if (!safetyFileName.empty()) {
         if (wxFileExists(fileName))
            wxRemove(fileName);
         wxRename(safetyFileName, fileName);
      }

      // strOtherNamesArray is a temporary array of file names, used only when
      // saving compressed
      if (!success) {
         AudacityMessageBox(wxString::Format(_("Could not save project. Perhaps %s \nis not writable or the disk is full."),
                                       project),
                      _("Error Saving Project"),
                      wxICON_ERROR, &window);

         // Make the export of tracks succeed all-or-none.
         auto dir = project + wxT("_data");
         for ( auto &name : strOtherNamesArray )
            wxRemoveFile( dir + wxFileName::GetPathSeparator() + name);
         // This has effect only if the folder is empty
         wxFileName::Rmdir( dir );
      }
   } );

   if (fromSaveAs) {
      // This block of code is duplicated in WriteXML, for now...
      project = fileName;
      wxFileName projFName{ fileName };
      if (projFName.GetExt() == wxT("aup"))
         projFName.SetExt( {} ), project = projFName.GetFullPath();
      projName = wxFileNameFromPath(project) + wxT("_data");
      projPath = wxPathOnly(project);

      if( !wxDir::Exists( projPath ) ){
         AudacityMessageBox(wxString::Format(
            _("Could not save project. Path not found. Try creating \ndirectory \"%s\" before saving project with this name."),
            projPath),
                      _("Error Saving Project"),
                      wxICON_ERROR, &window);
         return (success = false);
      }

      if (bWantSaveCopy)
      {
         // Do this before saving the .aup, because we accumulate
         // strOtherNamesArray which affects the contents of the .aup

         // This populates the array strOtherNamesArray
         success = this->SaveCopyWaveTracks(
            project, bLossless, strOtherNamesArray);
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
   XMLFileWriter saveFile{ fileName, _("Error Saving Project") };
   success = GuardedCall< bool >( [&] {
         projectFileIO.WriteXMLHeader(saveFile);
         projectFileIO.WriteXML(saveFile, bWantSaveCopy ? &strOtherNamesArray : nullptr);
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
      projectFileIO.DeleteCurrentAutoSaveFile();

      if ( projectFileIO.IsRecovered() )
      {
         // This was a recovered file, that is, we have just overwritten the
         // old, crashed .aup file. There may still be orphaned blockfiles in
         // this directory left over from the crash, so we DELETE them now
         dirManager.RemoveOrphanBlockfiles();

         // Before we saved this, this was a recovered project, but now it is
         // a regular project, so remember this.
         projectFileIO.SetIsRecovered( false );
         projectFileIO.SetProjectTitle();
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

   window.GetStatusBar()->SetStatusText(
      wxString::Format(_("Saved %s"), fileName), mainStatusBarField);

   return true;
}

bool ProjectFileManager::SaveCopyWaveTracks(const FilePath & strProjectPathName,
   const bool bLossless, FilePaths &strOtherNamesArray)
{
   auto &project = mProject;
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

   Exporter theExporter;
   wxFileName uniqueTrackFileName;
   for (auto pTrack : (trackRange + &Track::IsLeader))
   {
      SelectionStateChanger changer{ SelectionState::Get( project ), tracks };
      auto channels = TrackList::Channels(pTrack);

      for (auto channel : channels)
         channel->SetSelected(true);
      uniqueTrackFileName = wxFileName(strDataDirPathName, pTrack->GetName(), extension);
      FileNames::MakeNameUnique(strOtherNamesArray, uniqueTrackFileName);
      const auto startTime = channels.min( &Track::GetStartTime );
      const auto endTime = channels.max( &Track::GetEndTime );
      bSuccess =
         theExporter.Process(&project, channels.size(),
                              fileFormat, uniqueTrackFileName.GetFullPath(), true,
                              startTime, endTime);

      if (!bSuccess)
         // If only some exports succeed, the cleanup is not done here
         // but trusted to the caller
         break;
   }

   return bSuccess;
}

bool ProjectFileManager::SaveAs(const wxString & newFileName, bool bWantSaveCopy /*= false*/, bool addToHistory /*= true*/)
{
   auto &project = mProject;
   auto &projectFileIO = ProjectFileIO::Get( project );
   bool bLoadedFromAup = projectFileIO.IsLoadedFromAup();

   // This version of SaveAs is invoked only from scripting and does not
   // prompt for a file name
   auto oldFileName = project.GetFileName();

   bool bOwnsNewAupName = bLoadedFromAup && (oldFileName == newFileName);
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

   project.SetFileName( newFileName );
   bool success = false;
   auto cleanup = finally( [&] {
      if (!success || bWantSaveCopy)
         // Restore file name on error
         project.SetFileName( oldFileName );
   } );

   //Don't change the title, unless we succeed.
   //SetProjectTitle();

   success = DoSave(!bOwnsNewAupName || bWantSaveCopy, bWantSaveCopy);

   if (success && addToHistory) {
      FileHistory::Global().AddFileToHistory( project.GetFileName() );
   }
   if (!success || bWantSaveCopy) // bWantSaveCopy doesn't actually change current project.
   {
   }
   else {
      projectFileIO.SetLoadedFromAup( true );
      projectFileIO.SetProjectTitle();
   }

   return(success);
}


bool ProjectFileManager::SaveAs(bool bWantSaveCopy /*= false*/, bool bLossless /*= false*/)
{
   auto &project = mProject;
   auto &projectFileIO = ProjectFileIO::Get( project );
   auto &window = GetProjectFrame( project );
   TitleRestorer Restorer( window, project ); // RAII
   bool bHasPath = true;
   wxFileName filename{ project.GetFileName() };
   // Save a copy of the project with 32-bit float tracks.
   if (bLossless)
      bWantSaveCopy = true;

   bool bLoadedFromAup = projectFileIO.IsLoadedFromAup();

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
   if (ShowWarningDialog(&window, wxT("FirstProjectSave"), message, true) != wxID_OK)
   {
      return false;
   }

   bool bPrompt = (project.mBatchMode == 0) || (project.GetFileName().empty());
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
         &window);

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

   bool bOwnsNewAupName = bLoadedFromAup && ( project.GetFileName() == fName );
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
      int mayOverwrite = ( project.GetFileName() == fName ) ? 2 : 1;
      for ( auto p : AllProjects{} ) {
         const wxFileName openProjectName{ p->GetFileName() };
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
                                         &window);
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

   auto oldFileName = project.GetFileName();
   project.SetFileName( fName );
   bool success = false;
   auto cleanup = finally( [&] {
      if (!success || bWantSaveCopy)
         // Restore file name on error
         project.SetFileName( oldFileName );
   } );

   success = DoSave(!bOwnsNewAupName || bWantSaveCopy, bWantSaveCopy, bLossless);

   if (success) {
      FileHistory::Global().AddFileToHistory( project.GetFileName() );
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
      projectFileIO.SetLoadedFromAup( true );
      projectFileIO.SetProjectTitle();
   }


   return(success);
}

void ProjectFileManager::Reset()
{
   // mLastSavedTrack code copied from OnCloseWindow.
   // Lock all blocks in all tracks of the last saved version, so that
   // the blockfiles aren't deleted on disk when we DELETE the blockfiles
   // in memory.  After it's locked, DELETE the data structure so that
   // there's no memory leak.
   CloseLock();
   
   ProjectFileIO::Get( mProject ).Reset();
}

bool ProjectFileManager::SaveFromTimerRecording(wxFileName fnFile)
{
   auto &project = mProject;
   auto &projectFileIO = ProjectFileIO::Get( project );

   // MY: Will save the project to a NEW location a-la Save As
   // and then tidy up after itself.

   wxString sNewFileName = fnFile.GetFullPath();

   // MY: To allow SaveAs from Timer Recording we need to check what
   // the value of mFileName is before we change it.
   FilePath sOldFilename;
   if (projectFileIO.IsProjectSaved()) {
      sOldFilename = project.GetFileName();
   }

   // MY: If the project file already exists then bail out
   // and send populate the message string (pointer) so
   // we can tell the user what went wrong.
   if (wxFileExists(sNewFileName)) {
      return false;
   }

   project.SetFileName( sNewFileName );
   bool bSuccess = false;
   auto cleanup = finally( [&] {
      if (!bSuccess)
         // Restore file name on error
         project.SetFileName( sOldFilename );
   } );

   bSuccess = DoSave(true, false);

   if (bSuccess) {
      FileHistory::Global().AddFileToHistory( project.GetFileName() );
      projectFileIO.SetLoadedFromAup( true );
      projectFileIO.SetProjectTitle();
   }

   return bSuccess;
}

void ProjectFileManager::CloseLock()
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
