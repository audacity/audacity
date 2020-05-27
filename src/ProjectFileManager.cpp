/**********************************************************************

Audacity: A Digital Audio Editor

ProjectFileManager.cpp

Paul Licameli split from AudacityProject.cpp

**********************************************************************/

#include "ProjectFileManager.h"

#include "Experimental.h"

#include <wx/crt.h> // for wxPrintf

#if defined(__WXGTK__)
#include <wx/evtloop.h>
#endif

#include <wx/frame.h>
#include "AutoRecovery.h"
#include "Dependencies.h"
#include "DirManager.h"
#include "FileFormats.h"
#include "FileNames.h"
#include "Legacy.h"
#include "PlatformCompatibility.h"
#include "Project.h"
#include "ProjectFileIO.h"
#include "ProjectFileIORegistry.h"
#include "ProjectFSCK.h"
#include "ProjectHistory.h"
#include "ProjectSelectionManager.h"
#include "ProjectSettings.h"
#include "ProjectStatus.h"
#include "ProjectWindow.h"
#include "SelectUtilities.h"
#include "SelectionState.h"
#include "Sequence.h"
#include "Tags.h"
#include "TrackPanelAx.h"
#include "TrackPanel.h"
#include "UndoManager.h"
#include "WaveClip.h"
#include "WaveTrack.h"
#include "wxFileNameWrapper.h"
#include "blockfile/ODDecodeBlockFile.h"
#include "export/Export.h"
#include "import/Import.h"
#include "import/ImportMIDI.h"
#include "commands/CommandContext.h"
#include "ondemand/ODComputeSummaryTask.h"
#include "ondemand/ODDecodeFlacTask.h"
#include "ondemand/ODManager.h"
#include "ondemand/ODTask.h"
#include "toolbars/SelectionBar.h"
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

namespace {

const char *const defaultHelpUrl =
   "FAQ:Errors_on_opening_or_recovering_an_Audacity_project";

using Pair = std::pair< const char *, const char * >;
const Pair helpURLTable[] = {
   {
      "not well-formed (invalid token)",
      "Error:_not_well-formed_(invalid_token)_at_line_x"
   },
   {
      "reference to invalid character number",
      "Error_Opening_Project:_Reference_to_invalid_character_number_at_line_x"
   },
   {
      "mismatched tag",
      "#mismatched"
   },
// These two errors with FAQ entries are reported elsewhere, not here....
//#[[#import-error|Error Importing: Aup is an Audacity Project file. Use the File > Open command]]
//#[[#corrupt|Error Opening File or Project: File may be invalid or corrupted]]
};

wxString FindHelpUrl( const TranslatableString &libraryError )
{
   wxString helpUrl;
   if ( !libraryError.empty() ) {
      helpUrl = defaultHelpUrl;

      auto msgid = libraryError.MSGID().GET();
      auto found = std::find_if( begin(helpURLTable), end(helpURLTable),
         [&]( const Pair &pair ) {
            return msgid.Contains( pair.first ); }
      );
      if (found != end(helpURLTable)) {
         auto url = found->second;
         if (url[0] == '#')
            helpUrl += url;
         else
            helpUrl = url;
      }
   }

   return helpUrl;
}

}

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
            XO("Error decoding file"),
            wxOK | wxCENTRE,
            &window );
         // Important: Prevent deleting any temporary files!
         DirManager::SetDontDeleteTempFiles();
         return { true };
      }
   }

   ///
   /// Parse project file
   ///

   XMLFileReader xmlFile;

#ifdef EXPERIMENTAL_OD_DATA
   // 'Lossless copy' projects have dependencies. We need to always copy-in
   // these dependencies when converting to a normal project.
   auto oldAction = FileFormatsCopyOrEditSetting.Read();
   bool oldAsk =
      gPrefs->ReadBool(wxT("/Warnings/CopyOrEditUncompressedDataAsk"), true);

   if (oldAction != wxT("copy"))
      FileFormatsCopyOrEditSetting.Write( wxT("copy") );
   if (oldAsk)
      gPrefs->Write(wxT("/Warnings/CopyOrEditUncompressedDataAsk"), (long) false);
   gPrefs->Flush();

   auto cleanup = finally( [&] {
      // and restore old settings if necessary.
      if (oldAction != wxT("copy"))
         FileFormatsCopyOrEditSetting.Write( oldAction );
      if (oldAsk)
         gPrefs->Write(wxT("/Warnings/CopyOrEditUncompressedDataAsk"), (long) true);
      gPrefs->Flush();
   } );
#endif

   bool bParseSuccess = xmlFile.Parse(&projectFileIO, fileName);
   
   bool err = false;

   if (bParseSuccess) {
      // By making a duplicate set of pointers to the existing blocks
      // on disk, we add one to their reference count, guaranteeing
      // that their reference counts will never reach zero and thus
      // the version saved on disk will be preserved until the
      // user selects Save().

      mLastSavedTracks = TrackList::Create( nullptr );

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

   return {
      false, bParseSuccess, err, xmlFile.GetErrorStr(),
      FindHelpUrl( xmlFile.GetLibraryErrorStr() )
   };
}

///gets an int with OD flags so that we can determine which ODTasks should be run on this track after save/open, etc.
unsigned int ProjectFileManager::GetODFlags( const WaveTrack &track )
{
   unsigned int ret = 0;
   for ( const auto &clip : track.GetClips() )
   {
      auto sequence = clip->GetSequence();
      const auto &blocks = sequence->GetBlockArray();
      for ( const auto &block : blocks ) {
         const auto &file = block.f;
         if(!file->IsDataAvailable())
            ret |= (static_cast< ODDecodeBlockFile * >( &*file ))->GetDecodeType();
         else if(!file->IsSummaryAvailable())
            ret |= ODTask::eODPCMSummary;
      }
   }
   return ret;
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
         unsigned int odFlags = GetODFlags( *wt );

         //add the track to the already created tasks that correspond to the od flags in the wavetrack.
         for(unsigned int i=0;i<newTasks.size();i++) {
            if(newTasks[i]->GetODType() & odFlags)
               newTasks[i]->AddWaveTrack(wt->SharedPointer< WaveTrack >());
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
               newTask->AddWaveTrack(wt->SharedPointer< WaveTrack >());
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

   wxASSERT_MSG(!bWantSaveCopy || fromSaveAs, "Copy Project SHOULD only be available from SaveAs");

   // Some confirmation dialogs
   if (!bWantSaveCopy)
   {
      auto &tracks = TrackList::Get( proj );
      if ( ! tracks.Any() )
      {
         if ( UndoManager::Get( proj ).UnsavedChanges()
         && settings.EmptyCanBeDirty()) {
            int result = AudacityMessageBox(
               XO(
"Your project is now empty.\nIf saved, the project will have no tracks.\n\nTo save any previously open tracks:\nClick 'No', Edit > Undo until all tracks\nare open, then File > Save Project.\n\nSave anyway?"),
               XO("Warning - Empty Project"),
               wxYES_NO | wxICON_QUESTION,
               &window);
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

      bool bOK=true;
      if (wxFileExists(safetyFileName))
         bOK = wxRemoveFile(safetyFileName);

      if ( !wxRenameFile(fileName, safetyFileName) ) {
         AudacityMessageBox(
            XO(
"Audacity failed to write file %s.\nPerhaps disk is full or not writable.")
               .Format( safetyFileName ),
            XO("Error Writing to File"),
            wxICON_STOP,
            &window);
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
         AudacityMessageBox(
            XO(
"Could not save project. Perhaps %s \nis not writable or the disk is full.")
               .Format( project ),
            XO("Error Saving Project"),
            wxICON_ERROR,
            &window);

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
         AudacityMessageBox(
            XO(
"Could not save project. Path not found. Try creating \ndirectory \"%s\" before saving project with this name.")
               .Format( projPath ),
            XO("Error Saving Project"),
            wxICON_ERROR,
            &window);
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
   XMLFileWriter saveFile{ fileName, XO("Error Saving Project") };
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
   Optional<DirManager::ProjectSetter> pSetter;
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
      pSetter.emplace( dirManager, projPath, projName, true, moving );

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
      mLastSavedTracks = TrackList::Create( nullptr );

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

   ProjectStatus::Get( proj ).Set( XO("Saved %s").Format( fileName ) );

   return true;
}

bool ProjectFileManager::SaveCopyWaveTracks(const FilePath & strProjectPathName,
   const bool bLossless, FilePaths &strOtherNamesArray)
{
   auto &project = mProject;
   auto &tracks = TrackList::Get( project );
   auto &trackFactory = TrackFactory::Get( project );

   wxString extension, fileFormat;
   bool haveVorbis =
#if defined(USE_LIBVORBIS)
      true;
#else
      false;
#endif
   if (!bLossless && haveVorbis) {
      extension = wxT("ogg");
      fileFormat = wxT("OGG");
   } else{
      extension = wxT("wav");
      fileFormat = wxT("WAV");

      // LLL: Temporary hack until I can figure out how to add an "ExportPCMCommand"
      //      to create a 32-bit float WAV file.  It tells the ExportPCM exporter
      //      to use float when exporting the next WAV file.
      //
      //      This was done as part of the resolution for bug #2062.
      //
      // See: ExportPCM.cpp, LoadEncoding()
      auto cleanup = finally([&] {
         gPrefs->DeleteEntry(wxT("/FileFormats/ExportFormat_SF1_ForceFloat"));
         gPrefs->Flush();
      });
      gPrefs->Write(wxT("/FileFormats/ExportFormat_SF1_ForceFloat"), true);
      gPrefs->Flush();
   }

   // Some of this is similar to code in ExportMultipleDialog::ExportMultipleByTrack
   // but that code is really tied into the dialogs.

      // Copy the tracks because we're going to do some state changes before exporting.
      unsigned int numWaveTracks = 0;

   auto ppSavedTrackList = TrackList::Create( nullptr );
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

   Exporter theExporter{ project };
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
         theExporter.Process(channels.size(),
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
         nullptr,
         XO("The project was not saved because the file name provided would overwrite another project.\nPlease try again and select an original name."),
         XO("Error Saving Project"),
         wxOK|wxICON_ERROR );
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
      FileHistory::Global().Append( project.GetFileName() );
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

   TranslatableString title;
   TranslatableString message;
   if (bWantSaveCopy)
   {
      if (bLossless)
      {
         title = XO("%sSave Lossless Copy of Project \"%s\" As...")
            .Format( Restorer.sProjNumber,Restorer.sProjName );
         message = XO("\
'Save Lossless Copy of Project' is for an Audacity project, not an audio file.\n\
For an audio file that will open in other apps, use 'Export'.\n\n\
\
Lossless copies of project are a good way to backup your project, \n\
with no loss of quality, but the projects are large.\n");
      }
      else
      {
         title = XO("%sSave Compressed Copy of Project \"%s\" As...")
            .Format( Restorer.sProjNumber, Restorer.sProjName );
         message = XO("\
'Save Compressed Copy of Project' is for an Audacity project, not an audio file.\n\
For an audio file that will open in other apps, use 'Export'.\n\n\
\
Compressed project files are a good way to transmit your project online, \n\
but they have some loss of fidelity.\n");
      }
   }
   else
   {
      title = XO("%sSave Project \"%s\" As...")
         .Format( Restorer.sProjNumber, Restorer.sProjName );
      message = XO("\
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
         { FileNames::AudacityProjects },
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
         nullptr,
         XO("Saving a copy must not overwrite an existing saved project.\nPlease try again and select an original name."),
         XO("Error Saving Copy of Project"),
         wxOK|wxICON_ERROR );
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
         auto Message = XO("\
Do you want to overwrite the project:\n\"%s\"?\n\n\
If you select \"Yes\" the project\n\"%s\"\n\
will be irreversibly overwritten.").Format( fName, fName );

         // For safety, there should NOT be an option to hide this warning.
         int result = AudacityMessageBox(
            Message,
            /* i18n-hint: Heading: A warning that a project is about to be overwritten.*/
            XO("Overwrite Project Warning"),
            wxYES_NO | wxNO_DEFAULT | wxICON_WARNING,
            &window);
         if (result != wxYES) {
            return false;
         }
      }
      else
      {
         // Overwrite disallowed. The destination project is open in another window.
         AudacityMessageDialog m(
            nullptr,
            XO("The project will not saved because the selected project is open in another window.\nPlease try again and select an original name."),
            XO("Error Saving Project"),
            wxOK|wxICON_ERROR );
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
      FileHistory::Global().Append( project.GetFileName() );
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
      FileHistory::Global().Append( project.GetFileName() );
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

// static method, can be called outside of a project
wxArrayString ProjectFileManager::ShowOpenDialog(
   const FileNames::FileType &extraType )
{
   // Construct the filter
   const auto fileTypes = Importer::Get().GetFileTypes( extraType );

   // Retrieve saved path
   auto path = FileNames::FindDefaultPath(FileNames::Operation::Open);

   // Construct and display the file dialog
   wxArrayString selected;

   FileDialogWrapper dlog(nullptr,
      XO("Select one or more files"),
      path,
      wxT(""),
      fileTypes,
      wxFD_OPEN | wxFD_MULTIPLE | wxRESIZE_BORDER);

   dlog.SetFilterIndex( Importer::SelectDefaultOpenType( fileTypes ) );

   int dialogResult = dlog.ShowModal();

   // Convert the filter index to type and save
   auto index = dlog.GetFilterIndex();
   const auto &saveType = fileTypes[ index ];

   Importer::SetDefaultOpenType( saveType );
   Importer::SetLastOpenType( saveType );

   if (dialogResult == wxID_OK) {
      // Return the selected files
      dlog.GetPaths(selected);
   }
   return selected;
}

// static method, can be called outside of a project
bool ProjectFileManager::IsAlreadyOpen(const FilePath &projPathName)
{
   const wxFileName newProjPathName(projPathName);
   auto start = AllProjects{}.begin(), finish = AllProjects{}.end(),
   iter = std::find_if( start, finish,
      [&]( const AllProjects::value_type &ptr ){
         return newProjPathName.SameAs(wxFileNameWrapper{ ptr->GetFileName() });
      } );
   if (iter != finish) {
      auto errMsg =
      XO("%s is already open in another window.")
         .Format( newProjPathName.GetName() );
      wxLogError(errMsg.Translation()); //Debug?
      AudacityMessageBox(
         errMsg,
         XO("Error Opening Project"),
         wxOK | wxCENTRE);
      return true;
   }
   return false;
}

XMLTagHandler *
ProjectFileManager::RecordingRecoveryFactory( AudacityProject &project ) {
   auto &ProjectFileManager = Get( project );
   auto &ptr = ProjectFileManager.mRecordingRecoveryHandler;
   if (!ptr)
      ptr =
         std::make_unique<RecordingRecoveryHandler>( &project );
   return ptr.get();
}

ProjectFileIORegistry::Entry
ProjectFileManager::sRecoveryFactory{
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
         ProjectFileManager::Get( *mProject ).Import(strAttr, &trackArray); },
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
ProjectFileManager::ImportHandlerFactory( AudacityProject &project ) {
   auto &ProjectFileManager = Get( project );
   auto &ptr = ProjectFileManager.mImportXMLTagHandler;
   if (!ptr)
      ptr =
         std::make_unique<ImportXMLTagHandler>( &project );
   return ptr.get();
}

ProjectFileIORegistry::Entry
ProjectFileManager::sImportHandlerFactory{
   wxT("import"), ImportHandlerFactory
};

// FIXME:? TRAP_ERR This should return a result that is checked.
//    See comment in AudacityApp::MRUOpen().
void ProjectFileManager::OpenFile(const FilePath &fileNameArg, bool addtohistory)
{
   auto &project = mProject;
   auto &history = ProjectHistory::Get( project );
   auto &projectFileIO = ProjectFileIO::Get( project );
   auto &tracks = TrackList::Get( project );
   auto &trackPanel = TrackPanel::Get( project );
   auto &dirManager = DirManager::Get( project );
   auto &window = ProjectWindow::Get( project );

   // On Win32, we may be given a short (DOS-compatible) file name on rare
   // occasions (e.g. stuff like "C:\PROGRA~1\AUDACI~1\PROJEC~1.AUP"). We
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
         XO(
"You are trying to open an automatically created backup file.\nDoing this may result in severe data loss.\n\nPlease open the actual Audacity project file instead."),
         XO("Warning - Backup File Detected"),
         wxOK | wxCENTRE,
         &window);
      return;
   }

   if (!::wxFileExists(fileName)) {
      AudacityMessageBox(
         XO("Could not open file: %s").Format( fileName ),
         XO("Error Opening File"),
         wxOK | wxCENTRE,
         &window);
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
            XO("Could not open file: %s").Format( fileName ),
            XO("Error opening file"),
            wxOK | wxCENTRE,
            &window);
         return;
      }
      int numRead = ff.Read(buf, 15);
      if (numRead != 15) {
         AudacityMessageBox(
            XO("File may be invalid or corrupted: \n%s").Format( fileName ),
            XO("Error Opening File or Project"),
            wxOK | wxCENTRE,
            &window);
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
         AudacityMessageBox(
            XO(
"Audacity was unable to convert an Audacity 1.0 project to the new project format."),
            XO("Error Opening Project"),
            wxOK | wxCENTRE,
            &window);
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
         if (FileNames::IsMidi(fileName))
            DoImportMIDI( project, fileName );
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

   auto results = ReadProjectFile( fileName );

   if ( results.decodeError )
      return;

   const bool bParseSuccess = results.parseSuccess;
   const auto &errorStr = results.errorString;
   const bool err = results.trackError;

   if (bParseSuccess) {
      auto &settings = ProjectSettings::Get( project );
      window.mbInitializingScrollbar = true; // this must precede AS_SetSnapTo
         // to make persistence of the vertical scrollbar position work

      auto &selectionManager = ProjectSelectionManager::Get( project );
      selectionManager.AS_SetSnapTo(settings.GetSnapTo());
      selectionManager.AS_SetSelectionFormat(settings.GetSelectionFormat());
      selectionManager.TT_SetAudioTimeFormat(settings.GetAudioTimeFormat());
      selectionManager.SSBL_SetFrequencySelectionFormatName(
      settings.GetFrequencySelectionFormatName());
      selectionManager.SSBL_SetBandwidthSelectionFormatName(
      settings.GetBandwidthSelectionFormatName());

      SelectionBar::Get( project ).SetRate( settings.GetRate() );

      ProjectHistory::Get( project ).InitialState();
      TrackFocus::Get( project ).Set( *tracks.Any().begin() );
      window.HandleResize();
      trackPanel.Refresh(false);
      trackPanel.Update(); // force any repaint to happen now,
      // else any asynch calls into the blockfile code will not have
      // finished logging errors (if any) before the call to ProjectFSCK()

      if (addtohistory)
         FileHistory::Global().Append(fileName);
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
            EnqueueODTasks();
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
         history.PushState(XO("Project was recovered"), XO("Recover"));

         if (!wxRemoveFile(fileName))
            AudacityMessageBox(
               XO("Could not remove old auto save file"),
               XO("Error"),
               wxICON_STOP,
               &window);
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
               // explicitly opted to close the project.
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
            //    this->PushState(XO("Project checker repaired file"), XO("Project Repair"));

            if (status & FSCKstatus_SAVE_AUP)
               Save(), saved = true;
         }
      }

      if (mImportXMLTagHandler) {
         if (!saved)
            // We processed an <import> tag, so save it as a normal project,
            // with no <import> tags.
            Save();
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

      wxLogError(wxT("Could not parse file \"%s\". \nError: %s"), fileName, errorStr.Debug());

      ShowErrorDialog(
         &window,
         XO("Error Opening Project"),
         errorStr,
         results.helpUrl);
   }
}

std::vector< std::shared_ptr< Track > >
ProjectFileManager::AddImportedTracks(const FilePath &fileName,
                                   TrackHolders &&newTracks)
   {
   auto &project = mProject;
   auto &history = ProjectHistory::Get( project );
   auto &projectFileIO = ProjectFileIO::Get( project );
   auto &tracks = TrackList::Get( project );

   std::vector< std::shared_ptr< Track > > results;

   SelectUtilities::SelectNone( project );

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
                  SetImportedDependencies( true );
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

   history.PushState(XO("Imported '%s'").Format( fileName ),
       XO("Import"));

#if defined(__WXGTK__)
   // See bug #1224
   // The track panel hasn't we been fully created, so the DoZoomFit() will not give
   // expected results due to a window width of zero.  Should be safe to yield here to
   // allow the creation to complete.  If this becomes a problem, it "might" be possible
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
bool ProjectFileManager::Import(
   const FilePath &fileName, WaveTrackArray* pTrackArray /*= NULL*/)
{
   auto &project = mProject;
   auto &dirManager = DirManager::Get( project );
   auto oldTags = Tags::Get( project ).shared_from_this();
   TrackHolders newTracks;
   TranslatableString errorMessage;

   {
      // Backup Tags, before the import.  Be prepared to roll back changes.
      bool committed = false;
      auto cleanup = finally([&]{
         if ( !committed )
            Tags::Set( project, oldTags );
      });
      auto newTags = oldTags->Duplicate();
      Tags::Set( project, newTags );

      bool success = Importer::Get().Import(project, fileName,
                                            &TrackFactory::Get( project ),
                                            newTracks,
                                            newTags.get(),
                                            errorMessage);

      if (!errorMessage.empty()) {
         // Error message derived from Importer::Import
         // Additional help via a Help button links to the manual.
         ShowErrorDialog(&GetProjectFrame( project ), XO("Error Importing"),
                         errorMessage, wxT("Importing_Audio"));
      }
      if (!success)
         return false;

      FileHistory::Global().Append(fileName);

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

   // This is a no-fail:
   dirManager.FillBlockfilesCache();
   return true;
}
