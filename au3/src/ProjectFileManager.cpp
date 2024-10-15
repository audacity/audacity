/**********************************************************************

Audacity: A Digital Audio Editor

ProjectFileManager.cpp

Paul Licameli split from AudacityProject.cpp

**********************************************************************/

#include "ProjectFileManager.h"

#include <wx/crt.h> // for wxPrintf

#if defined(__WXGTK__)
#include <wx/evtloop.h>
#endif

#include "AnalyzedWaveClip.h"
#include "AudacityMessageBox.h"
#include "AudacityMirProject.h"
#include "BasicUI.h"
#include "ClipMirAudioReader.h"
#include "CodeConversions.h"
#include "Export.h"
#include "HelpText.h"
#include "Import.h"
#include "ImportPlugin.h"
#include "ImportProgressListener.h"
#include "Legacy.h"
#include "MusicInformationRetrieval.h"
#include "PlatformCompatibility.h"
#include "Project.h"
#include "ProjectFileIO.h"
#include "ProjectHistory.h"
#include "ProjectNumericFormats.h"
#include "ProjectRate.h"
#include "ProjectSelectionManager.h"
#include "ProjectSettings.h"
#include "ProjectStatus.h"
#include "ProjectTimeSignature.h"
#include "ProjectWindow.h"
#include "ProjectWindows.h"
#include "RealtimeEffectList.h"
#include "SelectFile.h"
#include "SelectUtilities.h"
#include "SelectionState.h"
#include "Tags.h"
#include "TempDirectory.h"
#include "TempoChange.h"
#include "TimeDisplayMode.h"
#include "TrackFocus.h"
#include "TrackPanel.h"
#include "UndoTracks.h"
#include "UserException.h"
#include "ViewInfo.h"
#include "WaveClip.h"
#include "WaveTrack.h"
#include "WaveTrackUtilities.h"
#include "XMLFileReader.h"
#include "import/ImportStreamDialog.h"
#include "prefs/ImportExportPrefs.h"
#include "widgets/FileHistory.h"
#include "widgets/UnwritableLocationErrorDialog.h"
#include "widgets/Warning.h"
#include "wxFileNameWrapper.h"
#include "wxPanelWrapper.h"

#include "ProjectFileIOExtension.h"

#include <optional>
#include <wx/frame.h>
#include <wx/log.h>

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

void ProjectFileManager::DiscardAutosave(const FilePath &filename)
{
   InvisibleTemporaryProject tempProject;
   auto &project = tempProject.Project();
   auto &projectFileManager = Get(project);
   // Read the project, discarding autosave
   projectFileManager.ReadProjectFile(filename, true);

   if (projectFileManager.mLastSavedTracks) {
      for (auto wt : projectFileManager.mLastSavedTracks->Any<WaveTrack>())
         WaveTrackUtilities::CloseLock(*wt);
      projectFileManager.mLastSavedTracks.reset();
   }

   // Side-effect on database is done, and destructor of tempProject
   // closes the temporary project properly
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
// This error with FAQ entry is reported elsewhere, not here....
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

auto ProjectFileManager::ReadProjectFile(
   const FilePath &fileName, bool discardAutosave )
  -> ReadProjectResults
{
   auto &project = mProject;
   auto &projectFileIO = ProjectFileIO::Get( project );
   auto &window = GetProjectFrame( project );

   ///
   /// Parse project file
   ///
   auto parseResult = projectFileIO.LoadProject(fileName, discardAutosave);
   const bool bParseSuccess = parseResult.has_value();

   bool err = false;
   std::optional<TranslatableString> linkTypeChangeReason;

   TranslatableString otherError;

   if (bParseSuccess)
   {
      auto& tracks = TrackList::Get(project);
      // By making a duplicate set of pointers to the existing blocks
      // on disk, we add one to their reference count, guaranteeing
      // that their reference counts will never reach zero and thus
      // the version saved on disk will be preserved until the
      // user selects Save().
      // Do this before FixTracks might delete zero-length clips!
      mLastSavedTracks = TrackList::Create( nullptr );
      WaveTrack *leader{};
      tracks.Any().Visit(
         [&](WaveTrack& track) {
            // A rare place where TrackList::Channels remains necessary, to
            // visit the right channels of stereo tracks not yet "zipped",
            // otherwise later, CloseLock() will be missed for some sample
            // blocks and corrupt the project
            for (const auto pChannel : TrackList::Channels(&track))
            {
               auto left = leader;
               auto newTrack =
                  pChannel->Duplicate(Track::DuplicateOptions {}.Backup());
               leader = left ? nullptr // now visiting the right channel
                        :
                        (pChannel->GetLinkType() == Track::LinkType::None) ?
                               nullptr // now visiting a mono channel
                               :
                               static_cast<WaveTrack*>(newTrack.get())
                  // now visiting a left channel
                  ;
               mLastSavedTracks->Add(newTrack);
               if (left)
                  // Zip clips allowing misalignment -- this may be a legacy
                  // project.  This duplicate track will NOT be used for normal
                  // editing, but only later to visit all the sample blocks that
                  // existed at last save time.
                  left->ZipClips(false);
            }
         },
         [this](Track& track) {
            mLastSavedTracks->Add(
               track.Duplicate(Track::DuplicateOptions {}.Backup()));
         });

      FixTracks(
         tracks,
         // Keep at most one of the error messages
         [&](const auto& errorMessage) { otherError = errorMessage; err = true; },
         [&](const auto& unlinkReason) { linkTypeChangeReason = unlinkReason; });

      if (!err) {
         if(linkTypeChangeReason && !discardAutosave)
         {
            BasicUI::ShowMessageBox(XO(
//i18n-hint: Text of the message dialog that may appear on attempt
//to open a project created by Audacity version prior to 3.4.
//%s will be replaced with an explanation of the actual reason of
//project modification.
"%s\n"
"This feature is not supported in Audacity versions past 3.3.3.\n"
"These stereo tracks have been split into mono tracks.\n"
"As a result, some realtime effects may be missing.\n"
"Please verify that everything works as intended before saving."
            ).Format(linkTypeChangeReason->Translation()));
         }

         parseResult->Commit();
         if (discardAutosave)
            // REVIEW: Failure OK?
            projectFileIO.AutoSaveDelete();
         else if (projectFileIO.IsRecovered()) {
            bool resaved = false;

            if (!projectFileIO.IsTemporary() &&
               !linkTypeChangeReason)
            {
               // Re-save non-temporary project to its own path.  This
               // might fail to update the document blob in the database.
               resaved = projectFileIO.SaveProject(fileName, nullptr);
            }

            AudacityMessageBox(
               resaved
                  ? XO(
"This project was not saved properly the last time Audacity ran.\n\n"
"It has been recovered to the last snapshot.")
                  : XO(
"This project was not saved properly the last time Audacity ran.\n\n"
"It has been recovered to the last snapshot, but you must save it\n"
"to preserve its contents."),
               XO("Project Recovered"),
               wxICON_WARNING,
               &window);
         }
      }

      ProjectFileIOExtensionRegistry::OnLoad(mProject);
   }

   return {
      bParseSuccess,
      err,
      (bParseSuccess ? otherError : projectFileIO.GetLastError()),
      FindHelpUrl(projectFileIO.GetLibraryError())
   };
}

bool ProjectFileManager::Save()
{
   auto &projectFileIO = ProjectFileIO::Get(mProject);

   if (auto action = ProjectFileIOExtensionRegistry::OnSave(
          mProject, [this](auto& path, bool rename)
          { return DoSave(audacity::ToWXString(path), rename); });
      action != OnSaveAction::Continue)
      return action == OnSaveAction::Handled;

   // Prompt for file name?
   if (projectFileIO.IsTemporary())
   {
      return SaveAs(true);
   }

   return DoSave(projectFileIO.GetFileName(), false);
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

// Assumes ProjectFileIO::mFileName has been set to the desired path.
bool ProjectFileManager::DoSave(const FilePath & fileName, const bool fromSaveAs)
{
   // See explanation above
   // ProjectDisabler disabler(this);
   auto &proj = mProject;
   auto &window = GetProjectFrame( proj );
   auto &projectFileIO = ProjectFileIO::Get( proj );
   const auto &settings = ProjectSettings::Get( proj );

   // Some confirmation dialogs
   {
      if (TempDirectory::FATFilesystemDenied(fileName, XO("Projects cannot be saved to FAT drives.")))
      {
         return false;
      }

      wxULongLong fileSize = wxFileName::GetSize(projectFileIO.GetFileName());

      wxDiskspaceSize_t freeSpace;
      if (wxGetDiskSpace(FileNames::AbbreviatePath(fileName), NULL, &freeSpace))
      {
         if (freeSpace.GetValue() <= fileSize.GetValue())
         {
            BasicUI::ShowErrorDialog( *ProjectFramePlacement( &proj ),
               XO("Insufficient Disk Space"),
               XO("The project size exceeds the available free space on the target disk.\n\n"
                  "Please select a different disk with more free space."),
               "Error:_Disk_full_or_not_writable"
               );

            return false;
         }
      }
   }
   // End of confirmations

   // Always save a backup of the original project file
   std::optional<ProjectFileIO::BackupProject> pBackupProject;
   if (fromSaveAs && wxFileExists(fileName))
   {
      pBackupProject.emplace(projectFileIO, fileName);
      if (!pBackupProject->IsOk())
         return false;
   }

   if (FileNames::IsOnFATFileSystem(fileName))
   {
      if (wxFileName::GetSize(projectFileIO.GetFileName()) > UINT32_MAX)
      {
         BasicUI::ShowErrorDialog( *ProjectFramePlacement( &proj ),
            XO("Error Saving Project"),
            XO("The project exceeds the maximum size of 4GB when writing to a FAT32 formatted filesystem."),
            "Error:_Unsuitable_drive"
            );
         return false;
      }
   }

   bool success = projectFileIO.SaveProject(fileName, mLastSavedTracks.get());
   if (!success)
   {
      // Show this error only if we didn't fail reconnection in SaveProject
      // REVIEW: Could HasConnection() be true but SaveProject() still have failed?
      if (!projectFileIO.HasConnection()) {
         using namespace BasicUI;
         ShowErrorDialog( *ProjectFramePlacement( &proj ),
            XO("Error Saving Project"),
            FileException::WriteFailureMessage(fileName),
            "Error:_Disk_full_or_not_writable",
            ErrorDialogOptions{ ErrorDialogType::ModalErrorReport } );
      }
      return false;
   }

   proj.SetProjectName(wxFileName(fileName).GetName());
   projectFileIO.SetProjectTitle();

   UndoManager::Get(proj).StateSaved();
   ProjectStatus::Get(proj).Set(XO("Saved %s").Format(fileName));

   if (mLastSavedTracks)
   {
      mLastSavedTracks->Clear();
   }
   mLastSavedTracks = TrackList::Create(nullptr);

   auto &tracks = TrackList::Get(proj);
   for (auto t : tracks)
      mLastSavedTracks->Add(t->Duplicate(Track::DuplicateOptions{}.Backup()));

   // If we get here, saving the project was successful, so we can DELETE
   // any backup project.
   if (pBackupProject)
      pBackupProject->Discard();

   return true;
}

// This version of SaveAs is invoked only from scripting and does not
// prompt for a file name
bool ProjectFileManager::SaveAs(const FilePath &newFileName, bool addToHistory /*= true*/)
{
   auto &project = mProject;
   auto &projectFileIO = ProjectFileIO::Get( project );

   auto oldFileName = projectFileIO.GetFileName();

   bool bOwnsNewName = !projectFileIO.IsTemporary() && (oldFileName == newFileName);
   //check to see if the NEW project file already exists.
   //We should only overwrite it if this project already has the same name, where the user
   //simply chose to use the save as command although the save command would have the effect.
   if( !bOwnsNewName && wxFileExists(newFileName)) {
      AudacityMessageDialog m(
         nullptr,
         XO("The project was not saved because the file name provided would overwrite another project.\nPlease try again and select an original name."),
         XO("Error Saving Project"),
         wxOK|wxICON_ERROR );
      m.ShowModal();
      return false;
   }

   auto success = DoSave(newFileName, !bOwnsNewName);
   if (success && addToHistory) {
      FileHistory::Global().Append( projectFileIO.GetFileName() );
   }

   return(success);
}

bool ProjectFileManager::SaveAs(bool allowOverwrite /* = false */)
{
   auto &project = mProject;
   auto &projectFileIO = ProjectFileIO::Get( project );
   auto &window = GetProjectFrame( project );
   TitleRestorer Restorer( window, project ); // RAII
   wxFileName filename;
   FilePath defaultSavePath = FileNames::FindDefaultPath(FileNames::Operation::Save);

   if (projectFileIO.IsTemporary()) {
      filename.SetPath(defaultSavePath);
      filename.SetName(project.GetProjectName());
   }
   else {
      filename = projectFileIO.GetFileName();
   }

   // Bug 1304: Set a default file path if none was given.  For Save/SaveAs/SaveCopy
   if( !FileNames::IsPathAvailable( filename.GetPath( wxPATH_GET_VOLUME| wxPATH_GET_SEPARATOR) ) ){
      filename.SetPath(defaultSavePath);
   }

   TranslatableString title = XO("%sSave Project \"%s\" As...")
      .Format( Restorer.sProjNumber, Restorer.sProjName );

   bool bPrompt = (project.mBatchMode == 0) || (projectFileIO.GetFileName().empty());
   FilePath fName;
   bool bOwnsNewName;

   do {
      if (bPrompt) {
         // JKC: I removed 'wxFD_OVERWRITE_PROMPT' because we are checking
         // for overwrite ourselves later, and we disallow it.
         fName = SelectFile(FileNames::Operation::Save,
            title,
            filename.GetPath(),
            filename.GetFullName(),
            wxT("aup3"),
            { FileNames::AudacityProjects },
            wxFD_SAVE | wxRESIZE_BORDER,
            &window);

         if (fName.empty())
            return false;

         filename = fName;
      };

      filename.SetExt(wxT("aup3"));

      if ((!bPrompt || !allowOverwrite) && filename.FileExists()) {
         // Saving a copy of the project should never overwrite an existing project.
         AudacityMessageDialog m(
            nullptr,
            XO("The project was not saved because the file name provided would overwrite another project.\nPlease try again and select an original name."),
            XO("Error Saving Project"),
            wxOK|wxICON_ERROR );
         m.ShowModal();
         return false;
      }

      fName = filename.GetFullPath();

      bOwnsNewName = !projectFileIO.IsTemporary() && ( projectFileIO.GetFileName() == fName );
      // Check to see if the project file already exists, and if it does
      // check that the project file 'belongs' to this project.
      // otherwise, prompt the user before overwriting.
      if (!bOwnsNewName && filename.FileExists()) {
         // Ensure that project of same name is not open in another window.
         // fName is the destination file.
         // mFileName is this project.
         // It is possible for mFileName == fName even when this project is not
         // saved to disk, and we then need to check the destination file is not
         // open in another window.
         int mayOverwrite = ( projectFileIO.GetFileName() == fName ) ? 2 : 1;
         for ( auto p : AllProjects{} ) {
            const wxFileName openProjectName{ ProjectFileIO::Get(*p).GetFileName() };
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
            if (result == wxNO) {
               continue;
            }
            if (result == wxCANCEL) {
               return false;
            }
         }
         else {
            // Overwrite disallowed. The destination project is open in another window.
            AudacityMessageDialog m(
               nullptr,
               XO("The project was not saved because the selected project is open in another window.\nPlease try again and select an original name."),
               XO("Error Saving Project"),
               wxOK|wxICON_ERROR );
            m.ShowModal();
            continue;
         }
      }

      break;
   } while (bPrompt);


   // Pretend that we are closing the project
   if (!bOwnsNewName)
   {
      if (
         ProjectFileIOExtensionRegistry::OnClose(mProject) ==
         OnCloseAction::Veto)
         return false;
   }

   auto success = DoSave(fName, !bOwnsNewName);
   if (success) {
      FileHistory::Global().Append( projectFileIO.GetFileName() );
   }

   return(success);
}

bool ProjectFileManager::SaveCopy(const FilePath &fileName /* = wxT("") */)
{
   auto &project = mProject;
   auto &projectFileIO = ProjectFileIO::Get(project);
   auto &window = GetProjectFrame(project);
   TitleRestorer Restorer(window, project); // RAII
   wxFileName filename = fileName;
   FilePath defaultSavePath = FileNames::FindDefaultPath(FileNames::Operation::Save);

   if (fileName.empty())
   {
      if (projectFileIO.IsTemporary())
      {
         filename.SetPath(defaultSavePath);
      }
      else
      {
         filename = projectFileIO.GetFileName();
      }
   }

   // Bug 1304: Set a default file path if none was given.  For Save/SaveAs/SaveCopy
   if (!FileNames::IsPathAvailable(filename.GetPath(wxPATH_GET_VOLUME | wxPATH_GET_SEPARATOR)))
   {
      filename.SetPath(defaultSavePath);
   }

   TranslatableString title =
      XO("%sSave Copy of Project \"%s\" As...")
         .Format(Restorer.sProjNumber, Restorer.sProjName);

   bool bPrompt = (project.mBatchMode == 0) || (projectFileIO.GetFileName().empty());
   FilePath fName;

   do
   {
      if (bPrompt)
      {
         // JKC: I removed 'wxFD_OVERWRITE_PROMPT' because we are checking
         // for overwrite ourselves later, and we disallow it.
         // Previously we disallowed overwrite because we would have had
         // to DELETE the many smaller files too, or prompt to move them.
         // Maybe we could allow it now that we have aup3 format?
         fName = SelectFile(FileNames::Operation::Export,
                                       title,
                                       filename.GetPath(),
                                       filename.GetFullName(),
                                       wxT("aup3"),
                                       { FileNames::AudacityProjects },
                                       wxFD_SAVE | wxRESIZE_BORDER,
                                       &window);

         if (fName.empty())
         {
            return false;
         }

         filename = fName;
      };

      filename.SetExt(wxT("aup3"));

      if (TempDirectory::FATFilesystemDenied(filename.GetFullPath(), XO("Projects cannot be saved to FAT drives.")))
      {
         if (project.mBatchMode)
         {
            return false;
         }

         continue;
      }

      if (filename.FileExists())
      {
         // Saving a copy of the project should never overwrite an existing project.
         AudacityMessageDialog m(nullptr,
                                 XO("Saving a copy must not overwrite an existing saved project.\nPlease try again and select an original name."),
                                 XO("Error Saving Copy of Project"),
                                 wxOK | wxICON_ERROR);
         m.ShowModal();

         if (project.mBatchMode)
         {
            return false;
         }

         continue;
      }

      wxULongLong fileSize = wxFileName::GetSize(projectFileIO.GetFileName());

      wxDiskspaceSize_t freeSpace;
      if (wxGetDiskSpace(FileNames::AbbreviatePath(filename.GetFullPath()), NULL, &freeSpace))
      {
         if (freeSpace.GetValue() <= fileSize.GetValue())
         {
            BasicUI::ShowErrorDialog( *ProjectFramePlacement( &project ),
               XO("Insufficient Disk Space"),
               XO("The project size exceeds the available free space on the target disk.\n\n"
                  "Please select a different disk with more free space."),
               "Error:_Unsuitable_drive"
               );

            continue;
         }
      }

      if (FileNames::IsOnFATFileSystem(filename.GetFullPath()))
      {
         if (fileSize > UINT32_MAX)
         {
            BasicUI::ShowErrorDialog( *ProjectFramePlacement( &project ),
               XO("Error Saving Project"),
               XO("The project exceeds the maximum size of 4GB when writing to a FAT32 formatted filesystem."),
               "Error:_Unsuitable_drive"
               );

            if (project.mBatchMode)
            {
               return false;
            }

            continue;
         }
      }

      fName = filename.GetFullPath();
      break;
   } while (bPrompt);

   if (!projectFileIO.SaveCopy(fName))
   {
      auto msg = FileException::WriteFailureMessage(fName);
      AudacityMessageDialog m(
         nullptr, msg, XO("Error Saving Project"), wxOK | wxICON_ERROR);

      m.ShowModal();

      return false;
   }

   return true;
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
   if (!projectFileIO.IsModified()) {
      sOldFilename = projectFileIO.GetFileName();
   }

   // MY: If the project file already exists then bail out
   // and send populate the message string (pointer) so
   // we can tell the user what went wrong.
   if (wxFileExists(sNewFileName)) {
      return false;
   }

   auto success = DoSave(sNewFileName, true);

   if (success) {
      FileHistory::Global().Append( projectFileIO.GetFileName() );
   }

   return success;
}

void ProjectFileManager::CompactProjectOnClose()
{
   auto &project = mProject;
   auto &projectFileIO = ProjectFileIO::Get(project);

   // Lock all blocks in all tracks of the last saved version, so that
   // the sample blocks aren't deleted from the database when we destroy the
   // sample block objects in memory.
   if (mLastSavedTracks)
   {
      for (auto wt : mLastSavedTracks->Any<WaveTrack>())
         WaveTrackUtilities::CloseLock(*wt);

      // Attempt to compact the project
      projectFileIO.Compact({ mLastSavedTracks.get() });

      if (
         !projectFileIO.WasCompacted() &&
         UndoManager::Get(project).UnsavedChanges())
      {
         // If compaction failed, we must do some work in case of close
         // without save.  Don't leave the document blob from the last
         // push of undo history, when that undo state may get purged
         // with deletion of some new sample blocks.
         // REVIEW: UpdateSaved() might fail too.  Do we need to test
         // for that and report it?
         projectFileIO.UpdateSaved(mLastSavedTracks.get());
      }
   }
}

bool ProjectFileManager::OpenProject()
{
   auto &project = mProject;
   auto &projectFileIO = ProjectFileIO::Get(project);

   return projectFileIO.OpenProject();
}

bool ProjectFileManager::OpenNewProject()
{
   auto &project = mProject;
   auto &projectFileIO = ProjectFileIO::Get(project);

   bool bOK = OpenProject();
   if( !bOK )
   {
       auto tmpdir = wxFileName(TempDirectory::UnsavedProjectFileName()).GetPath();

       UnwritableLocationErrorDialog dlg(nullptr, tmpdir);
       dlg.ShowModal();
   }
   return bOK;
}

void ProjectFileManager::CloseProject()
{
   auto &project = mProject;
   auto &projectFileIO = ProjectFileIO::Get(project);

   projectFileIO.CloseProject();

   // Blocks were locked in CompactProjectOnClose, so DELETE the data structure so that
   // there's no memory leak.
   if (mLastSavedTracks)
   {
      mLastSavedTracks->Clear();
      mLastSavedTracks.reset();
   }
}

// static method, can be called outside of a project
wxArrayString ProjectFileManager::ShowOpenDialog(FileNames::Operation op,
   const FileNames::FileType &extraType )
{
   // Construct the filter
   const auto fileTypes = Importer::Get().GetFileTypes( extraType );

   // Retrieve saved path
   auto path = FileNames::FindDefaultPath(op);

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

      // Remember the directory
      FileNames::UpdateDefaultPath(op, ::wxPathOnly(dlog.GetPath()));
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
         return newProjPathName.SameAs(wxFileNameWrapper{ ProjectFileIO::Get(*ptr).GetFileName() });
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

AudacityProject *ProjectFileManager::OpenFile( const ProjectChooserFn &chooser,
   const FilePath &fileNameArg, bool addtohistory)
{
   // On Win32, we may be given a short (DOS-compatible) file name on rare
   // occasions (e.g. stuff like "C:\PROGRA~1\AUDACI~1\PROJEC~1.AUP"). We
   // convert these to long file name first.
   auto fileName = PlatformCompatibility::GetLongFileName(fileNameArg);

   // Make sure it isn't already open.
   // Vaughan, 2011-03-25: This was done previously in AudacityProject::OpenFiles()
   //    and AudacityApp::MRUOpen(), but if you open an aup file by double-clicking it
   //    from, e.g., Win Explorer, it would bypass those, get to here with no check,
   //    then open a NEW project from the same data with no warning.
   //    This was reported in http://bugzilla.audacityteam.org/show_bug.cgi?id=137#c17,
   //    but is not really part of that bug. Anyway, prevent it!
   if (IsAlreadyOpen(fileName))
      return nullptr;

   // Data loss may occur if users mistakenly try to open ".aup3.bak" files
   // left over from an unsuccessful save or by previous versions of Audacity.
   // So we always refuse to open such files.
   if (fileName.Lower().EndsWith(wxT(".aup3.bak")))
   {
      AudacityMessageBox(
         XO(
"You are trying to open an automatically created backup file.\nDoing this may result in severe data loss.\n\nPlease open the actual Audacity project file instead."),
         XO("Warning - Backup File Detected"),
         wxOK | wxCENTRE,
         nullptr);
      return nullptr;
   }

   if (!::wxFileExists(fileName)) {
      AudacityMessageBox(
         XO("Could not open file: %s").Format( fileName ),
         XO("Error Opening File"),
         wxOK | wxCENTRE,
         nullptr);
      return nullptr;
   }

   // Following block covers cases other than a project file:
   {
      wxFFile ff(fileName, wxT("rb"));

      auto cleanup = finally([&]
      {
         if (ff.IsOpened())
         {
            ff.Close();
         }
      });

      if (!ff.IsOpened()) {
         AudacityMessageBox(
            XO("Could not open file: %s").Format( fileName ),
            XO("Error opening file"),
            wxOK | wxCENTRE,
            nullptr);
         return nullptr;
      }

      char buf[7];
      auto numRead = ff.Read(buf, 6);
      if (numRead != 6) {
         AudacityMessageBox(
            XO("File may be invalid or corrupted: \n%s").Format( fileName ),
            XO("Error Opening File or Project"),
            wxOK | wxCENTRE,
            nullptr);
         return nullptr;
      }

      if (wxStrncmp(buf, "SQLite", 6) != 0)
      {
         // Not a database
#ifdef EXPERIMENTAL_DRAG_DROP_PLUG_INS
         // Is it a plug-in?
         if (PluginManager::Get().DropFile(fileName)) {
            MenuCreator::RebuildAllMenuBars();
            // Plug-in installation happened, not really opening of a file,
            // so return null
            return nullptr;
         }
#endif
         auto &project = chooser(false);
         // Undo history is incremented inside this:
         if (Get(project).Import(fileName))
         {
            // Undo history is incremented inside this:
            // Bug 2743: Don't zoom with lof.
            if (!fileName.AfterLast('.').IsSameAs(wxT("lof"), false))
               Viewport::Get(project).ZoomFitHorizontallyAndShowTrack(nullptr);
            return &project;
         }
         return nullptr;
      }
   }

   // Disallow opening of .aup3 project files from FAT drives, but only such
   // files, not importable types.  (Bug 2800)
   if (TempDirectory::FATFilesystemDenied(fileName,
      XO("Project resides on FAT formatted drive.\n"
        "Copy it to another drive to open it.")))
   {
      return nullptr;
   }

   auto &project = chooser(true);
   return Get(project).OpenProjectFile(fileName, addtohistory);
}

void ProjectFileManager::FixTracks(TrackList& tracks,
   const std::function<void(const TranslatableString&)>& onError,
   const std::function<void(const TranslatableString&)>& onUnlink)
{
   // This is successively assigned the left member of each pair that
   // becomes unlinked
   Track::Holder unlinkedTrack;
   // Beware iterator invalidation, because stereo channels get zipped,
   // replacing WaveTracks
   for (auto iter = tracks.begin(); iter != tracks.end();) {
      auto t = (*iter++)->SharedPointer();
      const auto linkType = t->GetLinkType();
      // Note, the next function may have an important upgrading side effect,
      // and return no error; or it may find a real error and repair it, but
      // that repaired track won't be used because opening will fail.
      if (!t->LinkConsistencyFix()) {
         onError(XO("A channel of a stereo track was missing."));
         unlinkedTrack = nullptr;
      }
      if (!unlinkedTrack) {
         if (linkType != ChannelGroup::LinkType::None &&
            t->NChannels() == 1) {
            // The track became unlinked.
            // It should NOT have been replaced with a "zip"
            assert(t->GetOwner().get() == &tracks);
            // Wait until LinkConsistencyFix is called on the second track
            unlinkedTrack = t;
            // Fix the iterator, which skipped the right channel before the
            // unlinking
            iter = tracks.Find(t.get());
            ++iter;
         }
      }
      else {
         //Not an elegant way to deal with stereo wave track linking
         //compatibility between versions
         if (const auto left = dynamic_cast<WaveTrack*>(unlinkedTrack.get())) {
            if (const auto right = dynamic_cast<WaveTrack*>(t.get())) {
               // As with the left, it should not have vanished from the list
               assert(right->GetOwner().get() == &tracks);
               left->SetPan(-1.0f);
               right->SetPan(1.0f);
               RealtimeEffectList::Get(*left).Clear();
               RealtimeEffectList::Get(*right).Clear();

               if(left->GetRate() != right->GetRate())
                  //i18n-hint: explains why opened project was auto-modified
                  onUnlink(XO("This project contained stereo tracks with different sample rates per channel."));
               if(left->GetSampleFormat() != right->GetSampleFormat())
                  //i18n-hint: explains why opened project was auto-modified
                  onUnlink(XO("This project contained stereo tracks with different sample formats in channels."));
               //i18n-hint: explains why opened project was auto-modified
               onUnlink(XO("This project contained stereo tracks with non-aligned content."));
            }
         }
         unlinkedTrack = nullptr;
      }

      if (const auto message = t->GetErrorOpening()) {
         wxLogWarning(
            wxT("Track %s had error reading clip values from project file."),
            t->GetName());
         onError(*message);
      }
   }
}

AudacityProject *ProjectFileManager::OpenProjectFile(
   const FilePath &fileName, bool addtohistory)
{
   // Allow extensions to update the project before opening it.
   if (ProjectFileIOExtensionRegistry::OnOpen(
          mProject, audacity::ToUTF8(fileName)) == OnOpenAction::Cancel)
      return nullptr;

   auto &project = mProject;
   auto &history = ProjectHistory::Get( project );
   auto &tracks = TrackList::Get( project );
   auto &trackPanel = TrackPanel::Get( project );
   auto &projectFileIO = ProjectFileIO::Get( project );
   auto &viewport = Viewport::Get( project );

   auto results = ReadProjectFile( fileName );
   const bool bParseSuccess = results.parseSuccess;
   const auto &errorStr = results.errorString;
   const bool err = results.trackError;

   if (bParseSuccess && !err) {
      Viewport::Get(project).ReinitScrollbars();

      ProjectHistory::Get( project ).InitialState();
      TrackFocus::Get(project).Set(*tracks.begin());
      viewport.HandleResize();
      trackPanel.Refresh(false);

      // ? Old rationale in this comment no longer applies in 3.0.0, with no
      // more on-demand loading:
      trackPanel.Update(); // force any repaint to happen now,
      // else any asynch calls into the blockfile code will not have
      // finished logging errors (if any) before the call to ProjectFSCK()

      if (addtohistory)
         FileHistory::Global().Append(fileName);
   }

   if (bParseSuccess && !err) {
      if (projectFileIO.IsRecovered())
      {
         // PushState calls AutoSave(), so no longer need to do so here.
         history.PushState(XO("Project was recovered"), XO("Recover"));
      }
      return &project;
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
      for (auto pTrack : tracks.Any<WaveTrack>())
         WaveTrackUtilities::CloseLock(*pTrack);

      tracks.Clear(); //tracks.Clear(true);

      wxLogError(wxT("Could not parse file \"%s\". \nError: %s"), fileName, errorStr.Debug());

      projectFileIO.ShowError( *ProjectFramePlacement(&project),
         XO("Error Opening Project"),
         errorStr,
         results.helpUrl);

      return nullptr;
   }
}

void
ProjectFileManager::AddImportedTracks(const FilePath &fileName,
   TrackHolders &&newTracks)
{
   auto &project = mProject;
   auto &history = ProjectHistory::Get( project );
   auto &projectFileIO = ProjectFileIO::Get( project );
   auto &tracks = TrackList::Get( project );

   std::vector<Track*> results;

   SelectUtilities::SelectNone( project );

   wxFileName fn(fileName);

   bool initiallyEmpty = tracks.empty();
   double newRate = 0;
   wxString trackNameBase = fn.GetName();
   int i = -1;

   // Fix the bug 2109.
   // In case the project had soloed tracks before importing,
   // all newly imported tracks are muted.
   const bool projectHasSolo =
      !(tracks.Any<PlayableTrack>() + &PlayableTrack::GetSolo).empty();
   if (projectHasSolo) {
      for (auto &group : newTracks)
         if (auto pTrack = dynamic_cast<PlayableTrack*>(group.get()))
            pTrack->SetMute(true);
   }

   for (auto &group : newTracks) {
      if (auto pTrack = dynamic_cast<WaveTrack*>(group.get()))
         results.push_back(pTrack);
      tracks.Add(group);
   }
   newTracks.clear();

   // Now name them

   // Add numbers to track names only if there is more than one (mono or stereo)
   // track (not necessarily, more than one channel)
   const bool useSuffix = results.size() > 1;

   for (const auto &newTrack : results) {
      ++i;
      newTrack->SetSelected(true);
      if (useSuffix)
         //i18n-hint Name default name assigned to a clip on track import
         newTrack->SetName(XC("%s %d", "clip name template")
            .Format(trackNameBase, i + 1).Translation());
      else
         newTrack->SetName(trackNameBase);

      newTrack->TypeSwitch([&](WaveTrack &wt) {
         if (newRate == 0)
            newRate = wt.GetRate();
         const auto trackName = wt.GetName();
         for (const auto &interval : wt.Intervals())
            interval->SetName(trackName);
      });
   }

   history.PushState(XO("Imported '%s'").Format( fileName ),
       XO("Import"));

#if defined(__WXGTK__)
   // See bug #1224
   // The track panel hasn't been fully created, so ZoomFitHorizontally() will not give
   // expected results due to a window width of zero.  Should be safe to yield here to
   // allow the creation to complete.  If this becomes a problem, it "might" be possible
   // to queue a dummy event to trigger ZoomFitHorizontally().
   wxEventLoopBase::GetActive()->YieldFor(wxEVT_CATEGORY_UI | wxEVT_CATEGORY_USER_INPUT);
#endif

   // If the project was clean and temporary (not permanently saved), then set
   // the filename to the just imported path.
   if (initiallyEmpty && projectFileIO.IsTemporary()) {
      project.SetProjectName(fn.GetName());
      project.SetInitialImportPath(fn.GetPath());
      projectFileIO.SetProjectTitle();
   }

   // Moved this call to higher levels to prevent flicker redrawing everything on each file.
   //   HandleResize();
}

namespace {
bool ImportProject(AudacityProject &dest, const FilePath &fileName)
{
   InvisibleTemporaryProject temp;
   auto &project = temp.Project();

   auto &projectFileIO = ProjectFileIO::Get(project);
   if (!projectFileIO.LoadProject(fileName, false))
      return false;
   auto &srcTracks = TrackList::Get(project);
   auto &destTracks = TrackList::Get(dest);
   for (const Track *pTrack : srcTracks)
      pTrack->PasteInto(dest, destTracks);
   Tags::Get(dest).Merge(Tags::Get(project));

   return true;
}

class ImportProgress final
   : public ImportProgressListener
{
   wxWeakRef<AudacityProject> mProject;
public:

   ImportProgress(AudacityProject& project)
      : mProject(&project)
   {

   }

   bool OnImportFileOpened(ImportFileHandle& importFileHandle) override
   {
      mImportFileHandle = &importFileHandle;
      // File has more than one stream - display stream selector
      if (importFileHandle.GetStreamCount() > 1)
      {
         ImportStreamDialog ImportDlg(&importFileHandle, NULL, -1, XO("Select stream(s) to import"));

         if (ImportDlg.ShowModal() == wxID_CANCEL)
            return false;
      }
      // One stream - import it by default
      else
         importFileHandle.SetStreamUsage(0,TRUE);
      return true;
   }

   void OnImportProgress(double progress) override
   {
      constexpr double ProgressSteps { 1000.0 };
      if(!mProgressDialog)
      {
         wxFileName ff( mImportFileHandle->GetFilename() );
         auto title = XO("Importing %s").Format(  mImportFileHandle->GetFileDescription() );
         mProgressDialog = BasicUI::MakeProgress(title, Verbatim(ff.GetFullName()));
      }
      auto result = mProgressDialog->Poll(progress * ProgressSteps, ProgressSteps);
      if(result == BasicUI::ProgressResult::Cancelled)
         mImportFileHandle->Cancel();
      else if(result == BasicUI::ProgressResult::Stopped)
         mImportFileHandle->Stop();
   }

   void OnImportResult(ImportResult result) override
   {
      mProgressDialog.reset();
      if(result == ImportResult::Error)
      {
         auto message = mImportFileHandle->GetErrorMessage();
         if(!message.empty())
         {
            AudacityMessageBox(message, XO("Import"), wxOK | wxCENTRE | wxICON_ERROR,
                               mProject ? &GetProjectFrame(*mProject) : nullptr);
         }
      }
   }

private:

   ImportFileHandle* mImportFileHandle {nullptr};
   std::unique_ptr<BasicUI::ProgressDialog> mProgressDialog;
};
} // namespace

bool ProjectFileManager::Import(const FilePath& fileName, bool addToHistory)
{
   wxArrayString fileNames;
   fileNames.Add(fileName);
   return Import(std::move(fileNames), addToHistory);
}

bool ProjectFileManager::Import(wxArrayString fileNames, bool addToHistory)
{
   fileNames.Sort(FileNames::CompareNoCase);
   if (!ProjectFileManager::Get(mProject).ImportAndRunTempoDetection(
          std::vector<wxString> { fileNames.begin(), fileNames.end() },
          addToHistory))
      return false;
   // Last track in the project is the one that was just added. Use it for
   // focus, etc.
   Track* lastTrack = nullptr;
   const auto range = TrackList::Get(mProject).Any<Track>();
   assert(!range.empty());
   if(range.empty())
      return false;
   lastTrack = *(range.rbegin());
   BasicUI::CallAfter([wTrack = lastTrack->weak_from_this(), wProject = mProject.weak_from_this()] {
      const auto project = wProject.lock();
      const auto track = wTrack.lock();
      if (!project || !track)
         return;
      auto& viewPort = Viewport::Get(*project);
      TrackFocus::Get(*project).Set(track.get(), true);
      viewPort.ZoomFitHorizontally();
      viewPort.ShowTrack(*track);
      viewPort.HandleResize(); // Adjust scrollers for NEW track sizes.
   });
   return true;
}

namespace
{
std::vector<std::shared_ptr<MIR::AnalyzedAudioClip>> RunTempoDetection(
   const std::vector<std::shared_ptr<ClipMirAudioReader>>& readers,
   const MIR::ProjectInterface& project, bool projectWasEmpty)
{
   const auto isBeatsAndMeasures = project.ViewIsBeatsAndMeasures();
   const auto projectTempo = project.GetTempo();

   using namespace BasicUI;
   auto progress = MakeProgress(
      XO("Music Information Retrieval"), XO("Analyzing imported audio"),
      ProgressShowCancel);
   auto count = 0;
   const auto reportProgress = [&](double progressFraction) {
      const auto result = progress->Poll(
         (count + progressFraction) / readers.size() * 1000, 1000);
      if (result != ProgressResult::Success)
         throw UserException {};
   };

   std::vector<std::shared_ptr<MIR::AnalyzedAudioClip>> analyzedClips;
   analyzedClips.reserve(readers.size());
   std::transform(
      readers.begin(), readers.end(), std::back_inserter(analyzedClips),
      [&](const std::shared_ptr<ClipMirAudioReader>& reader) {
         const MIR::ProjectSyncInfoInput input {
            *reader,      reader->filename, reader->tags,       reportProgress,
            projectTempo, projectWasEmpty,  isBeatsAndMeasures,
         };
         auto syncInfo = MIR::GetProjectSyncInfo(input);
         ++count;
         return std::make_shared<AnalyzedWaveClip>(reader, syncInfo);
      });
   return analyzedClips;
}
} // namespace

bool ProjectFileManager::ImportAndRunTempoDetection(
   const std::vector<FilePath>& fileNames, bool addToHistory)
{
   const auto projectWasEmpty =
      TrackList::Get(mProject).Any<WaveTrack>().empty();
   std::vector<std::shared_ptr<ClipMirAudioReader>> resultingReaders;
   const auto success = std::all_of(
      fileNames.begin(), fileNames.end(), [&](const FilePath& fileName) {
         std::shared_ptr<ClipMirAudioReader> resultingReader;
         const auto success = DoImport(fileName, addToHistory, resultingReader);
         if (success && resultingReader)
            resultingReaders.push_back(std::move(resultingReader));
         return success;
      });
   // At the moment, one failing import doesn't revert the project state, hence
   // we still run the analysis on what was successfully imported.
   // TODO implement reverting of the project state on failure.
   if (!resultingReaders.empty())
   {
      const auto pProj = mProject.shared_from_this();
      BasicUI::CallAfter([=] {
         AudacityMirProject mirInterface { *pProj };
         const auto analyzedClips =
            RunTempoDetection(resultingReaders, mirInterface, projectWasEmpty);
         MIR::SynchronizeProject(analyzedClips, mirInterface, projectWasEmpty);
      });
   }
   return success;
}

// If pNewTrackList is passed in non-NULL, it gets filled with the pointers to NEW tracks.
bool ProjectFileManager::DoImport(
   const FilePath& fileName, bool addToHistory,
   std::shared_ptr<ClipMirAudioReader>& resultingReader)
{
   auto &project = mProject;
   auto &projectFileIO = ProjectFileIO::Get(project);
   auto oldTags = Tags::Get( project ).shared_from_this();
   bool initiallyEmpty = TrackList::Get(project).empty();
   TrackHolders newTracks;
   TranslatableString errorMessage;

#ifdef EXPERIMENTAL_IMPORT_AUP3
   // Handle AUP3 ("project") files directly
   if (fileName.AfterLast('.').IsSameAs(wxT("aup3"), false)) {
      if (ImportProject(project, fileName)) {
         auto &history = ProjectHistory::Get(project);

         // If the project was clean and temporary (not permanently saved), then set
         // the filename to the just imported path.
         if (initiallyEmpty && projectFileIO.IsTemporary()) {
            wxFileName fn(fileName);
            project.SetProjectName(fn.GetName());
            project.SetInitialImportPath(fn.GetPath());
            projectFileIO.SetProjectTitle();
         }

         history.PushState(XO("Imported '%s'").Format(fileName), XO("Import"));

         if (addToHistory) {
            FileHistory::Global().Append(fileName);
         }
      }
      else {
         errorMessage = projectFileIO.GetLastError();
         if (errorMessage.empty()) {
            errorMessage = XO("Failed to import project");
         }

         // Additional help via a Help button links to the manual.
         ShowErrorDialog( *ProjectFramePlacement(&project),
            XO("Error Importing"),
            errorMessage, wxT("Importing_Audio"));
      }

      return false;
   }
#endif

   {
      // Backup Tags, before the import.  Be prepared to roll back changes.
      bool committed = false;
      auto cleanup = finally([&]{
         if ( !committed )
            Tags::Set( project, oldTags );
      });
      auto newTags = oldTags->Duplicate();
      Tags::Set( project, newTags );

#ifndef EXPERIMENTAL_IMPORT_AUP3
      // Handle AUP3 ("project") files specially
      if (fileName.AfterLast('.').IsSameAs(wxT("aup3"), false)) {
         BasicUI::ShowErrorDialog( *ProjectFramePlacement(&project),
            XO("Error Importing"),
            XO( "Cannot import AUP3 format.  Use File > Open instead"),
            wxT("File_Menu"));
         return false;
      }
#endif

      ImportProgress importProgress(project);
      std::optional<LibFileFormats::AcidizerTags> acidTags;
      bool success = Importer::Get().Import(
         project, fileName, &importProgress, &WaveTrackFactory::Get(project),
         newTracks, newTags.get(), acidTags, errorMessage);
      if (!errorMessage.empty()) {
         // Error message derived from Importer::Import
         // Additional help via a Help button links to the manual.
         BasicUI::ShowErrorDialog( *ProjectFramePlacement(&project),
            XO("Error Importing"), errorMessage, wxT("Importing_Audio"));
      }
      if (!success)
         return false;

      const auto projectTempo = ProjectTimeSignature::Get(project).GetTempo();
      for (auto track : newTracks)
         DoProjectTempoChange(*track, projectTempo);

      if (newTracks.size() == 1)
      {
         const auto waveTrack = dynamic_cast<WaveTrack*>(newTracks[0].get());
         // Also check that the track has a clip, as protection against empty
         // file import.
         if (waveTrack && !waveTrack->GetClipInterfaces().empty())
            resultingReader.reset(new ClipMirAudioReader {
               std::move(acidTags), fileName.ToStdString(),
               *waveTrack });
      }

      if (addToHistory) {
         FileHistory::Global().Append(fileName);
      }

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

   // Handle AUP ("legacy project") files directly
   if (fileName.AfterLast('.').IsSameAs(wxT("aup"), false)) {
      // If the project was clean and temporary (not permanently saved), then set
      // the filename to the just imported path.
      if (initiallyEmpty && projectFileIO.IsTemporary()) {
         wxFileName fn(fileName);
         project.SetProjectName(fn.GetName());
         project.SetInitialImportPath(fn.GetPath());
         projectFileIO.SetProjectTitle();
      }

      auto &history = ProjectHistory::Get( project );

      history.PushState(XO("Imported '%s'").Format( fileName ), XO("Import"));

      return true;
   }

   // PRL: Undo history is incremented inside this:
   AddImportedTracks(fileName, std::move(newTracks));

   return true;
}

#include "Clipboard.h"
#include "ShuttleGui.h"
#include "HelpSystem.h"

// Compact dialog
namespace {
class CompactDialog : public wxDialogWrapper
{
public:
   CompactDialog(TranslatableString text)
   :  wxDialogWrapper(nullptr, wxID_ANY, XO("Compact Project"))
   {
      ShuttleGui S(this, eIsCreating);

      S.StartVerticalLay(true);
      {
         S.AddFixedText(text, false, 500);

         S.AddStandardButtons(eYesButton | eNoButton | eHelpButton);
      }
      S.EndVerticalLay();

      FindWindowById(wxID_YES, this)->Bind(wxEVT_BUTTON, &CompactDialog::OnYes, this);
      FindWindowById(wxID_NO, this)->Bind(wxEVT_BUTTON, &CompactDialog::OnNo, this);
      FindWindowById(wxID_HELP, this)->Bind(wxEVT_BUTTON, &CompactDialog::OnGetURL, this);

      Layout();
      Fit();
      Center();
   }

   void OnYes(wxCommandEvent &WXUNUSED(evt))
   {
      EndModal(wxYES);
   }

   void OnNo(wxCommandEvent &WXUNUSED(evt))
   {
      EndModal(wxNO);
   }

   void OnGetURL(wxCommandEvent &WXUNUSED(evt))
   {
      HelpSystem::ShowHelp(this, L"File_Menu:_Compact_Project", true);
   }
};
}

void ProjectFileManager::Compact()
{
   auto &project = mProject;
   auto &undoManager = UndoManager::Get(project);
   auto &clipboard = Clipboard::Get();
   auto &projectFileIO = ProjectFileIO::Get(project);
   bool isBatch = project.mBatchMode > 0;

   // Purpose of this is to remove the -wal file.
   projectFileIO.ReopenProject();

   auto savedState = undoManager.GetSavedState();
   const auto currentState = undoManager.GetCurrentState();
   if (savedState < 0) {
      undoManager.StateSaved();
      savedState = undoManager.GetSavedState();
      if (savedState < 0) {
         wxASSERT(false);
         savedState = 0;
      }
   }
   const auto least = std::min<size_t>(savedState, currentState);
   const auto greatest = std::max<size_t>(savedState, currentState);
   std::vector<const TrackList*> trackLists;
   auto fn = [&](const UndoStackElem& elem) {
      if (auto pTracks = UndoTracks::Find(elem))
         trackLists.push_back(pTracks);
   };
   undoManager.VisitStates(fn, least, 1 + least);
   if (least != greatest)
      undoManager.VisitStates(fn, greatest, 1 + greatest);

   int64_t total = projectFileIO.GetTotalUsage();
   int64_t used = projectFileIO.GetCurrentUsage(trackLists);

   auto before = wxFileName::GetSize(projectFileIO.GetFileName());

   CompactDialog dlg(
         XO("Compacting this project will free up disk space by removing unused bytes within the file.\n\n"
            "There is %s of free disk space and this project is currently using %s.\n"
            "\n"
            "If you proceed, the current Undo/Redo History and clipboard contents will be discarded "
            "and you will recover approximately %s of disk space.\n"
            "\n"
            "Do you want to continue?")
         .Format(Internat::FormatSize(projectFileIO.GetFreeDiskSpace()),
                  Internat::FormatSize(before.GetValue()),
                  Internat::FormatSize(total - used)));
   if (isBatch || dlg.ShowModal() == wxYES)
   {
      // We can remove redo states, if they are after the saved state.
      undoManager.RemoveStates(1 + greatest, undoManager.GetNumStates());

      // We can remove all states between the current and the last saved.
      if (least < greatest)
         undoManager.RemoveStates(least + 1, greatest);

      // We can remove all states before the current and the last saved.
      undoManager.RemoveStates(0, least);

      // And clear the clipboard, if needed
      if (&mProject == clipboard.Project().lock().get())
         clipboard.Clear();

      // Refresh the before space usage since it may have changed due to the
      // above actions.
      auto before = wxFileName::GetSize(projectFileIO.GetFileName());

      projectFileIO.Compact(trackLists, true);

      auto after = wxFileName::GetSize(projectFileIO.GetFileName());

      if (!isBatch)
      {
         AudacityMessageBox(
            XO("Compacting actually freed %s of disk space.")
            .Format(Internat::FormatSize((before - after).GetValue())),
            XO("Compact Project"));
      }

      undoManager.RenameState( undoManager.GetCurrentState(),
         XO("Compacted project file"),
         XO("Compact") );
   }
}

static void RefreshAllTitles(bool bShowProjectNumbers )
{
   for ( auto pProject : AllProjects{} ) {
      if ( !GetProjectFrame( *pProject ).IsIconized() ) {
         ProjectFileIO::Get( *pProject ).SetProjectTitle(
            bShowProjectNumbers ? pProject->GetProjectNumber() : -1 );
      }
   }
}

TitleRestorer::TitleRestorer(
   wxTopLevelWindow &window, AudacityProject &project )
{
   if( window.IsIconized() )
      window.Restore();
   window.Raise(); // May help identifying the window on Mac

   // Construct this project's name and number.
   sProjName = project.GetProjectName();
   if ( sProjName.empty() ) {
      sProjName = _("<untitled>");
      UnnamedCount = std::count_if(
         AllProjects{}.begin(), AllProjects{}.end(),
         []( const AllProjects::value_type &ptr ){
            return ptr->GetProjectName().empty();
         }
      );
      if ( UnnamedCount > 1 ) {
         sProjNumber.Printf(
            _("[Project %02i] "), project.GetProjectNumber() + 1 );
         RefreshAllTitles( true );
      }
   }
   else
      UnnamedCount = 0;
}

TitleRestorer::~TitleRestorer() {
   if( UnnamedCount > 1 )
      RefreshAllTitles( false );
}
