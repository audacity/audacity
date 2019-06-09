/**********************************************************************

Audacity: A Digital Audio Editor

ProjectFileManager.h

Paul Licameli split from AudacityProject.h

**********************************************************************/

#ifndef __AUDACITY_PROJECT_FILE_MANAGER__
#define __AUDACITY_PROJECT_FILE_MANAGER__

#include <memory>

#include "ClientData.h" // to inherit

class wxString;
class wxFileName;
class AudacityProject;
class TrackList;

class ProjectFileManager final
   : public ClientData::Base
{
public:
   static ProjectFileManager &Get( AudacityProject &project );
   static const ProjectFileManager &Get( const AudacityProject &project );

   explicit ProjectFileManager( AudacityProject &project );
   ~ProjectFileManager();

   struct ReadProjectResults
   {
      bool decodeError;
      bool parseSuccess;
      bool trackError;
      wxString errorString;
   };
   ReadProjectResults ReadProjectFile( const FilePath &fileName );

   void EnqueueODTasks();

   // To be called when closing a project that has been saved, so that
   // block files are not erased
   void CloseLock();

   bool Save();
   bool SaveAs(bool bWantSaveCopy = false, bool bLossless = false);
   bool SaveAs(const wxString & newFileName, bool bWantSaveCopy = false,
      bool addToHistory = true);
   // strProjectPathName is full path for aup except extension
   bool SaveFromTimerRecording( wxFileName fnFile );

   void Reset();

   void SetImportedDependencies( bool value ) { mImportedDependencies = value; }

private:
   // Push names of NEW export files onto the path list
   bool SaveCopyWaveTracks(const FilePath & strProjectPathName,
      bool bLossless, FilePaths &strOtherNamesArray);
   bool DoSave(bool fromSaveAs, bool bWantSaveCopy, bool bLossless = false);

   AudacityProject &mProject;

   std::shared_ptr<TrackList> mLastSavedTracks;
   
   // Dependencies have been imported and a warning should be shown on save
   bool mImportedDependencies{ false };
};

#endif
