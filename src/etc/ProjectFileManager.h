/**********************************************************************

Audacity: A Digital Audio Editor

ProjectFileManager.h

Paul Licameli split from AudacityProject.h

**********************************************************************/

#ifndef __AUDACITY_PROJECT_FILE_MANAGER__
#define __AUDACITY_PROJECT_FILE_MANAGER__

#include <memory>
#include <vector>

#include "ClientData.h" // to inherit
#include "FileNames.h" // for FileType

class wxString;
class wxFileName;
class AudacityProject;
class ImportXMLTagHandler;
class RecordingRecoveryHandler;
class Track;
class TrackList;
class WaveTrack;
class XMLTagHandler;
namespace ProjectFileIORegistry{ struct Entry; }

using WaveTrackArray = std::vector < std::shared_ptr < WaveTrack > >;
using TrackHolders = std::vector< WaveTrackArray >;

class ProjectFileManager final
   : public ClientData::Base
{
public:
   static ProjectFileManager &Get( AudacityProject &project );
   static const ProjectFileManager &Get( const AudacityProject &project );

   explicit ProjectFileManager( AudacityProject &project );
   ProjectFileManager( const ProjectFileManager & ) PROHIBITED;
   ProjectFileManager &operator=( const ProjectFileManager & ) PROHIBITED;
   ~ProjectFileManager();

   struct ReadProjectResults
   {
      bool decodeError;
      bool parseSuccess;
      bool trackError;
      TranslatableString errorString;
      wxString helpUrl;
   };
   ReadProjectResults ReadProjectFile( const FilePath &fileName );

   static unsigned int GetODFlags( const WaveTrack &track );
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

   /** @brief Show an open dialogue for opening audio files, and possibly other
    * sorts of files.
    *
    * The file type filter will automatically contain:
    * - "All files" with any extension or none,
    * - "All supported files" based on the file formats supported in this
    *   build of Audacity,
    * - All of the individual formats specified by the importer plug-ins which
    *   are built into this build of Audacity, each with the relevant file
    *   extensions for that format.
    * The dialogue will start in the DefaultOpenPath directory read from the
    * preferences, failing that the working directory. The file format filter
    * will be set to the DefaultOpenType from the preferences, failing that
    * the first format specified in the dialogue. These two parameters will
    * be saved to the preferences once the user has chosen a file to open.
    * @param extraType Specify an additional format to allow opening in this
    * dialogue.
    * @return Array of file paths which the user selected to open (multiple
    * selections allowed).
    */
   static wxArrayString ShowOpenDialog(
      const FileNames::FileType &extraType = {});

   static bool IsAlreadyOpen(const FilePath &projPathName);

   void OpenFile(const FilePath &fileName, bool addtohistory = true);

   // If pNewTrackList is passed in non-NULL, it gets filled with the pointers to NEW tracks.
   bool Import(const FilePath &fileName, WaveTrackArray *pTrackArray = NULL);

   // Takes array of unique pointers; returns array of shared
   std::vector< std::shared_ptr<Track> >
   AddImportedTracks(const FilePath &fileName,
                     TrackHolders &&newTracks);

   bool GetMenuClose() const { return mMenuClose; }
   void SetMenuClose(bool value) { mMenuClose = value; }

private:   
   void SetImportedDependencies( bool value ) { mImportedDependencies = value; }
   
   // Push names of NEW export files onto the path list
   bool SaveCopyWaveTracks(const FilePath & strProjectPathName,
      bool bLossless, FilePaths &strOtherNamesArray);
   bool DoSave(bool fromSaveAs, bool bWantSaveCopy, bool bLossless = false);

   // Declared in this class so that they can have access to private members
   static XMLTagHandler *RecordingRecoveryFactory( AudacityProject &project );
   static ProjectFileIORegistry::Entry sRecoveryFactory;
   static XMLTagHandler *ImportHandlerFactory( AudacityProject &project );
   static ProjectFileIORegistry::Entry sImportHandlerFactory;

   AudacityProject &mProject;

   std::shared_ptr<TrackList> mLastSavedTracks;
   
   // The handler that handles recovery of <recordingrecovery> tags
   std::unique_ptr<RecordingRecoveryHandler> mRecordingRecoveryHandler;
   
   std::unique_ptr<ImportXMLTagHandler> mImportXMLTagHandler;
   
   // Dependencies have been imported and a warning should be shown on save
   bool mImportedDependencies{ false };
   
   // Are we currently closing as the result of a menu command?
   bool mMenuClose{ false };
};

#endif
