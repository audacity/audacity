/**********************************************************************

Audacity: A Digital Audio Editor

ProjectManager.h

Paul Licameli split from AudacityProject.h

**********************************************************************/

#ifndef __AUDACITY_PROJECT_MANAGER__
#define __AUDACITY_PROJECT_MANAGER__

#include <memory>
#include <vector>

#include <wx/event.h> // to inherit
#include "ClientData.h" // to inherit
#include "import/ImportRaw.h" // defines TrackHolders

class wxTimer;
class wxTimerEvent;

class AudacityProject;
struct AudioIOStartStreamOptions;
class ImportXMLTagHandler;
class RecordingRecoveryHandler;
class Track;
class WaveTrack;
class XMLTagHandler;
namespace ProjectFileIORegistry{ struct Entry; }

using WaveTrackArray = std::vector < std::shared_ptr < WaveTrack > >;

///\brief Object associated with a project for high-level management of the
/// project's lifetime, including creation, destruction, opening from file,
/// importing, pushing undo states, and reverting to saved states
class ProjectManager final
   : public wxEvtHandler
   , public ClientData::Base
{
public:
   static ProjectManager &Get( AudacityProject &project );
   static const ProjectManager &Get( const AudacityProject &project );

   explicit ProjectManager( AudacityProject &project );
   ~ProjectManager() override;

   // This is the factory for projects:
   static AudacityProject *New();

   // File I/O

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
    * @param extraformat Specify the name of an additional format to allow
    * opening in this dialogue. This string is free-form, but should be short
    * enough to fit in the file dialogue filter drop-down. It should be
    * translated.
    * @param extrafilter Specify the file extension(s) for the additional format
    * specified by extraformat. The patterns must include the wildcard (e.g.
    * "*.aup" not "aup" or ".aup"), separate multiple patters with a semicolon,
    * e.g. "*.aup;*.AUP" because patterns are case-sensitive. Do not add a
    * trailing semicolon to the string. This string should not be translated
    * @return Array of file paths which the user selected to open (multiple
    * selections allowed).
    */
   static wxArrayString ShowOpenDialog(const wxString &extraformat = {},
         const wxString &extrafilter = {});

   static bool IsAlreadyOpen(const FilePath &projPathName);

   // The functions that open and import files can act as factories too, and
   // they set projects to initial state.
   static void OpenFiles(AudacityProject *proj);
   
   void OpenFile(const FilePath &fileName, bool addtohistory = true);

   // Return the given project if that is not NULL, else create a project.
   // Then open the given project path.
   // But if an exception escapes this function, create no NEW project.
   static AudacityProject *OpenProject(
      AudacityProject *pProject,
      const FilePath &fileNameArg, bool addtohistory = true);

   // If pNewTrackList is passed in non-NULL, it gets filled with the pointers to NEW tracks.
   bool Import(const FilePath &fileName, WaveTrackArray *pTrackArray = NULL);

   // Takes array of unique pointers; returns array of shared
   std::vector< std::shared_ptr<Track> >
   AddImportedTracks(const FilePath &fileName,
                     TrackHolders &&newTracks);

   void ResetProjectToEmpty();

   static void SaveWindowSize();

   // Routine to estimate how many minutes of recording time are left on disk
   int GetEstimatedRecordingMinsLeftOnDisk(long lCaptureChannels = 0);
   // Converts number of minutes to human readable format
   wxString GetHoursMinsString(int iMinutes);

   void SetMenuClose(bool value) { mMenuClose = value; }

private:
   void OnCloseWindow(wxCloseEvent & event);
   void OnTimer(wxTimerEvent & event);
   void OnOpenAudioFile(wxCommandEvent & event);
   void OnStatusChange( wxCommandEvent& );

   void RestartTimer();

   // Declared in this class so that they can have access to private members
   static XMLTagHandler *RecordingRecoveryFactory( AudacityProject &project );
   static ProjectFileIORegistry::Entry sRecoveryFactory;
   static XMLTagHandler *ImportHandlerFactory( AudacityProject &project );
   static ProjectFileIORegistry::Entry sImportHandlerFactory;

   // non-static data members
   AudacityProject &mProject;

   // The handler that handles recovery of <recordingrecovery> tags
   std::unique_ptr<RecordingRecoveryHandler> mRecordingRecoveryHandler;

   std::unique_ptr<ImportXMLTagHandler> mImportXMLTagHandler;

   std::unique_ptr<wxTimer> mTimer;

   // See explanation in OnCloseWindow
   bool mIsBeingDeleted{ false };

   // Are we currently closing as the result of a menu command?
   bool mMenuClose{ false };

   DECLARE_EVENT_TABLE()

   static bool sbWindowRectAlreadySaved;
};

#endif
