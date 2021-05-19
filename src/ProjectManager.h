/**********************************************************************

Audacity: A Digital Audio Editor

ProjectManager.h

Paul Licameli split from AudacityProject.h

**********************************************************************/

#ifndef __AUDACITY_PROJECT_MANAGER__
#define __AUDACITY_PROJECT_MANAGER__

#include <memory>

#include <wx/event.h> // to inherit
#include "ClientData.h" // to inherit

class wxTimer;
class wxTimerEvent;

class AudacityProject;
struct AudioIOStartStreamOptions;

///\brief Object associated with a project for high-level management of the
/// project's lifetime, including creation, destruction, opening from file,
/// importing, pushing undo states, and reverting to saved states
class AUDACITY_DLL_API ProjectManager final
   : public wxEvtHandler
   , public ClientData::Base
{
public:
   static ProjectManager &Get( AudacityProject &project );
   static const ProjectManager &Get( const AudacityProject &project );

   explicit ProjectManager( AudacityProject &project );
   ProjectManager( const ProjectManager & ) PROHIBITED;
   ProjectManager &operator=( const ProjectManager & ) PROHIBITED;
   ~ProjectManager() override;

   // This is the factory for projects:
   static AudacityProject *New();

   // The function that imports files can act as a factory too, and for that
   // reason remains in this class, not in ProjectFileManager
   static void OpenFiles(AudacityProject *proj);

   //! False when it is unsafe to overwrite proj with contents of an .aup3 file
   static bool SafeToOpenProjectInto(AudacityProject &proj);

   //! Open a file into an AudacityProject, returning the project, or nullptr for failure
   /*!
    If an exception escapes this function, no projects are created.
    @param pProject if not null, a project that may be reused
    @param fileNameArg path to the file to open; not always an Audacity project file, may be an import
    @param addtohistory whether to add .aup3 files to the MRU list (but always done for imports)
    @param reuseNonemptyProject if true, may reuse the given project when nonempty,
       but only if importing (not for a project file)
    */
   static AudacityProject *OpenProject(
      AudacityProject *pProject,
      const FilePath &fileNameArg, bool addtohistory, bool reuseNonemptyProject);

   void ResetProjectToEmpty();

   static void SaveWindowSize();

   // Routine to estimate how many minutes of recording time are left on disk
   int GetEstimatedRecordingMinsLeftOnDisk(long lCaptureChannels = 0);
   // Converts number of minutes to human readable format
   TranslatableString GetHoursMinsString(int iMinutes);

   void SetStatusText( const TranslatableString &text, int number );
   void SetSkipSavePrompt(bool bSkip) { sbSkipPromptingForSave = bSkip; };

private:
   void OnReconnectionFailure(wxCommandEvent & event);
   void OnCloseWindow(wxCloseEvent & event);
   void OnTimer(wxTimerEvent & event);
   void OnOpenAudioFile(wxCommandEvent & event);
   void OnStatusChange( wxCommandEvent& );

   void RestartTimer();

   // non-static data members
   AudacityProject &mProject;

   std::unique_ptr<wxTimer> mTimer;

   DECLARE_EVENT_TABLE()

   static bool sbWindowRectAlreadySaved;
   static bool sbSkipPromptingForSave;
};

#endif
