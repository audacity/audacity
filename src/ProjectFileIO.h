/**********************************************************************

Audacity: A Digital Audio Editor

ProjectFileIO.h

Paul Licameli split from AudacityProject.h

**********************************************************************/

#ifndef __AUDACITY_PROJECT_FILE_IO__
#define __AUDACITY_PROJECT_FILE_IO__

#include "ClientData.h" // to inherit
#include "Prefs.h" // to inherit
#include "xml/XMLTagHandler.h" // to inherit

class AudacityProject;
class TrackList;

///\brief Object associated with a project that manages reading and writing
/// of Audacity project file formats, and autosave
class ProjectFileIO final
   : public ClientData::Base
   , private XMLTagHandler
   , private PrefsListener
{
public:
   static ProjectFileIO &Get( AudacityProject &project );
   static const ProjectFileIO &Get( const AudacityProject &project );

   explicit ProjectFileIO( AudacityProject &project );
   ~ProjectFileIO();

   struct ReadProjectResults
   {
      bool decodeError;
      bool parseSuccess;
      bool trackError;
      wxString errorString;
   };
   ReadProjectResults ReadProjectFile( const FilePath &fileName );

   void EnqueueODTasks();

   bool WarnOfLegacyFile( );
   
   // To be called when closing a project that has been saved, so that
   // block files are not erased
   void CloseLock();

   bool Save();
   bool SaveAs(bool bWantSaveCopy = false, bool bLossless = false);
   bool SaveAs(const wxString & newFileName, bool bWantSaveCopy = false,
      bool addToHistory = true);
   // strProjectPathName is full path for aup except extension
   bool SaveFromTimerRecording( wxFileName fnFile );

   const FilePath &GetAutoSaveFileName() { return mAutoSaveFileName; }

   // It seems odd to put this method in this class, but the results do depend
   // on what is discovered while opening the file, such as whether it is a
   // recovery file
   void SetProjectTitle( int number = -1 );

   bool IsProjectSaved();

   void ResetProjectFileIO();
   
   void AutoSave();
   void DeleteCurrentAutoSaveFile();

   bool IsRecovered() const { return mIsRecovered; }
   void SetImportedDependencies( bool value ) { mImportedDependencies = value; }
   void SetLoadedFromAup( bool value ) { mbLoadedFromAup = value; }
 
private:
   bool SaveCopyWaveTracks(const FilePath & strProjectPathName,
      bool bLossless = false);
   bool DoSave(bool fromSaveAs, bool bWantSaveCopy, bool bLossless = false);

   void UpdatePrefs() override;

   // XMLTagHandler callback methods
   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs) override;
   XMLTagHandler *HandleXMLChild(const wxChar *tag) override;
   void WriteXMLHeader(XMLWriter &xmlFile) const;
   void WriteXML(
      XMLWriter &xmlFile, bool bWantSaveCopy) /* not override */;

   // non-staic data members
   AudacityProject &mProject;

   std::shared_ptr<TrackList> mLastSavedTracks;

   FilePaths mStrOtherNamesArray; // used to make sure compressed file names are unique

   // Last auto-save file name and path (empty if none)
   FilePath mAutoSaveFileName;

   // The auto-save data dir the project has been recovered from
   FilePath mRecoveryAutoSaveDataDir;

   // Are we currently auto-saving or not?
   bool mAutoSaving{ false };

   // Has this project been recovered from an auto-saved version
   bool mIsRecovered{ false };

   bool mbLoadedFromAup{ false };

   // Dependencies have been imported and a warning should be shown on save
   bool mImportedDependencies{ false };
};

#endif
