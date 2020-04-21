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

///\brief Object associated with a project that manages reading and writing
/// of Audacity project file formats, and autosave
class ProjectFileIO final
   : public ClientData::Base
   , public XMLTagHandler
   , private PrefsListener
{
public:
   static ProjectFileIO &Get( AudacityProject &project );
   static const ProjectFileIO &Get( const AudacityProject &project );

   explicit ProjectFileIO( AudacityProject &project );
   ProjectFileIO( const ProjectFileIO & ) PROHIBITED;
   ProjectFileIO &operator=( const ProjectFileIO & ) PROHIBITED;
   ~ProjectFileIO();

   bool WarnOfLegacyFile( );
   
   const FilePath &GetAutoSaveFileName() { return mAutoSaveFileName; }

   // It seems odd to put this method in this class, but the results do depend
   // on what is discovered while opening the file, such as whether it is a
   // recovery file
   void SetProjectTitle( int number = -1 );

   bool IsProjectSaved() const;

   void Reset();
   
   void AutoSave();
   void DeleteCurrentAutoSaveFile();

   bool IsRecovered() const { return mIsRecovered; }
   void SetIsRecovered( bool value ) { mIsRecovered = value; }
   bool IsLoadedFromAup() const { return mbLoadedFromAup; }
   void SetLoadedFromAup( bool value ) { mbLoadedFromAup = value; }
 
   XMLTagHandler *HandleXMLChild(const wxChar *tag) override;
   void WriteXMLHeader(XMLWriter &xmlFile) const;

   // If the second argument is not null, that means we are saving a
   // compressed project, and the wave tracks have been exported into the
   // named files
   void WriteXML(
      XMLWriter &xmlFile, FilePaths *strOtherNamesArray) /* not override */;

private:
   // XMLTagHandler callback methods
   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs) override;

   void UpdatePrefs() override;

   // non-static data members
   AudacityProject &mProject;

   // Last auto-save file name and path (empty if none)
   FilePath mAutoSaveFileName;

   // Are we currently auto-saving or not?
   bool mAutoSaving{ false };

   // Has this project been recovered from an auto-saved version
   bool mIsRecovered{ false };

   bool mbLoadedFromAup{ false };
};

class wxTopLevelWindow;

// TitleRestorer restores project window titles to what they were, in its destructor.
class TitleRestorer{
public:
   TitleRestorer( wxTopLevelWindow &window, AudacityProject &project );
   ~TitleRestorer();
   wxString sProjNumber;
   wxString sProjName;
   size_t UnnamedCount;
};

// This event is emitted by the project when there is a change
// in its title
wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API,
                         EVT_PROJECT_TITLE_CHANGE, wxCommandEvent);

#endif
