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

#include <sqlite3.h>

class AudacityProject;
class AutoSaveFile;
class SampleBlock;
class WaveTrack;

using WaveTrackArray = std::vector < std::shared_ptr < WaveTrack > >;

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
   
   // It seems odd to put this method in this class, but the results do depend
   // on what is discovered while opening the file, such as whether it is a
   // recovery file
   void SetProjectTitle(int number = -1);
   // Should be empty or a fully qualified file name

   const FilePath &GetFileName() const;
   void SetFileName( const FilePath &fileName );

   bool IsModified() const;
   bool IsTemporary() const;
   bool IsRecovered() const;

   void Reset();

   bool AutoSave(const WaveTrackArray *tracks = nullptr);
   bool AutoSave(const AutoSaveFile &autosave);
   bool AutoSaveDelete();

   bool LoadProject(const FilePath &fileName);
   bool SaveProject(const FilePath &fileName);

   XMLTagHandler *HandleXMLChild(const wxChar *tag) override;
   void WriteXMLHeader(XMLWriter &xmlFile) const;
   void WriteXML(XMLWriter &xmlFile, const WaveTrackArray *tracks = nullptr) /* not override */;

   wxLongLong GetFreeDiskSpace();

   const TranslatableString & GetLastError() const;
   const TranslatableString & GetLibraryError() const;

private:
   // XMLTagHandler callback methods
   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs) override;

   void UpdatePrefs() override;

   using ExecCB = std::function<int(wxString *result, int cols, char **vals, char **names)>;
   using ExecFunc = int (*)(void *data, int cols, char **vals, char **names);
   struct ExecParm
   {
      ExecCB func;
      wxString *result;
   };
   static int ExecCallback(void *data, int cols, char **vals, char **names);
   int Exec(const char *query, ExecCB callback, wxString *result);

   sqlite3 *DB();
   sqlite3 *OpenDB(FilePath fileName = {});
   bool CloseDB();
   bool DeleteDB();
   bool CleanDB();

   wxString GetValue(const char *sql);
   bool GetBlob(const char *sql, wxMemoryBuffer &buffer);

   bool CheckVersion();
   bool InstallSchema();
   bool UpgradeSchema();

   bool CopyTo(const FilePath &destpath);

   void SetError(const TranslatableString & msg);
   void SetDBError(const TranslatableString & msg);

private:
   // non-static data members
   AudacityProject &mProject;

   // The project's file path
   FilePath mFileName;

   // Has this project been recovered from an auto-saved version
   bool mRecovered;

   // Has this project been modified
   bool mModified;

   // Is this project still a temporary/unsaved project
   bool mTemporary;

   sqlite3 *mDB;
   FilePath mDBPath;
   TranslatableString mLastError;
   TranslatableString mLibraryError;

   friend SampleBlock;
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
