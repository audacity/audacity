/**********************************************************************

Audacity: A Digital Audio Editor

ProjectFileIO.h

Paul Licameli split from AudacityProject.h

**********************************************************************/

#ifndef __AUDACITY_PROJECT_FILE_IO__
#define __AUDACITY_PROJECT_FILE_IO__

#include <atomic>
#include <condition_variable>
#include <deque>
#include <memory>
#include <mutex>
#include <thread>
#include <set>

#include "ClientData.h" // to inherit
#include "Prefs.h" // to inherit
#include "xml/XMLTagHandler.h" // to inherit

struct sqlite3;
struct sqlite3_context;
struct sqlite3_value;

class AudacityProject;
class AutoCommitTransaction;
class ProjectSerializer;
class SqliteSampleBlock;
class TrackList;
class WaveTrack;

using WaveTrackArray = std::vector < std::shared_ptr < WaveTrack > >;

// From SampleBlock.h
using SampleBlockID = long long;

///\brief Object associated with a project that manages reading and writing
/// of Audacity project file formats, and autosave
class ProjectFileIO final
   : public ClientData::Base
   , public XMLTagHandler
   , private PrefsListener
   , public std::enable_shared_from_this<ProjectFileIO>
{
public:
   // Call this static function once before constructing any instances of this
   // class.  Reinvocations have no effect.  Return value is true for success.
   static bool InitializeSQL();

   static ProjectFileIO &Get( AudacityProject &project );
   static const ProjectFileIO &Get( const AudacityProject &project );

   explicit ProjectFileIO( AudacityProject &project );
   // unfortunate two-step construction needed because of
   // enable_shared_from_this
   void Init( AudacityProject &project );

   ProjectFileIO( const ProjectFileIO & ) PROHIBITED;
   ProjectFileIO &operator=( const ProjectFileIO & ) PROHIBITED;
   ~ProjectFileIO();

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

   bool AutoSave(bool recording = false);
   bool AutoSaveDelete(sqlite3 *db = nullptr);

   bool ImportProject(const FilePath &fileName);
   bool LoadProject(const FilePath &fileName);
   bool SaveProject(const FilePath &fileName);
   bool SaveCopy(const FilePath& fileName);

   wxLongLong GetFreeDiskSpace();

   const TranslatableString &GetLastError() const;
   const TranslatableString &GetLibraryError() const;

   // Provides a means to bypass "DELETE"s at shutdown if the database
   // is just going to be deleted anyway.  This prevents a noticable
   // delay caused by SampleBlocks being deleted when the Sequences that
   // own them are deleted.
   //
   // This is definitely hackage territory.  While this ability would
   // still be needed, I think handling it in a DB abstraction might be
   // a tad bit cleaner.
   //
   // For it's usage, see:
   //    SqliteSampleBlock::~SqliteSampleBlock()
   //    ProjectManager::OnCloseWindow()
   void Bypass(bool bypass);
   bool ShouldBypass();

   // Remove all unused space within a project file
   void Vacuum(const std::shared_ptr<TrackList> &tracks);

   // The last vacuum check did actually vacuum the project file if true
   bool WasVacuumed();

   // The last vacuum check found unused blocks in the project file
   bool HadUnused();

   bool TransactionStart(const wxString &name);
   bool TransactionCommit(const wxString &name);
   bool TransactionRollback(const wxString &name);

private:
   void WriteXMLHeader(XMLWriter &xmlFile) const;
   void WriteXML(XMLWriter &xmlFile, bool recording = false, const std::shared_ptr<TrackList> &tracks = nullptr) /* not override */;

   // XMLTagHandler callback methods
   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs) override;
   XMLTagHandler *HandleXMLChild(const wxChar *tag) override;

   void UpdatePrefs() override;

   using ExecResult = std::vector<std::vector<wxString>>;
   using ExecCB = std::function<int(ExecResult &result, int cols, char **vals, char **names)>;
   struct ExecParm
   {
      ExecCB func;
      ExecResult &result;
   };
   static int ExecCallback(void *data, int cols, char **vals, char **names);
   int Exec(const char *query, ExecCB callback, ExecResult &result);

   // The opening of the database may be delayed until demanded.
   // Returns a non-null pointer to an open database, or throws an exception
   // if opening fails.
   sqlite3 *DB();

   // Put the current database connection aside, keeping it open, so that
   // another may be opened with OpenDB()
   void SaveConnection();

   // Close any set-aside connection
   void DiscardConnection();

   // Close any current connection and switch back to using the saved
   void RestoreConnection();

   // Use a connection that is already open rather than invoke OpenDB
   void UseConnection(sqlite3 *db, const FilePath &filePath);

   // Make sure the connection/schema combo is configured the way we want
   void Config(sqlite3 *db, const char *config, const wxString &schema = wxT("main"));

   sqlite3 *OpenDB(FilePath fileName = {});
   bool CloseDB();
   bool DeleteDB();

   bool Query(const char *sql, ExecResult &result);

   bool GetValue(const char *sql, wxString &value);
   bool GetBlob(const char *sql, wxMemoryBuffer &buffer);

   bool CheckVersion();
   bool InstallSchema(sqlite3 *db, const char *schema = "main");
   bool UpgradeSchema();

   // Write project or autosave XML (binary) documents
   bool WriteDoc(const char *table, const ProjectSerializer &autosave, sqlite3 *db = nullptr);

   // Application defined function to verify blockid exists is in set of blockids
   using BlockIDs = std::set<SampleBlockID>;
   static void InSet(sqlite3_context *context, int argc, sqlite3_value **argv);

   // Checks for orphan blocks.  This will go away at a future date
   bool CheckForOrphans(BlockIDs &blockids);

   // Return a database connection if successful, which caller must close
   sqlite3 *CopyTo(const FilePath &destpath,
                   const TranslatableString &msg,
                   bool prune = false,
                   const std::shared_ptr<TrackList> &tracks = nullptr);

   void SetError(const TranslatableString & msg);
   void SetDBError(const TranslatableString & msg);

   bool ShouldVacuum(const std::shared_ptr<TrackList> &tracks);

   void CheckpointThread();
   static int CheckpointHook(void *that, sqlite3 *db, const char *schema, int pages);

private:
   // non-static data members
   std::weak_ptr<AudacityProject> mpProject;

   // The project's file path
   FilePath mFileName;

   // Has this project been recovered from an auto-saved version
   bool mRecovered;

   // Has this project been modified
   bool mModified;

   // Is this project still a temporary/unsaved project
   bool mTemporary;

   // Bypass transactions if database will be deleted after close
   bool mBypass;

   // Project was vacuumed last time Vacuum() ran
   bool mWasVacuumed;

   // Project had unused blocks during last Vacuum()
   bool mHadUnused;

   sqlite3 *mPrevDB;
   FilePath mPrevFileName;

   sqlite3 *mDB;
   TranslatableString mLastError;
   TranslatableString mLibraryError;

   std::deque<sqlite3 *> mCheckpointWork;
   std::condition_variable mCheckpointCondition;
   std::mutex mCheckpointMutex;
   std::thread mCheckpointThread;
   std::atomic_bool mCheckpointStop;
   std::mutex mCheckpointActive;

   friend SqliteSampleBlock;
   friend AutoCommitTransaction;
};

class AutoCommitTransaction
{
public:
   AutoCommitTransaction(ProjectFileIO &projectFileIO, const char *name);
   ~AutoCommitTransaction();

   bool Commit();
   bool Rollback();

private:
   ProjectFileIO &mIO;
   bool mInTrans;
   wxString mName;
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
