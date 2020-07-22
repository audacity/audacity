/**********************************************************************

Audacity: A Digital Audio Editor

ProjectFileIO.h

Paul Licameli split from AudacityProject.h

**********************************************************************/

#ifndef __AUDACITY_PROJECT_FILE_IO__
#define __AUDACITY_PROJECT_FILE_IO__

#include <atomic>
#include <condition_variable>
#include <memory>
#include <map>
#include <mutex>
#include <thread>
#include <set>

#include "ClientData.h" // to inherit
#include "Prefs.h" // to inherit
#include "xml/XMLTagHandler.h" // to inherit

struct sqlite3;
struct sqlite3_context;
struct sqlite3_stmt;
struct sqlite3_value;

class AudacityProject;
class AutoCommitTransaction;
class DBConnection;
class ProjectSerializer;
class SqliteSampleBlock;
class TrackList;
class WaveTrack;

using WaveTrackArray = std::vector < std::shared_ptr < WaveTrack > >;

// From SampleBlock.h
using SampleBlockID = long long;

using Connection = std::unique_ptr<DBConnection>;

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
   bool CloseProject();

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
   void SetBypass();

   // Remove all unused space within a project file
   void Vacuum(const std::shared_ptr<TrackList> &tracks);

   // The last vacuum check did actually vacuum the project file if true
   bool WasVacuumed();

   // The last vacuum check found unused blocks in the project file
   bool HadUnused();

   bool TransactionStart(const wxString &name);
   bool TransactionCommit(const wxString &name);
   bool TransactionRollback(const wxString &name);

   // Type of function that is given the fields of one row and returns
   // 0 for success or non-zero to stop the query
   using ExecCB = std::function<int(int cols, char **vals, char **names)>;

private:
   void WriteXMLHeader(XMLWriter &xmlFile) const;
   void WriteXML(XMLWriter &xmlFile, bool recording = false, const std::shared_ptr<TrackList> &tracks = nullptr) /* not override */;

   // XMLTagHandler callback methods
   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs) override;
   XMLTagHandler *HandleXMLChild(const wxChar *tag) override;

   void UpdatePrefs() override;

   int Exec(const char *query, const ExecCB &callback);

   // The opening of the database may be delayed until demanded.
   // Returns a non-null pointer to an open database, or throws an exception
   // if opening fails.
   sqlite3 *DB();

   Connection &Conn();

   bool OpenConnection(FilePath fileName = {});
   bool CloseConnection();

   // Put the current database connection aside, keeping it open, so that
   // another may be opened with OpenDB()
   void SaveConnection();

   // Close any set-aside connection
   void DiscardConnection();

   // Close any current connection and switch back to using the saved
   void RestoreConnection();

   // Use a connection that is already open rather than invoke OpenConnection
   void UseConnection(Connection &&conn, const FilePath &filePath);

   bool Query(const char *sql, const ExecCB &callback);

   bool GetValue(const char *sql, wxString &value);
   bool GetBlob(const char *sql, wxMemoryBuffer &buffer);

   bool CheckVersion();
   bool InstallSchema(sqlite3 *db, const char *schema = "main");
   bool UpgradeSchema();

   // Write project or autosave XML (binary) documents
   bool WriteDoc(const char *table, const ProjectSerializer &autosave, const char *schema = "main");

   // Application defined function to verify blockid exists is in set of blockids
   using BlockIDs = std::set<SampleBlockID>;
   static void InSet(sqlite3_context *context, int argc, sqlite3_value **argv);

   // Checks for orphan blocks.  This will go away at a future date
   bool CheckForOrphans(BlockIDs &blockids);

   // Return a database connection if successful, which caller must close
   Connection CopyTo(const FilePath &destpath,
                                      const TranslatableString &msg,
                                      bool prune = false,
                                      const std::shared_ptr<TrackList> &tracks = nullptr);

   void SetError(const TranslatableString & msg);
   void SetDBError(const TranslatableString & msg);

   bool ShouldVacuum(const std::shared_ptr<TrackList> &tracks);

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

   // Project was vacuumed last time Vacuum() ran
   bool mWasVacuumed;

   // Project had unused blocks during last Vacuum()
   bool mHadUnused;

   Connection mPrevConn;
   FilePath mPrevFileName;
   bool mPrevTemporary;

   Connection mCurrConn;
   TranslatableString mLastError;
   TranslatableString mLibraryError;

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

class DBConnection
{
public:
   explicit
   DBConnection(const std::weak_ptr<AudacityProject> &pProject);
   ~DBConnection();

   bool Open(const char *fileName);
   bool Close();

   bool SafeMode(const char *schema = "main");
   bool FastMode(const char *schema = "main");

   bool Assign(sqlite3 *handle);
   sqlite3 *Detach();

   sqlite3 *DB();

   int GetLastRC() const ;
   const wxString GetLastMessage() const;

   enum StatementID
   {
      GetSamples,
      GetSummary256,
      GetSummary64k,
      LoadSampleBlock,
      InsertSampleBlock,
      DeleteSampleBlock
   };
   sqlite3_stmt *GetStatement(enum StatementID id);
   sqlite3_stmt *Prepare(enum StatementID id, const char *sql);

   void SetBypass( bool bypass );
   bool ShouldBypass();

private:
   bool ModeConfig(sqlite3 *db, const char *schema, const char *config);

   void CheckpointThread();
   static int CheckpointHook(void *data, sqlite3 *db, const char *schema, int pages);

private:
   std::weak_ptr<AudacityProject> mpProject;
   sqlite3 *mDB;

   std::thread mCheckpointThread;
   std::condition_variable mCheckpointCondition;
   std::mutex mCheckpointMutex;
   std::atomic_bool mCheckpointStop{ false };
   std::atomic_int mCheckpointWaitingPages{ 0 };
   std::atomic_int mCheckpointCurrentPages{ 0 };

   std::map<enum StatementID, sqlite3_stmt *> mStatements;

   // Bypass transactions if database will be deleted after close
   bool mBypass;
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
