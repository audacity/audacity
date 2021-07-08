/**********************************************************************

Audacity: A Digital Audio Editor

ProjectFileIO.cpp

Paul Licameli split from AudacityProject.cpp

**********************************************************************/

#include "ProjectFileIO.h"

#include <atomic>
#include <sqlite3.h>
#include <wx/crt.h>
#include <wx/frame.h>
#include <wx/progdlg.h>
#include <wx/sstream.h>
#include <wx/xml/xml.h>

#include "ActiveProjects.h"
#include "CodeConversions.h"
#include "DBConnection.h"
#include "Project.h"
#include "ProjectFileIORegistry.h"
#include "ProjectSerializer.h"
#include "ProjectSettings.h"
#include "SampleBlock.h"
#include "Tags.h"
#include "TempDirectory.h"
#include "ViewInfo.h"
#include "WaveTrack.h"
#include "widgets/AudacityMessageBox.h"
#include "widgets/ErrorDialog.h"
#include "widgets/NumericTextCtrl.h"
#include "widgets/ProgressDialog.h"
#include "wxFileNameWrapper.h"
#include "xml/XMLFileReader.h"

// Don't change this unless the file format changes
// in an irrevocable way
#define AUDACITY_FILE_FORMAT_VERSION "1.3.0"

#undef NO_SHM
#if !defined(__WXMSW__)
   #define NO_SHM
#endif

wxDEFINE_EVENT(EVT_PROJECT_TITLE_CHANGE, wxCommandEvent);
wxDEFINE_EVENT( EVT_CHECKPOINT_FAILURE, wxCommandEvent);
wxDEFINE_EVENT( EVT_RECONNECTION_FAILURE, wxCommandEvent);

// Used to convert 4 byte-sized values into an integer for use in SQLite
// PRAGMA statements. These values will be store in the database header.
//
// Note that endianness is not an issue here since SQLite integers are
// architecture independent.
#define PACK(b1, b2, b3, b4) ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4)

// The ProjectFileID is stored in the SQLite database header to identify the file
// as an Audacity project file. It can be used by applications that identify file
// types, such as the Linux "file" command.
static const int ProjectFileID = PACK('A', 'U', 'D', 'Y');

// The "ProjectFileVersion" represents the version of Audacity at which a specific
// database schema was used. It is assumed that any changes to the database schema
// will require a new Audacity version so if schema changes are required set this
// to the new release being produced.
//
// This version is checked before accessing any tables in the database since there's
// no guarantee what tables exist. If it's found that the database is newer than the
// currently running Audacity, an error dialog will be displayed informing the user
// that they need a newer version of Audacity.
//
// Note that this is NOT the "schema_version" that SQLite maintains. The value
// specified here is stored in the "user_version" field of the SQLite database
// header.
static const int ProjectFileVersion = PACK(3, 0, 0, 0);

// Navigation:
//
// Bindings are marked out in the code by, e.g. 
// BIND SQL sampleblocks
// A search for "BIND SQL" will find all bindings.
// A search for "SQL sampleblocks" will find all SQL related 
// to sampleblocks.

static const char *ProjectFileSchema =
   // These are persistent and not connection based
   //
   // See the CMakeList.txt for the SQLite lib for more
   // settings.
   "PRAGMA <schema>.application_id = %d;"
   "PRAGMA <schema>.user_version = %d;"
   ""
   // project is a binary representation of an XML file.
   // it's in binary for speed.
   // One instance only.  id is always 1.
   // dict is a dictionary of fieldnames.
   // doc is the binary representation of the XML
   // in the doc, fieldnames are replaced by 2 byte dictionary
   // index numbers.
   // This is all opaque to SQLite.  It just sees two
   // big binary blobs.
   // There is no limit to document blob size.
   // dict will be smallish, with an entry for each 
   // kind of field.
   "CREATE TABLE IF NOT EXISTS <schema>.project"
   "("
   "  id                   INTEGER PRIMARY KEY,"
   "  dict                 BLOB,"
   "  doc                  BLOB"
   ");"
   ""
   // CREATE SQL autosave
   // autosave is a binary representation of an XML file.
   // it's in binary for speed.
   // One instance only.  id is always 1.
   // dict is a dictionary of fieldnames.
   // doc is the binary representation of the XML
   // in the doc, fieldnames are replaced by 2 byte dictionary
   // index numbers.
   // This is all opaque to SQLite.  It just sees two
   // big binary blobs.
   // There is no limit to document blob size.
   // dict will be smallish, with an entry for each 
   // kind of field.
   "CREATE TABLE IF NOT EXISTS <schema>.autosave"
   "("
   "  id                   INTEGER PRIMARY KEY,"
   "  dict                 BLOB,"
   "  doc                  BLOB"
   ");"
   ""
   // CREATE SQL sampleblocks
   // 'samples' are fixed size blocks of int16, int32 or float32 numbers.
   // The blocks may be partially empty.
   // The quantity of valid data in the blocks is
   // provided in the project blob.
   // 
   // sampleformat specifies the format of the samples stored.
   //
   // blockID is a 64 bit number.
   //
   // Rows are immutable -- never updated after addition, but may be
   // deleted.
   //
   // summin to summary64K are summaries at 3 distance scales.
   "CREATE TABLE IF NOT EXISTS <schema>.sampleblocks"
   "("
   "  blockid              INTEGER PRIMARY KEY AUTOINCREMENT,"
   "  sampleformat         INTEGER,"
   "  summin               REAL,"
   "  summax               REAL,"
   "  sumrms               REAL,"
   "  summary256           BLOB,"
   "  summary64k           BLOB,"
   "  samples              BLOB"
   ");";

// This singleton handles initialization/shutdown of the SQLite library.
// It is needed because our local SQLite is built with SQLITE_OMIT_AUTOINIT
// defined.
//
// It's safe to use even if a system version of SQLite is used that didn't
// have SQLITE_OMIT_AUTOINIT defined.
class SQLiteIniter
{
public:
   SQLiteIniter()
   {
      // Enable URI filenames for all connections
      mRc = sqlite3_config(SQLITE_CONFIG_URI, 1);
      if (mRc == SQLITE_OK)
      {
         mRc = sqlite3_config(SQLITE_CONFIG_LOG, LogCallback, nullptr);
         if (mRc == SQLITE_OK)
         {
            mRc = sqlite3_initialize();
         }
      }

#ifdef NO_SHM
      if (mRc == SQLITE_OK)
      {
         // Use the "unix-excl" VFS to make access to the DB exclusive.  This gets
         // rid of the "<database name>-shm" shared memory file.
         //
         // Though it shouldn't, it doesn't matter if this fails.
         auto vfs = sqlite3_vfs_find("unix-excl");
         if (vfs)
         {
            sqlite3_vfs_register(vfs, 1);
         }
      }
#endif
   }
   ~SQLiteIniter()
   {
      // This function must be called single-threaded only
      // It returns a value, but there's nothing we can do with it
      (void) sqlite3_shutdown();
   }

   static void LogCallback(void *WXUNUSED(arg), int code, const char *msg)
   {
      wxLogMessage("sqlite3 message: (%d) %s", code, msg);
   }

   int mRc;
};

bool ProjectFileIO::InitializeSQL()
{
   static SQLiteIniter sqliteIniter;
   return sqliteIniter.mRc == SQLITE_OK;
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

static const AudacityProject::AttachedObjects::RegisteredFactory sFileIOKey{
   []( AudacityProject &parent ){
      auto result = std::make_shared< ProjectFileIO >( parent );
      return result;
   }
};

ProjectFileIO &ProjectFileIO::Get( AudacityProject &project )
{
   auto &result = project.AttachedObjects::Get< ProjectFileIO >( sFileIOKey );
   return result;
}

const ProjectFileIO &ProjectFileIO::Get( const AudacityProject &project )
{
   return Get( const_cast< AudacityProject & >( project ) );
}

ProjectFileIO::ProjectFileIO(AudacityProject &project)
   : mProject{ project }
   , mpErrors{ std::make_shared<DBConnectionErrors>() }
{
   mPrevConn = nullptr;

   mRecovered = false;
   mModified = false;
   mTemporary = true;

   UpdatePrefs();
}

ProjectFileIO::~ProjectFileIO()
{
}

bool ProjectFileIO::HasConnection() const
{
   auto &connectionPtr = ConnectionPtr::Get( mProject );
   return connectionPtr.mpConnection != nullptr;
}

DBConnection &ProjectFileIO::GetConnection()
{
   auto &curConn = CurrConn();
   if (!curConn)
   {
      if (!OpenConnection())
      {
         throw SimpleMessageBoxException
         {
            ExceptionType::Internal,
            XO("Failed to open the project's database"),
            XO("Warning"),
            "Error:_Disk_full_or_not_writable"
         };
      }
   }

   return *curConn;
}

wxString ProjectFileIO::GenerateDoc()
{
   auto &trackList = TrackList::Get( mProject );

   XMLStringWriter doc;
   WriteXMLHeader(doc);
   WriteXML(doc, false, trackList.empty() ? nullptr : &trackList);
   return doc;
}

sqlite3 *ProjectFileIO::DB()
{
   return GetConnection().DB();
}

/*!
 @pre *CurConn() does not exist
 @post *CurConn() exists or return value is false
 */
bool ProjectFileIO::OpenConnection(FilePath fileName /* = {}  */)
{
   auto &curConn = CurrConn();
   wxASSERT(!curConn);
   bool isTemp = false;

   if (fileName.empty())
   {
      fileName = GetFileName();
      if (fileName.empty())
      {
         fileName = TempDirectory::UnsavedProjectFileName();
         isTemp = true;
      }
   }
   else
   {
      // If this project resides in the temporary directory, then we'll mark it
      // as temporary.
      wxFileName temp(TempDirectory::TempDir(), wxT(""));
      wxFileName file(fileName);
      file.SetFullName(wxT(""));
      if (file == temp)
      {
         isTemp = true;
      }
   }

   // Pass weak_ptr to project into DBConnection constructor
   curConn = std::make_unique<DBConnection>(
      mProject.shared_from_this(), mpErrors, [this]{ OnCheckpointFailure(); } );
   auto rc = curConn->Open(fileName);
   if (rc != SQLITE_OK)
   {
      // Must use SetError() here since we do not have an active DB
      SetError(
         XO("Failed to open database file:\n\n%s").Format(fileName),
         {},
         rc
      );
      curConn.reset();
      return false;
   }

   if (!CheckVersion())
   {
      CloseConnection();
      curConn.reset();
      return false;
   }

   mTemporary = isTemp;

   SetFileName(fileName);

   return true;
}

bool ProjectFileIO::CloseConnection()
{
   auto &curConn = CurrConn();
   if (!curConn)
      return false;

   if (!curConn->Close())
   {
      return false;
   }
   curConn.reset();

   SetFileName({});

   return true;
}

// Put the current database connection aside, keeping it open, so that
// another may be opened with OpenConnection()
void ProjectFileIO::SaveConnection()
{
   // Should do nothing in proper usage, but be sure not to leak a connection:
   DiscardConnection();

   mPrevConn = std::move(CurrConn());
   mPrevFileName = mFileName;
   mPrevTemporary = mTemporary;

   SetFileName({});
}

// Close any set-aside connection
void ProjectFileIO::DiscardConnection()
{
   if (mPrevConn)
   {
      if (!mPrevConn->Close())
      {
         // Store an error message
         SetDBError(
            XO("Failed to discard connection")
         );
      }

      // If this is a temporary project, we no longer want to keep the
      // project file.
      if (mPrevTemporary)
      {
         // This is just a safety check.
         wxFileName temp(TempDirectory::TempDir(), wxT(""));
         wxFileName file(mPrevFileName);
         file.SetFullName(wxT(""));
         if (file == temp)
         {
            if (!RemoveProject(mPrevFileName))
            {
               wxLogMessage("Failed to remove temporary project %s", mPrevFileName);
            }
         }
      }
      mPrevConn = nullptr;
      mPrevFileName.clear();
   }
}

// Close any current connection and switch back to using the saved
void ProjectFileIO::RestoreConnection()
{
   auto &curConn = CurrConn();
   if (curConn)
   {
      if (!curConn->Close())
      {
         // Store an error message
         SetDBError(
            XO("Failed to restore connection")
         );
      }
   }

   curConn = std::move(mPrevConn);
   SetFileName(mPrevFileName);
   mTemporary = mPrevTemporary;

   mPrevFileName.clear();
}

void ProjectFileIO::UseConnection(Connection &&conn, const FilePath &filePath)
{
   auto &curConn = CurrConn();
   wxASSERT(!curConn);

   curConn = std::move(conn);
   SetFileName(filePath);
}

static int ExecCallback(void *data, int cols, char **vals, char **names)
{
   auto &cb = *static_cast<const ProjectFileIO::ExecCB *>(data);
   // Be careful not to throw anything across sqlite3's stack frames.
   return GuardedCall<int>(
      [&]{ return cb(cols, vals, names); },
      MakeSimpleGuard( 1 )
   );
}

int ProjectFileIO::Exec(const char *query, const ExecCB &callback)
{
   char *errmsg = nullptr;

   const void *ptr = &callback;
   int rc = sqlite3_exec(DB(), query, ExecCallback,
                         const_cast<void*>(ptr), &errmsg);

   if (rc != SQLITE_ABORT && errmsg)
   {
      SetDBError(
         XO("Failed to execute a project file command:\n\n%s").Format(query),
         Verbatim(errmsg),
         rc
      );
   }
   if (errmsg)
   {
      sqlite3_free(errmsg);
   }

   return rc;
}

bool ProjectFileIO::Query(const char *sql, const ExecCB &callback)
{
   int rc = Exec(sql, callback);
   // SQLITE_ABORT is a non-error return only meaning the callback
   // stopped the iteration of rows early
   if ( !(rc == SQLITE_OK || rc == SQLITE_ABORT) )
   {
      return false;
   }

   return true;
}

bool ProjectFileIO::GetValue(const char *sql, wxString &result)
{
   // Retrieve the first column in the first row, if any
   result.clear();
   auto cb = [&result](int cols, char **vals, char **){
      if (cols > 0)
         result = vals[0];
      // Stop after one row
      return 1;
   };

   return Query(sql, cb);
}

bool ProjectFileIO::GetBlob(const char *sql, wxMemoryBuffer &buffer)
{
   auto db = DB();
   int rc;

   buffer.Clear();

   sqlite3_stmt *stmt = nullptr;
   auto cleanup = finally([&]
   {
      if (stmt)
      {
         sqlite3_finalize(stmt);
      }
   });

   rc = sqlite3_prepare_v2(db, sql, -1, &stmt, nullptr);
   if (rc != SQLITE_OK)
   {
      SetDBError(
         XO("Unable to prepare project file command:\n\n%s").Format(sql)
      );
      return false;
   }

   rc = sqlite3_step(stmt);

   // A row wasn't found...not an error
   if (rc == SQLITE_DONE)
   {
      return true;
   }

   if (rc != SQLITE_ROW)
   {
      SetDBError(
         XO("Failed to retrieve data from the project file.\nThe following command failed:\n\n%s").Format(sql)
      );
      // AUD TODO handle error
      return false;
   }

   const void *blob = sqlite3_column_blob(stmt, 0);
   int size = sqlite3_column_bytes(stmt, 0);

   buffer.AppendData(blob, size);

   return true;
}

bool ProjectFileIO::CheckVersion()
{
   auto db = DB();
   int rc;

   // Install our schema if this is an empty DB
   wxString result;
   if (!GetValue("SELECT Count(*) FROM sqlite_master WHERE type='table';", result))
   {
      // Bug 2718 workaround for a better error message:
      // If at this point we get SQLITE_CANTOPEN, then the directory is read-only
      if (GetLastErrorCode() == SQLITE_CANTOPEN)
      {
          SetError(
              /* i18n-hint: An error message. */
              XO("Project is in a read only directory\n(Unable to create the required temporary files)"),
              GetLibraryError()
          );
      }

      return false;
   }

   // If the return count is zero, then there are no tables defined, so this
   // must be a new project file.
   if (wxStrtol<char **>(result, nullptr, 10) == 0)
   {
      return InstallSchema(db);
   }

   // Check for our application ID
   if (!GetValue("PRAGMA application_ID;", result))
   {
      return false;
   }

   // It's a database that SQLite recognizes, but it's not one of ours
   if (wxStrtoul<char **>(result, nullptr, 10) != ProjectFileID)
   {
      SetError(XO("This is not an Audacity project file"));
      return false;
   }

   // Get the project file version
   if (!GetValue("PRAGMA user_version;", result))
   {
      return false;
   }

   long version = wxStrtol<char **>(result, nullptr, 10);

   // Project file version is higher than ours. We will refuse to
   // process it since we can't trust anything about it.
   if (version > ProjectFileVersion)
   {
      SetError(
         XO("This project was created with a newer version of Audacity.\n\nYou will need to upgrade to open it.")
      );
      return false;
   }
   
   // Project file is older than ours, ask the user if it's okay to
   // upgrade.
   if (version < ProjectFileVersion)
   {
      return UpgradeSchema();
   }

   return true;
}

bool ProjectFileIO::InstallSchema(sqlite3 *db, const char *schema /* = "main" */)
{
   int rc;

   wxString sql;
   sql.Printf(ProjectFileSchema, ProjectFileID, ProjectFileVersion);
   sql.Replace("<schema>", schema);

   rc = sqlite3_exec(db, sql, nullptr, nullptr, nullptr);
   if (rc != SQLITE_OK)
   {
      SetDBError(
         XO("Unable to initialize the project file")
      );
      return false;
   }

   return true;
}

bool ProjectFileIO::UpgradeSchema()
{
   // To do
   return true;
}

// The orphan block handling should be removed once autosave and related
// blocks become part of the same transaction.

// An SQLite function that takes a blockid and looks it up in a set of
// blockids captured during project load.  If the blockid isn't found
// in the set, it will be deleted.
void ProjectFileIO::InSet(sqlite3_context *context, int argc, sqlite3_value **argv)
{
   BlockIDs *blockids = (BlockIDs *) sqlite3_user_data(context);
   SampleBlockID blockid = sqlite3_value_int64(argv[0]);

   sqlite3_result_int(context, blockids->find(blockid) != blockids->end());
}

bool ProjectFileIO::DeleteBlocks(const BlockIDs &blockids, bool complement)
{
   auto db = DB();
   int rc;

   auto cleanup = finally([&]
   {
      // Remove our function, whether it was successfully defined or not.
      sqlite3_create_function(db, "inset", 1, SQLITE_UTF8 | SQLITE_DETERMINISTIC, nullptr, nullptr, nullptr, nullptr);
   });

   // Add the function used to verify each row's blockid against the set of active blockids
   const void *p = &blockids;
   rc = sqlite3_create_function(db, "inset", 1, SQLITE_UTF8 | SQLITE_DETERMINISTIC, const_cast<void*>(p), InSet, nullptr, nullptr);
   if (rc != SQLITE_OK)
   {
      /* i18n-hint: An error message.  Don't translate inset or blockids.*/
      SetDBError(XO("Unable to add 'inset' function (can't verify blockids)"));
      return false;
   }

   // Delete all rows in the set, or not in it
   // This is the first command that writes to the database, and so we
   // do more informative error reporting than usual, if it fails.
   auto sql = wxString::Format(
      "DELETE FROM sampleblocks WHERE %sinset(blockid);",
      complement ? "NOT " : "" );
   rc = sqlite3_exec(db, sql, nullptr, nullptr, nullptr);
   if (rc != SQLITE_OK)
   {
      if( rc==SQLITE_READONLY)
         /* i18n-hint: An error message.  Don't translate blockfiles.*/
         SetDBError(XO("Project is read only\n(Unable to work with the blockfiles)"));
      else if( rc==SQLITE_LOCKED)
         /* i18n-hint: An error message.  Don't translate blockfiles.*/
         SetDBError(XO("Project is locked\n(Unable to work with the blockfiles)"));
      else if( rc==SQLITE_BUSY)
         /* i18n-hint: An error message.  Don't translate blockfiles.*/
         SetDBError(XO("Project is busy\n(Unable to work with the blockfiles)"));
      else if( rc==SQLITE_CORRUPT)
         /* i18n-hint: An error message.  Don't translate blockfiles.*/
         SetDBError(XO("Project is corrupt\n(Unable to work with the blockfiles)"));
      else if( rc==SQLITE_PERM)
         /* i18n-hint: An error message.  Don't translate blockfiles.*/
         SetDBError(XO("Some permissions issue\n(Unable to work with the blockfiles)"));
      else if( rc==SQLITE_IOERR)
         /* i18n-hint: An error message.  Don't translate blockfiles.*/
         SetDBError(XO("A disk I/O error\n(Unable to work with the blockfiles)"));
      else if( rc==SQLITE_AUTH)
         /* i18n-hint: An error message.  Don't translate blockfiles.*/
         SetDBError(XO("Not authorized\n(Unable to work with the blockfiles)"));
      else
         /* i18n-hint: An error message.  Don't translate blockfiles.*/
         SetDBError(XO("Unable to work with the blockfiles"));

      return false;
   }

   // Mark the project recovered if we deleted any rows
   int changes = sqlite3_changes(db);
   if (changes > 0)
   {
      wxLogInfo(XO("Total orphan blocks deleted %d").Translation(), changes);
      mRecovered = true;
   }

   return true;
}

bool ProjectFileIO::CopyTo(const FilePath &destpath,
   const TranslatableString &msg,
   bool isTemporary,
   bool prune /* = false */,
   const std::vector<const TrackList *> &tracks /* = {} */)
{
   auto pConn = CurrConn().get();
   if (!pConn)
      return false;

   // Get access to the active tracklist
   auto pProject = &mProject;

   SampleBlockIDSet blockids;

   // Collect all active blockids
   if (prune)
   {
      for (auto trackList : tracks)
         if (trackList)
            InspectBlocks( *trackList, {}, &blockids );
   }
   // Collect ALL blockids
   else
   {
      auto cb = [&blockids](int cols, char **vals, char **){
         SampleBlockID blockid;
         wxString{ vals[0] }.ToLongLong(&blockid);
         blockids.insert(blockid);
         return 0;
      };

      if (!Query("SELECT blockid FROM sampleblocks;", cb))
      {
         // Error message already captured.
         return false;
      }
   }

   // Create the project doc
   ProjectSerializer doc;
   WriteXMLHeader(doc);
   WriteXML(doc, false, tracks.empty() ? nullptr : tracks[0]);

   auto db = DB();
   Connection destConn = nullptr;
   bool success = false;
   int rc = SQLITE_OK;
   ProgressResult res = ProgressResult::Success;

   // Cleanup in case things go awry
   auto cleanup = finally([&]
   {
      if (!success)
      {
         if (destConn)
         {
            destConn->Close();
            destConn = nullptr;
         }

         // Rollback transaction in case one was active.
         // If this fails (probably due to memory or disk space), the transaction will
         // (presumably) stil be active, so further updates to the project file will
         // fail as well. Not really much we can do about it except tell the user.
         auto result = sqlite3_exec(db, "ROLLBACK;", nullptr, nullptr, nullptr);

         // Only capture the error if there wasn't a previous error
         if (result != SQLITE_OK && (rc == SQLITE_DONE || rc == SQLITE_OK))
         {
            SetDBError(
               XO("Failed to rollback transaction during import")
            );
         }

         // And detach the outbound DB in case (if it's attached). Don't check for
         // errors since it may not be attached. But, if it is and the DETACH fails,
         // subsequent CopyTo() actions will fail until Audacity is relaunched.
         sqlite3_exec(db, "DETACH DATABASE outbound;", nullptr, nullptr, nullptr);

         // RemoveProject not necessary to clean up attached database
         wxRemoveFile(destpath);
      }
   });

   // Attach the destination database 
   wxString sql;
   wxString dbName = destpath;
   // Bug 2793: Quotes in name need escaping for sqlite3.
   dbName.Replace( "'", "''");
   sql.Printf("ATTACH DATABASE '%s' AS outbound;", dbName.ToUTF8());

   rc = sqlite3_exec(db, sql, nullptr, nullptr, nullptr);
   if (rc != SQLITE_OK)
   {
      SetDBError(
         XO("Unable to attach destination database")
      );
      return false;
   }

   // Ensure attached DB connection gets configured
   //
   // NOTE:  Between the above attach and setting the mode here, a normal DELETE
   //        mode journal will be used and will briefly appear in the filesystem.
   if ( pConn->FastMode("outbound") != SQLITE_OK)
   {
      SetDBError(
         XO("Unable to switch to fast journaling mode")
      );

      return false;
   }

   // Install our schema into the new database
   if (!InstallSchema(db, "outbound"))
   {
      // Message already set
      return false;
   }

   {
      // Ensure statement gets cleaned up
      sqlite3_stmt *stmt = nullptr;
      auto cleanup = finally([&]
      {
         if (stmt)
         {
            // No need to check return code
            sqlite3_finalize(stmt);
         }
      });

      // Prepare the statement only once
      rc = sqlite3_prepare_v2(db,
                              "INSERT INTO outbound.sampleblocks"
                              "  SELECT * FROM main.sampleblocks"
                              "  WHERE blockid = ?;",
                              -1,
                              &stmt,
                              nullptr);
      if (rc != SQLITE_OK)
      {
         SetDBError(
            XO("Unable to prepare project file command:\n\n%s").Format(sql)
         );
         return false;
      }

      /* i18n-hint: This title appears on a dialog that indicates the progress
         in doing something.*/
      ProgressDialog progress(XO("Progress"), msg, pdlgHideStopButton);
      ProgressResult result = ProgressResult::Success;

      wxLongLong_t count = 0;
      wxLongLong_t total = blockids.size();

      // Start a transaction.  Since we're running without a journal,
      // this really doesn't provide rollback.  It just prevents SQLite
      // from auto committing after each step through the loop.
      //
      // Also note that we will have an open transaction if we fail
      // while copying the blocks. This is fine since we're just going
      // to delete the database anyway.
      sqlite3_exec(db, "BEGIN;", nullptr, nullptr, nullptr);

      // Copy sample blocks from the main DB to the outbound DB
      for (auto blockid : blockids)
      {
         // Bind statement parameters
         rc = sqlite3_bind_int64(stmt, 1, blockid);
         if (rc != SQLITE_OK)
         {
            SetDBError(
               XO("Failed to bind SQL parameter")
            );

            return false;
         }

         // Process it
         rc = sqlite3_step(stmt);
         if (rc != SQLITE_DONE)
         {
            SetDBError(
               XO("Failed to update the project file.\nThe following command failed:\n\n%s").Format(sql)
            );
            return false;
         }

         // Reset statement to beginning
         if (sqlite3_reset(stmt) != SQLITE_OK)
         {
            THROW_INCONSISTENCY_EXCEPTION;
         }

         result = progress.Update(++count, total);
         if (result != ProgressResult::Success)
         {
            // Note that we're not setting success, so the finally
            // block above will take care of cleaning up
            return false;
         }
      }

      // Write the doc.
      //
      // If we're compacting a temporary project (user initiated from the File
      // menu), then write the doc to the "autosave" table since temporary
      // projects do not have a "project" doc.
      if (!WriteDoc(isTemporary ? "autosave" : "project", doc, "outbound"))
      {
         return false;
      }

      // See BEGIN above...
      sqlite3_exec(db, "COMMIT;", nullptr, nullptr, nullptr);
   }

   // Detach the destination database
   rc = sqlite3_exec(db, "DETACH DATABASE outbound;", nullptr, nullptr, nullptr);
   if (rc != SQLITE_OK)
   {
      SetDBError(
         XO("Destination project could not be detached")
      );

      return false;
   }

   // Tell cleanup everything is good to go
   success = true;

   return true;
}

bool ProjectFileIO::ShouldCompact(const std::vector<const TrackList *> &tracks)
{
   SampleBlockIDSet active;
   unsigned long long current = 0;

   {
      auto fn = BlockSpaceUsageAccumulator( current );
      for (auto pTracks : tracks)
         if (pTracks)
            InspectBlocks( *pTracks, fn,
               &active // Visit unique blocks only
            );
   }

   // Get the number of blocks and total length from the project file.
   unsigned long long total = GetTotalUsage();
   unsigned long long blockcount = 0;
   
   auto cb = [&blockcount](int cols, char **vals, char **)
   {
      // Convert
      wxString(vals[0]).ToULongLong(&blockcount);
      return 0;
   };

   if (!Query("SELECT Count(*) FROM sampleblocks;", cb) || blockcount == 0)
   {
      // Shouldn't compact since we don't have the full picture
      return false;
   }

   // Remember if we had unused blocks in the project file
   mHadUnused = (blockcount > active.size());

   // Let's make a percentage...should be plenty of head room
   current *= 100;

   wxLogDebug(wxT("used = %lld total = %lld %lld"), current, total, total ? current / total : 0);
   if (!total || current / total > 80)
   {
      wxLogDebug(wxT("not compacting"));
      return false;
   }
   wxLogDebug(wxT("compacting"));

   return true;
}

Connection &ProjectFileIO::CurrConn()
{
   auto &connectionPtr = ConnectionPtr::Get( mProject );
   return connectionPtr.mpConnection;
}

const std::vector<wxString> &ProjectFileIO::AuxiliaryFileSuffixes()
{
   static const std::vector<wxString> strings {
      "-wal",
#ifndef NO_SHM
      "-shm",
#endif
   };
   return strings;
}

FilePath ProjectFileIO::SafetyFileName(const FilePath &src)
{
   wxFileNameWrapper fn{ src };

   // Extra characters inserted into filename before extension
   wxString extra =
#ifdef __WXGTK__
      wxT("~")
#else
      wxT(".bak")
#endif
   ;

   int nn = 1;
   auto numberString = [](int num) -> wxString {
      return num == 1 ? wxString{} : wxString::Format(".%d", num);
   };

   auto suffixes = AuxiliaryFileSuffixes();
   suffixes.push_back({});

   // Find backup paths not already occupied; check all auxiliary suffixes
   const auto name = fn.GetName();
   FilePath result;
   do {
      fn.SetName( name + numberString(nn++) + extra );
      result = fn.GetFullPath();
   }
   while( std::any_of(suffixes.begin(), suffixes.end(), [&](auto &suffix){
      return wxFileExists(result + suffix);
   }) );

   return result;
}

bool ProjectFileIO::RenameOrWarn(const FilePath &src, const FilePath &dst)
{
   std::atomic_bool done = {false};
   bool success = false;
   auto thread = std::thread([&]
   {
      success = wxRenameFile(src, dst);
      done = true;
   });

   auto &window = GetProjectFrame( mProject );

   // Provides a progress dialog with indeterminate mode
   wxGenericProgressDialog pd(XO("Copying Project").Translation(),
                              XO("This may take several seconds").Translation(),
                              300000,     // range
                              &window,     // parent
                              wxPD_APP_MODAL | wxPD_ELAPSED_TIME | wxPD_SMOOTH);

   // Wait for the checkpoints to end
   while (!done)
   {
      wxMilliSleep(50);
      pd.Pulse();
   }
   thread.join();

   if (!success)
   {
      ShowError(
         &window,
         XO("Error Writing to File"),
         XO("Audacity failed to write file %s.\n"
            "Perhaps disk is full or not writable.\n"
            "For tips on freeing up space, click the help button.")
            .Format(dst),
         "Error:_Disk_full_or_not_writable"
         );
      return false;
   }

   return true;
}

bool ProjectFileIO::MoveProject(const FilePath &src, const FilePath &dst)
{
   // Assume the src database file is not busy.
   if (!RenameOrWarn(src, dst))
      return false;

   // So far so good, but the separate -wal and -shm files might yet exist,
   // as when checkpointing failed for limited space on the drive.
   // If so move them too or else lose data.

   std::vector< std::pair<FilePath, FilePath> > pairs{ { src, dst } };
   bool success = false;
   auto cleanup = finally([&]{
      if (!success) {
         // If any one of the renames failed, back out the previous ones.
         // This should be a no-fail recovery!  Not clear what to do if any
         // of these renames fails.
         for (auto &pair : pairs) {
            if (!(pair.first.empty() && pair.second.empty()))
               wxRenameFile(pair.second, pair.first);
         }
      }
   });

   for (const auto &suffix : AuxiliaryFileSuffixes()) {
      auto srcName = src + suffix;
      if (wxFileExists(srcName)) {
         auto dstName = dst + suffix;
         if (!RenameOrWarn(srcName, dstName))
            return false;
         pairs.push_back({ srcName, dstName });
      }
   }

   return (success = true);
}

bool ProjectFileIO::RemoveProject(const FilePath &filename)
{
   if (!wxFileExists(filename))
      return false;

   bool success = wxRemoveFile(filename);
   auto &suffixes = AuxiliaryFileSuffixes();
   for (const auto &suffix : suffixes) {
      auto file = filename + suffix;
      if (wxFileExists(file))
         success = wxRemoveFile(file) && success;
   }
   return success;
}

ProjectFileIO::BackupProject::BackupProject(
   ProjectFileIO &projectFileIO, const FilePath &path )
{
   auto safety = SafetyFileName(path);
   if (!projectFileIO.MoveProject(path, safety))
      return;

   mPath = path;
   mSafety = safety;
}

void ProjectFileIO::BackupProject::Discard()
{
   if (!mPath.empty()) {
      // Succeeded; don't need the safety files
      RemoveProject(mSafety);
      mSafety.clear();
   }
}

ProjectFileIO::BackupProject::~BackupProject()
{
   if (!mPath.empty()) {
      if (!mSafety.empty()) {
         // Failed; restore from safety files
         auto suffixes = AuxiliaryFileSuffixes();
         suffixes.push_back({});
         for (const auto &suffix : suffixes) {
            auto path = mPath + suffix;
            if (wxFileExists(path))
               wxRemoveFile(path);
            wxRenameFile(mSafety + suffix, mPath + suffix);
         }
      }
   }
}

void ProjectFileIO::Compact(
   const std::vector<const TrackList *> &tracks, bool force)
{
   // Haven't compacted yet
   mWasCompacted = false;

   // Assume we have unused blocks until we find out otherwise. That way cleanup
   // at project close time will still occur.
   mHadUnused = true;

   // If forcing compaction, bypass inspection.
   if (!force)
   {
      // Don't compact if this is a temporary project or if it's determined there are not
      // enough unused blocks to make it worthwhile.
      if (IsTemporary() || !ShouldCompact(tracks))
      {
         // Delete the AutoSave doc it if exists
         if (IsModified())
         {
            // PRL:  not clear what to do if the following fails, but the worst should
            // be, the project may reopen in its present state as a recovery file, not
            // at the last saved state.
            // REVIEW: Could the autosave file be corrupt though at that point, and so 
            // prevent recovery?
            // LLL: I believe Paul is correct since it's deleted with a single SQLite
            // transaction. The next time the file opens will just invoke recovery.
            (void) AutoSaveDelete();
         }

         return;
      }
   }

   wxString origName = mFileName;
   wxString backName = origName + "_compact_back";
   wxString tempName = origName + "_compact_temp";

   // Copy the original database to a new database. Only prune sample blocks if
   // we have a tracklist.
   // REVIEW: Compact can fail on the CopyTo with no error messages.  That's OK?
   // LLL: We could display an error message or just ignore the failure and allow
   // the file to be compacted the next time it's saved.
   if (CopyTo(tempName, XO("Compacting project"), IsTemporary(), !tracks.empty(), tracks))
   {
      // Must close the database to rename it
      if (CloseConnection())
      {
         // Only use the new file if it is actually smaller than the original.
         //
         // If the original file doesn't have anything to compact (original and new
         // are basically identical), the file could grow by a few pages because of
         // differences in how SQLite constructs the b-tree.
         //
         // In this case, just toss the new file and continue to use the original.
         //
         // Also, do this after closing the connection so that the -wal file
         // gets cleaned up.
         if (wxFileName::GetSize(tempName) < wxFileName::GetSize(origName))
         {
            // Rename the original to backup
            if (wxRenameFile(origName, backName))
            {
               // Rename the temporary to original
               if (wxRenameFile(tempName, origName))
               {
                  // Open the newly compacted original file
                  if (OpenConnection(origName))
                  {
                     // Remove the old original file
                     if (!wxRemoveFile(backName))
                     {
                        // Just log the error, nothing can be done to correct it
                        // and WX should have logged another message showing the
                        // system error code.
                        wxLogWarning(wxT("Compaction failed to delete backup %s"), backName);
                     }

                     // Remember that we compacted
                     mWasCompacted = true;

                     return;
                  }
                  else
                  {
                     wxLogWarning(wxT("Compaction failed to open new project %s"), origName);
                  }

                  if (!wxRenameFile(origName, tempName))
                  {
                     wxLogWarning(wxT("Compaction failed to rename orignal %s to temp %s"),
                                  origName, tempName);
                  }
               }
               else
               {
                  wxLogWarning(wxT("Compaction failed to rename temp %s to orig %s"),
                                 origName, tempName);
               }

               if (!wxRenameFile(backName, origName))
               {
                  wxLogWarning(wxT("Compaction failed to rename back %s to orig %s"),
                               backName, origName);
               }
            }
            else
            {
               wxLogWarning(wxT("Compaction failed to rename orig %s to back %s"),
                              backName, origName);
            }
         }

         if (!OpenConnection(origName))
         {
            wxLogWarning(wxT("Compaction failed to reopen %s"), origName);
         }
      }

      // Did not achieve any real compaction
      // RemoveProject not needed for what was an attached database
      if (!wxRemoveFile(tempName))
      {
         // Just log the error, nothing can be done to correct it
         // and WX should have logged another message showing the
         // system error code.
         wxLogWarning(wxT("Failed to delete temporary file...ignoring"));
      }
   }

   return;
}

bool ProjectFileIO::WasCompacted()
{
   return mWasCompacted;
}

bool ProjectFileIO::HadUnused()
{
   return mHadUnused;
}

void ProjectFileIO::UpdatePrefs()
{
   SetProjectTitle();
}

// Pass a number in to show project number, or -1 not to.
void ProjectFileIO::SetProjectTitle(int number)
{
   auto &project = mProject;
   auto pWindow = project.GetFrame();
   if (!pWindow)
   {
      return;
   }
   auto &window = *pWindow;
   wxString name = project.GetProjectName();

   // If we are showing project numbers, then we also explicitly show "<untitled>" if there
   // is none.
   if (number >= 0)
   {
      name =
      /* i18n-hint: The %02i is the project number, the %s is the project name.*/
      XO("[Project %02i] Audacity \"%s\"")
         .Format( number + 1,
                 name.empty() ? XO("<untitled>") : Verbatim((const char *)name))
         .Translation();
   }
   // If we are not showing numbers, then <untitled> shows as 'Audacity'.
   else if (name.empty())
   {
      name = _TS("Audacity");
   }

   if (mRecovered)
   {
      name += wxT(" ");
      /* i18n-hint: E.g this is recovered audio that had been lost.*/
      name += _("(Recovered)");
   }

   if (name != window.GetTitle())
   {
      window.SetTitle( name );
      window.SetName(name);       // to make the nvda screen reader read the correct title

      project.QueueEvent(
         safenew wxCommandEvent{ EVT_PROJECT_TITLE_CHANGE } );
   }
}

const FilePath &ProjectFileIO::GetFileName() const
{
   return mFileName;
}

void ProjectFileIO::SetFileName(const FilePath &fileName)
{
   auto &project = mProject;

   if (!mFileName.empty())
   {
      ActiveProjects::Remove(mFileName);
   }

   mFileName = fileName;

   if (!mFileName.empty())
   {
      ActiveProjects::Add(mFileName);
   }

   if (IsTemporary())
   {
      project.SetProjectName({});
   }
   else
   {
      project.SetProjectName(wxFileName(mFileName).GetName());
   }

   SetProjectTitle();
}

bool ProjectFileIO::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   auto &project = mProject;
   auto &window = GetProjectFrame(project);
   auto &viewInfo = ViewInfo::Get(project);
   auto &settings = ProjectSettings::Get(project);

   wxString fileVersion;
   wxString audacityVersion;
   int requiredTags = 0;
   long longVpos = 0;

   // loop through attrs, which is a null-terminated list of
   // attribute-value pairs
   while (*attrs)
   {
      const wxChar *attr = *attrs++;
      const wxChar *value = *attrs++;

      if (!value || !XMLValueChecker::IsGoodString(value))
      {
         break;
      }

      if (viewInfo.ReadXMLAttribute(attr, value))
      {
         // We need to save vpos now and restore it below
         longVpos = std::max(longVpos, long(viewInfo.vpos));
         continue;
      }

      else if (!wxStrcmp(attr, wxT("version")))
      {
         fileVersion = value;
         requiredTags++;
      }

      else if (!wxStrcmp(attr, wxT("audacityversion")))
      {
         audacityVersion = value;
         requiredTags++;
      }

      else if (!wxStrcmp(attr, wxT("rate")))
      {
         double rate;
         Internat::CompatibleToDouble(value, &rate);
         settings.SetRate( rate );
      }

      else if (!wxStrcmp(attr, wxT("snapto")))
      {
         settings.SetSnapTo(wxString(value) == wxT("on") ? true : false);
      }

      else if (!wxStrcmp(attr, wxT("selectionformat")))
      {
         settings.SetSelectionFormat(
            NumericConverter::LookupFormat( NumericConverter::TIME, value) );
      }

      else if (!wxStrcmp(attr, wxT("audiotimeformat")))
      {
         settings.SetAudioTimeFormat(
            NumericConverter::LookupFormat( NumericConverter::TIME, value) );
      }

      else if (!wxStrcmp(attr, wxT("frequencyformat")))
      {
         settings.SetFrequencySelectionFormatName(
            NumericConverter::LookupFormat( NumericConverter::FREQUENCY, value ) );
      }

      else if (!wxStrcmp(attr, wxT("bandwidthformat")))
      {
         settings.SetBandwidthSelectionFormatName(
            NumericConverter::LookupFormat( NumericConverter::BANDWIDTH, value ) );
      }
   } // while

   if (longVpos != 0)
   {
      // PRL: It seems this must happen after SetSnapTo
      viewInfo.vpos = longVpos;
   }

   if (requiredTags < 2)
   {
      return false;
   }

   // Parse the file version from the project
   int fver;
   int frel;
   int frev;
   if (!wxSscanf(fileVersion, wxT("%i.%i.%i"), &fver, &frel, &frev))
   {
      return false;
   }

   // Parse the file version Audacity was build with
   int cver;
   int crel;
   int crev;
   wxSscanf(wxT(AUDACITY_FILE_FORMAT_VERSION), wxT("%i.%i.%i"), &cver, &crel, &crev);

   int fileVer = ((fver *100)+frel)*100+frev;
   int codeVer = ((cver *100)+crel)*100+crev;

   if (codeVer<fileVer)
   {
      /* i18n-hint: %s will be replaced by the version number.*/
      auto msg = XO("This file was saved using Audacity %s.\nYou are using Audacity %s. You may need to upgrade to a newer version to open this file.")
         .Format(audacityVersion, AUDACITY_VERSION_STRING);

      ShowError(
         &window,
         XO("Can't open project file"),
         msg, 
         "FAQ:Errors_opening_an_Audacity_project"
         );

      return false;
   }

   if (wxStrcmp(tag, wxT("project")))
   {
      return false;
   }

   // All other tests passed, so we succeed
   return true;
}

XMLTagHandler *ProjectFileIO::HandleXMLChild(const wxChar *tag)
{
   auto &project = mProject;
   auto fn = ProjectFileIORegistry::Lookup(tag);
   if (fn)
   {
      return fn(project);
   }

   return nullptr;
}

void ProjectFileIO::OnCheckpointFailure()
{
   wxCommandEvent evt{ EVT_CHECKPOINT_FAILURE };
   mProject.ProcessEvent(evt);
}

void ProjectFileIO::WriteXMLHeader(XMLWriter &xmlFile) const
{
   xmlFile.Write(wxT("<?xml "));
   xmlFile.Write(wxT("version=\"1.0\" "));
   xmlFile.Write(wxT("standalone=\"no\" "));
   xmlFile.Write(wxT("?>\n"));

   xmlFile.Write(wxT("<!DOCTYPE "));
   xmlFile.Write(wxT("project "));
   xmlFile.Write(wxT("PUBLIC "));
   xmlFile.Write(wxT("\"-//audacityproject-1.3.0//DTD//EN\" "));
   xmlFile.Write(wxT("\"http://audacity.sourceforge.net/xml/audacityproject-1.3.0.dtd\" "));
   xmlFile.Write(wxT(">\n"));
}

void ProjectFileIO::WriteXML(XMLWriter &xmlFile,
                             bool recording /* = false */,
                             const TrackList *tracks /* = nullptr */)
// may throw
{
   auto &proj = mProject;
   auto &tracklist = tracks ? *tracks : TrackList::Get(proj);
   auto &viewInfo = ViewInfo::Get(proj);
   auto &tags = Tags::Get(proj);
   const auto &settings = ProjectSettings::Get(proj);

   //TIMER_START( "AudacityProject::WriteXML", xml_writer_timer );

   xmlFile.StartTag(wxT("project"));
   xmlFile.WriteAttr(wxT("xmlns"), wxT("http://audacity.sourceforge.net/xml/"));

   xmlFile.WriteAttr(wxT("version"), wxT(AUDACITY_FILE_FORMAT_VERSION));
   xmlFile.WriteAttr(wxT("audacityversion"), AUDACITY_VERSION_STRING);

   viewInfo.WriteXMLAttributes(xmlFile);
   xmlFile.WriteAttr(wxT("rate"), settings.GetRate());
   xmlFile.WriteAttr(wxT("snapto"), settings.GetSnapTo() ? wxT("on") : wxT("off"));
   xmlFile.WriteAttr(wxT("selectionformat"),
                     settings.GetSelectionFormat().Internal());
   xmlFile.WriteAttr(wxT("frequencyformat"),
                     settings.GetFrequencySelectionFormatName().Internal());
   xmlFile.WriteAttr(wxT("bandwidthformat"),
                     settings.GetBandwidthSelectionFormatName().Internal());

   tags.WriteXML(xmlFile);

   unsigned int ndx = 0;
   tracklist.Any().Visit([&](const Track *t)
   {
      auto useTrack = t;
      if ( recording ) {
         // When append-recording, there is a temporary "shadow" track accumulating
         // changes and displayed on the screen but it is not yet part of the
         // regular track list.  That is the one that we want to back up.
         // SubstitutePendingChangedTrack() fetches the shadow, if the track has
         // one, else it gives the same track back.
         useTrack = t->SubstitutePendingChangedTrack().get();
      }
      else if ( useTrack->GetId() == TrackId{} ) {
         // This is a track added during a non-appending recording that is
         // not yet in the undo history.  The UndoManager skips backing it up
         // when pushing.  Don't auto-save it.
         return;
      }
      useTrack->WriteXML(xmlFile);
   });

   xmlFile.EndTag(wxT("project"));

   //TIMER_STOP( xml_writer_timer );
}

bool ProjectFileIO::AutoSave(bool recording)
{
   ProjectSerializer autosave;
   WriteXMLHeader(autosave);
   WriteXML(autosave, recording);

   if (WriteDoc("autosave", autosave))
   {
      mModified = true;
      return true;
   }

   return false;
}

bool ProjectFileIO::AutoSaveDelete(sqlite3 *db /* = nullptr */)
{
   int rc;

   if (!db)
   {
      db = DB();
   }

   rc = sqlite3_exec(db, "DELETE FROM autosave;", nullptr, nullptr, nullptr);
   if (rc != SQLITE_OK)
   {
      SetDBError(
         XO("Failed to remove the autosave information from the project file.")
      );
      return false;
   }

   mModified = false;

   return true;
}

bool ProjectFileIO::WriteDoc(const char *table,
                             const ProjectSerializer &autosave,
                             const char *schema /* = "main" */)
{
   auto db = DB();
   int rc;

   // For now, we always use an ID of 1. This will replace the previously
   // written row every time.
   char sql[256];
   sqlite3_snprintf(sizeof(sql),
                    sql,
                    "INSERT INTO %s.%s(id, dict, doc) VALUES(1, ?1, ?2)"
                    "       ON CONFLICT(id) DO UPDATE SET dict = ?1, doc = ?2;",
                    schema,
                    table);

   sqlite3_stmt *stmt = nullptr;
   auto cleanup = finally([&]
   {
      if (stmt)
      {
         sqlite3_finalize(stmt);
      }
   });

   rc = sqlite3_prepare_v2(db, sql, -1, &stmt, nullptr);
   if (rc != SQLITE_OK)
   {
      SetDBError(
         XO("Unable to prepare project file command:\n\n%s").Format(sql)
      );
      return false;
   }

   const wxMemoryBuffer &dict = autosave.GetDict();
   const wxMemoryBuffer &data = autosave.GetData();

   // Bind statement parameters
   // Might return SQL_MISUSE which means it's our mistake that we violated
   // preconditions; should return SQL_OK which is 0
   if (sqlite3_bind_blob(stmt, 1, dict.GetData(), dict.GetDataLen(), SQLITE_STATIC) ||
       sqlite3_bind_blob(stmt, 2, data.GetData(), data.GetDataLen(), SQLITE_STATIC))
   {
      SetDBError(
         XO("Unable to bind to blob")
      );
      return false;
   }

   rc = sqlite3_step(stmt);
   if (rc != SQLITE_DONE)
   {
      SetDBError(
         XO("Failed to update the project file.\nThe following command failed:\n\n%s").Format(sql)
      );
      return false;
   }

   return true;
}

bool ProjectFileIO::LoadProject(const FilePath &fileName, bool ignoreAutosave)
{
   bool success = false;

   auto cleanup = finally([&]
   {
      if (!success)
      {
         RestoreConnection();
      }
   });

   SaveConnection();

   // Open the project file
   if (!OpenConnection(fileName))
   {
      return false;
   }

   wxString project;
   wxMemoryBuffer buffer;
   bool usedAutosave = true;

   // Get the autosave doc, if any
   if (!ignoreAutosave &&
       !GetBlob("SELECT dict || doc FROM autosave WHERE id = 1;", buffer))
   {
      // Error already set
      return false;
   }
 
   // If we didn't have an autosave doc, load the project doc instead
   if (buffer.GetDataLen() == 0)
   {
      usedAutosave = false;

      if (!GetBlob("SELECT dict || doc FROM project WHERE id = 1;", buffer))
      {
         // Error already set
         return false;
      }
   }

   // Missing both the autosave and project docs. This can happen if the
   // system were to crash before the first autosave into a temporary file.
   // This should be a recoverable scenario.
   if (buffer.GetDataLen() == 0)
   {
      mRecovered = true;
   }
   else
   {
      project = ProjectSerializer::Decode(buffer);
      if (project.empty())
      {
         SetError(XO("Unable to decode project document"));

         return false;
      }

      XMLFileReader xmlFile;

      // Load 'er up
      success = xmlFile.ParseString(this, project);
      if (!success)
      {
         SetError(
            XO("Unable to parse project information."),
            xmlFile.GetErrorStr()
         );
         return false;
      }

      // Check for orphans blocks...sets mRecovered if any were deleted
      
      auto blockids = WaveTrackFactory::Get( mProject )
         .GetSampleBlockFactory()
            ->GetActiveBlockIDs();
      if (blockids.size() > 0)
      {
         success = DeleteBlocks(blockids, true);
         if (!success)
         {
            return false;
         }
      }
   
      // Remember if we used autosave or not
      if (usedAutosave)
      {
         mRecovered = true;
      }
   }

   // Mark the project modified if we recovered it
   if (mRecovered)
   {
      mModified = true;
   }

   // A previously saved project will have a document in the project table, so
   // we use that knowledge to determine if this file is an unsaved/temporary
   // file or a permanent project file
   wxString result;
   success = GetValue("SELECT Count(*) FROM project;", result);
   if (!success)
   {
      return false;
   }

   mTemporary = !result.IsSameAs(wxT("1"));

   SetFileName(fileName);

   DiscardConnection();

   success = true;

   return true;
}

bool ProjectFileIO::UpdateSaved(const TrackList *tracks)
{
   ProjectSerializer doc;
   WriteXMLHeader(doc);
   WriteXML(doc, false, tracks);

   if (!WriteDoc("project", doc))
   {
      return false;
   }

   // Autosave no longer needed
   if (!AutoSaveDelete())
   {
      return false;
   }

   return true;
}

// REVIEW: This function is believed to report an error to the user in all cases 
// of failure.  Callers are believed not to need to do so if they receive 'false'.
// LLL: All failures checks should now be displaying an error.
bool ProjectFileIO::SaveProject(
   const FilePath &fileName, const TrackList *lastSaved)
{
   // In the case where we're saving a temporary project to a permanent project,
   // we'll try to simply rename the project to save a bit of time. We then fall
   // through to the normal Save (not SaveAs) processing.
   if (IsTemporary() && mFileName != fileName)
   {
      FilePath savedName = mFileName;
      if (CloseConnection())
      {
         bool reopened = false;
         bool moved = false;
         if (true == (moved = MoveProject(savedName, fileName)))
         {
            if (OpenConnection(fileName))
               reopened = true;
            else {
               MoveProject(fileName, savedName);
               moved = false; // No longer moved

               reopened = OpenConnection(savedName);
            }
         }
         else {
            // Rename can fail -- if it's to a different device, requiring
            // real copy of contents, which might exhaust space
            reopened = OpenConnection(savedName);
         }

         // Warning issued in MoveProject()
         if (reopened && !moved) {
            return false;
         }

         if (!reopened) {
            wxTheApp->CallAfter([this]{
               ShowError(nullptr,
                  XO("Warning"),
                  XO(
"The project's database failed to reopen, "
"possibly because of limited space on the storage device."),
                  "Error:_Disk_full_or_not_writable"
               );
               wxCommandEvent evt{ EVT_RECONNECTION_FAILURE };
               mProject.ProcessEvent(evt);
            });

            return false;
         }
      }
   }

   // If we're saving to a different file than the current one, then copy the
   // current to the new file and make it the active file.
   if (mFileName != fileName)
   {
      // Do NOT prune here since we need to retain the Undo history
      // after we switch to the new file.
      if (!CopyTo(fileName, XO("Saving project"), false))
      {
         ShowError(
            nullptr,
            XO("Error Saving Project"),
            FileException::WriteFailureMessage(fileName),
            "Error:_Disk_full_or_not_writable"
            );
         return false;
      }

      // Open the newly created database
      Connection newConn = std::make_unique<DBConnection>(
         mProject.shared_from_this(), mpErrors,
         [this]{ OnCheckpointFailure(); });

      // NOTE: There is a noticeable delay here when dealing with large multi-hour
      //       projects that we just created. The delay occurs in Open() when it
      //       calls SafeMode() and is due to the switch from the NONE journal mode
      //       to the WAL journal mode.
      //
      //       So, we do the Open() in a thread and display a progress dialog. Since
      //       this is currently the only known instance where this occurs, we do the
      //       threading here. If more instances are identified, then the threading
      //       should be moved to DBConnection::Open(), wrapping the SafeMode() call
      //       there.
      {
         std::atomic_bool done = {false};
         bool success = true;
         auto thread = std::thread([&]
         {
            auto rc =  newConn->Open(fileName);
            if (rc != SQLITE_OK)
            {
               // Capture the error string
               SetError(Verbatim(sqlite3_errstr(rc)));
               success = false;
            }
            done = true;
         });

         // Provides a progress dialog with indeterminate mode
         wxGenericProgressDialog pd(XO("Syncing").Translation(),
                                    XO("This may take several seconds").Translation(),
                                    300000,     // range
                                    nullptr,    // parent
                                    wxPD_APP_MODAL | wxPD_ELAPSED_TIME | wxPD_SMOOTH);

         // Wait for the checkpoints to end
         while (!done)
         {
            wxMilliSleep(50);
            pd.Pulse();
         }
         thread.join();

         if (!success)
         {
            // Additional help via a Help button links to the manual.
            ShowError(nullptr,
                      XO("Error Saving Project"),
                      XO("The project failed to open, possibly due to limited space\n"
                         "on the storage device.\n\n%s").Format(GetLastError()),
                      "Error:_Disk_full_or_not_writable");

            newConn = nullptr;

            // Clean up the destination project
            if (!wxRemoveFile(fileName))
            {
               wxLogMessage("Failed to remove destination project after open failure: %s", fileName);
            }

            return false;
         }
      }

      // Autosave no longer needed in original project file.
      if (!AutoSaveDelete())
      {
         // Additional help via a Help button links to the manual.
         ShowError(nullptr,
                   XO("Error Saving Project"),
                   XO("Unable to remove autosave information, possibly due to limited space\n"
                      "on the storage device.\n\n%s").Format(GetLastError()),
                  "Error:_Disk_full_or_not_writable");

         newConn = nullptr;

         // Clean up the destination project
         if (!wxRemoveFile(fileName))
         {
            wxLogMessage("Failed to remove destination project after AutoSaveDelete failure: %s", fileName);
         }

         return false;
      }

      if (lastSaved) {
         // Bug2605: Be sure not to save orphan blocks
         bool recovered = mRecovered;
         SampleBlockIDSet blockids;
         InspectBlocks( *lastSaved, {}, &blockids );
         // TODO: Not sure what to do if the deletion fails
         DeleteBlocks(blockids, true);
         // Don't set mRecovered if any were deleted
         mRecovered = recovered;
      }

      // Try to compact the original project file.
      auto empty = TrackList::Create(&mProject);
      Compact( { lastSaved ? lastSaved : empty.get() }, true );

      // Safe to close the original project file now. Not much we can do if this fails,
      // but we should still be in good shape since we'll be switching to the newly
      // saved database below.
      CloseProject();

      // And make it the active project file 
      UseConnection(std::move(newConn), fileName);
   }
   else
   {
      if ( !UpdateSaved( nullptr ) ) {
         ShowError(
            nullptr,
            XO("Error Saving Project"),
            FileException::WriteFailureMessage(fileName),
            "Error:_Disk_full_or_not_writable"
            );
         return false;
      }
   }

   // Reaching this point defines success and all the rest are no-fail
   // operations:

   // No longer modified
   mModified = false;

   // No longer recovered
   mRecovered = false;

   // No longer a temporary project
   mTemporary = false;

   // Adjust the title
   SetProjectTitle();

   return true;
}

bool ProjectFileIO::SaveCopy(const FilePath& fileName)
{
   return CopyTo(fileName, XO("Backing up project"), false, true,
      {&TrackList::Get(mProject)});
}

bool ProjectFileIO::OpenProject()
{
   return OpenConnection();
}

bool ProjectFileIO::CloseProject()
{
   auto &currConn = CurrConn();
   if (!currConn)
   {
      wxLogDebug("Closing project with no database connection");
      return true;
   }

   // Save the filename since CloseConnection() will clear it
   wxString filename = mFileName;

   // Not much we can do if this fails.  The user will simply get
   // the recovery dialog upon next restart.
   if (CloseConnection())
   {
      // If this is a temporary project, we no longer want to keep the
      // project file.
      if (IsTemporary())
      {
         // This is just a safety check.
         wxFileName temp(TempDirectory::TempDir(), wxT(""));
         wxFileName file(filename);
         file.SetFullName(wxT(""));
         if (file == temp)
            RemoveProject(filename);
      }
   }

   return true;
}

bool ProjectFileIO::ReopenProject()
{
   FilePath fileName = mFileName;
   if (!CloseConnection())
   {
      return false;
   }

   return OpenConnection(fileName);
}

bool ProjectFileIO::IsModified() const
{
   return mModified;
}

bool ProjectFileIO::IsTemporary() const
{
   return mTemporary;
}

bool ProjectFileIO::IsRecovered() const
{
   return mRecovered;
}

wxLongLong ProjectFileIO::GetFreeDiskSpace() const
{
   wxLongLong freeSpace;
   if (wxGetDiskSpace(wxPathOnly(mFileName), NULL, &freeSpace))
   {
      if (FileNames::IsOnFATFileSystem(mFileName)) {
         // 4 GiB per-file maximum
         constexpr auto limit = 1ll << 32;

         // Opening a file only to find its length looks wasteful but
         // seems to be necessary at least on Windows with FAT filesystems.
         // I don't know if that is only a wxWidgets bug.
         auto length = wxFile{mFileName}.Length();
         // auto length = wxFileName::GetSize(mFileName);

         if (length == wxInvalidSize)
            length = 0;
         auto free = std::max<wxLongLong>(0, limit - length);
         freeSpace = std::min(freeSpace, free);
      }
      return freeSpace;
   }

   return -1;
}

/// Displays an error dialog with a button that offers help
void ProjectFileIO::ShowError(wxWindow *parent,
                              const TranslatableString &dlogTitle,
                              const TranslatableString &message,
                              const wxString &helpPage)
{
   ShowExceptionDialog(parent, dlogTitle, message, helpPage, true,
                   audacity::ToWString(GetLastLog()));
}

const TranslatableString &ProjectFileIO::GetLastError() const
{
   return mpErrors->mLastError;
}

const TranslatableString &ProjectFileIO::GetLibraryError() const
{
   return mpErrors->mLibraryError;
}

int ProjectFileIO::GetLastErrorCode() const
{
    return mpErrors->mErrorCode;
}

const wxString &ProjectFileIO::GetLastLog() const
{
    return mpErrors->mLog;
}

void ProjectFileIO::SetError(
    const TranslatableString& msg, const TranslatableString& libraryError, int errorCode)
{
   auto &currConn = CurrConn();
   if (currConn)
      currConn->SetError(msg, libraryError, errorCode);
}

void ProjectFileIO::SetDBError(
   const TranslatableString &msg, const TranslatableString &libraryError, int errorCode)
{
   auto &currConn = CurrConn();
   if (currConn)
      currConn->SetDBError(msg, libraryError, errorCode);
}

void ProjectFileIO::SetBypass()
{
   auto &currConn = CurrConn();
   if (!currConn)
      return;

   // Determine if we can bypass sample block deletes during shutdown.
   //
   // IMPORTANT:
   // If the project was compacted, then we MUST bypass further
   // deletions since the new file doesn't have the blocks that the
   // Sequences expect to be there.

   currConn->SetBypass( true );

   // Only permanent project files need cleaning at shutdown
   if (!IsTemporary() && !WasCompacted())
   {
      // If we still have unused blocks, then we must not bypass deletions
      // during shutdown.  Otherwise, we would have orphaned blocks the next time
      // the project is opened.
      //
      // An example of when dead blocks will exist is when a user opens a permanent
      // project, adds a track (with samples) to it, and chooses not to save the
      // changes.
      if (HadUnused())
      {
         currConn->SetBypass( false );
      }
   }

   return;
}

int64_t ProjectFileIO::GetBlockUsage(SampleBlockID blockid)
{
   auto pConn = CurrConn().get();
   if (!pConn)
      return 0;
   return GetDiskUsage(*pConn, blockid);
}

int64_t ProjectFileIO::GetCurrentUsage(
   const std::vector<const TrackList*> &trackLists) const
{
   unsigned long long current = 0;
   const auto fn = BlockSpaceUsageAccumulator(current);

   // Must pass address of this set, even if not otherwise used, to avoid
   // possible multiple count of shared blocks
   SampleBlockIDSet seen;
   for (auto pTracks: trackLists)
      if (pTracks)
         InspectBlocks(*pTracks, fn, &seen);

   return current;
}

int64_t ProjectFileIO::GetTotalUsage()
{
   auto pConn = CurrConn().get();
   if (!pConn)
      return 0;
   return GetDiskUsage(*pConn, 0);
}

//
// Returns the amount of disk space used by the specified sample blockid or all
// of the sample blocks if the blockid is 0.  It does this by using the raw SQLite
// pages available from the "sqlite_dbpage" virtual table to traverse the SQLite
// table b-tree described here:  https://www.sqlite.org/fileformat.html
//
int64_t ProjectFileIO::GetDiskUsage(DBConnection &conn, SampleBlockID blockid /* = 0 */)
{
   // Information we need to track our travels through the b-tree
   typedef struct
   {
      int64_t pgno;
      int currentCell;
      int numCells;
      unsigned char data[65536];
   } page;
   std::vector<page> stack;

   int64_t total = 0;
   int64_t found = 0;
   int64_t right = 0;
   int rc;

   // Get the rootpage for the sampleblocks table.
   sqlite3_stmt *stmt =
      conn.Prepare(DBConnection::GetRootPage,
                    "SELECT rootpage FROM sqlite_master WHERE tbl_name = 'sampleblocks';");
   if (stmt == nullptr || sqlite3_step(stmt) != SQLITE_ROW)
   {
      return 0;
   }

   // And store it in our first stack frame
   stack.push_back({sqlite3_column_int64(stmt, 0)});

   // All done with the statement
   sqlite3_clear_bindings(stmt);
   sqlite3_reset(stmt);

   // Prepare/retrieve statement to read raw database page
   stmt = conn.Prepare(DBConnection::GetDBPage,
      "SELECT data FROM sqlite_dbpage WHERE pgno = ?1;");
   if (stmt == nullptr)
   {
      return 0;
   }

   // Traverse the b-tree until we've visited all of the leaf pages or until
   // we find the one corresponding to the passed in sample blockid. Because we
   // use an integer primary key for the sampleblocks table, the traversal will
   // be in ascending blockid sequence.
   do
   {
      // Acces the top stack frame
      page &pg = stack.back();

      // Read the page from the sqlite_dbpage table if it hasn't yet been loaded
      if (pg.numCells == 0)
      {
         // Bind the page number
         sqlite3_bind_int64(stmt, 1, pg.pgno);

         // And retrieve the page
         if (sqlite3_step(stmt) != SQLITE_ROW)
         {
            // REVIEW: Likely harmless failure - says size is zero on
            // this error.
            // LLL: Yea, but not much else we can do.
            return 0;
         }

         // Copy the page content to the stack frame
         memcpy(&pg.data,
                sqlite3_column_blob(stmt, 0),
                sqlite3_column_bytes(stmt, 0));

         // And retrieve the total number of cells within it
         pg.numCells = get2(&pg.data[3]);

         // Reset statement for next usage
         sqlite3_clear_bindings(stmt);
         sqlite3_reset(stmt);
      }

      //wxLogDebug("%*.*spgno %lld currentCell %d numCells %d", (stack.size() - 1) * 2, (stack.size() - 1) * 2, "", pg.pgno, pg.currentCell, pg.numCells);

      // Process an interior table b-tree page
      if (pg.data[0] == 0x05)
      {
         // Process the next cell if we haven't examined all of them yet
         if (pg.currentCell < pg.numCells)
         {
            // Remember the right-most leaf page number.
            right = get4(&pg.data[8]);

            // Iterate over the cells.
            //
            // If we're not looking for a specific blockid, then we always push the
            // target page onto the stack and leave the loop after a single iteration.
            //
            // Otherwise, we match the blockid against the highest integer key contained
            // within the cell and if the blockid falls within the cell, we stack the
            // page and stop the iteration.
            //
            // In theory, we could do a binary search for a specific blockid here, but
            // because our sample blocks are always large, we will get very few cells
            // per page...usually 6 or less.
            //
            // In both cases, the stacked page can be either an internal or leaf page.
            bool stacked = false;
            while (pg.currentCell < pg.numCells)
            {
               // Get the offset to this cell using the offset in the cell pointer
               // array.
               //
               // The cell pointer array starts immediately after the page header
               // at offset 12 and the retrieved offset is from the beginning of
               // the page.
               int celloff = get2(&pg.data[12 + (pg.currentCell * 2)]);

               // Bump to the next cell for the next iteration.
               pg.currentCell++;

               // Get the page number this cell describes
               int pagenum = get4(&pg.data[celloff]);

               // And the highest integer key, which starts at offset 4 within the cell.
               int64_t intkey = 0;
               get_varint(&pg.data[celloff + 4], &intkey);

               //wxLogDebug("%*.*sinternal - right %lld celloff %d pagenum %d intkey %lld", (stack.size() - 1) * 2, (stack.size() - 1) * 2, " ", right, celloff, pagenum, intkey);

               // Stack the described page if we're not looking for a specific blockid
               // or if this page contains the given blockid.
               if (!blockid || blockid <= intkey)
               {
                  stack.push_back({pagenum, 0, 0});
                  stacked = true;
                  break;
               }
            }

            // If we pushed a new page onto the stack, we need to jump back up
            // to read the page
            if (stacked)
            {
               continue;
            }
         }

         // We've exhausted all the cells with this page, so we stack the right-most
         // leaf page.  Ensure we only process it once.
         if (right)
         {
            stack.push_back({right, 0, 0});
            right = 0;
            continue;
         }
      }
      // Process a leaf table b-tree page
      else if (pg.data[0] == 0x0d)
      {
         // Iterate over the cells
         //
         // If we're not looking for a specific blockid, then just accumulate the
         // payload sizes. We will be reading every leaf page in the sampleblocks
         // table.
         //
         // Otherwise we break out when we find the matching blockid. In this case,
         // we only ever look at 1 leaf page.
         bool stop = false;
         for (int i = 0; i < pg.numCells; i++)
         {
            // Get the offset to this cell using the offset in the cell pointer
            // array.
            //
            // The cell pointer array starts immediately after the page header
            // at offset 8 and the retrieved offset is from the beginning of
            // the page.
            int celloff = get2(&pg.data[8 + (i * 2)]);

            // Get the total payload size in bytes of the described row.
            int64_t payload = 0;
            int digits = get_varint(&pg.data[celloff], &payload);

            // Get the integer key for this row.
            int64_t intkey = 0;
            get_varint(&pg.data[celloff + digits], &intkey);

            //wxLogDebug("%*.*sleaf - celloff %4d intkey %lld payload %lld", (stack.size() - 1) * 2, (stack.size() - 1) * 2, " ", celloff, intkey, payload);

            // Add this payload size to the total if we're not looking for a specific
            // blockid
            if (!blockid)
            {
               total += payload;
            }
            // Otherwise, return the payload size for a matching row
            else if (blockid == intkey)
            {
               return payload;
            }
         }
      }

      // Done with the current branch, so pop back up to the previous one (if any)
      stack.pop_back();
   } while (!stack.empty());

   // Return the total used for all sample blocks
   return total;
}

// Retrieves a 2-byte big-endian integer from the page data
unsigned int ProjectFileIO::get2(const unsigned char *ptr)
{
   return (ptr[0] << 8) | ptr[1];
}

// Retrieves a 4-byte big-endian integer from the page data
unsigned int ProjectFileIO::get4(const unsigned char *ptr)
{
   return ((unsigned int) ptr[0] << 24) |
          ((unsigned int) ptr[1] << 16) |
          ((unsigned int) ptr[2] << 8)  |
          ((unsigned int) ptr[3]);
}

// Retrieves a variable length integer from the page data. Returns the
// number of digits used to encode the integer and the stores the
// value at the given location.
int ProjectFileIO::get_varint(const unsigned char *ptr, int64_t *out)
{
   int64_t val = 0;
   int i;

   for (i = 0; i < 8; ++i)
   {
      val = (val << 7) + (ptr[i] & 0x7f);
      if ((ptr[i] & 0x80) == 0)
      {
         *out = val;
         return i + 1;
      }
   }

   val = (val << 8) + (ptr[i] & 0xff);
   *out = val;

   return 9;
}

InvisibleTemporaryProject::InvisibleTemporaryProject()
   : mpProject{ std::make_shared< AudacityProject >() }
{
}

InvisibleTemporaryProject::~InvisibleTemporaryProject()
{
   auto &projectFileIO = ProjectFileIO::Get( Project() );
   projectFileIO.SetBypass();
   auto &tracks = TrackList::Get( Project() );
   tracks.Clear();

   // Consume some delayed track list related events before destroying the
   // temporary project
   try { wxTheApp->Yield(); } catch(...) {}

   // Destroy the project and yield again to let delayed window deletions happen
   projectFileIO.CloseProject();
   mpProject.reset();
   try { wxTheApp->Yield(); } catch(...) {}
}
