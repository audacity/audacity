/*!********************************************************************

Audacity: A Digital Audio Editor

@file DBConnection.cpp
@brief Implements DBConnection

Paul Licameli -- split from ProjectFileIO.cpp

**********************************************************************/

#include "DBConnection.h"

#include "sqlite3.h"

#include <wx/progdlg.h>
#include <wx/string.h>

#include "AudacityLogger.h"
#include "FileNames.h"
#include "Internat.h"
#include "Project.h"
#include "FileException.h"
#include "wxFileNameWrapper.h"

// Configuration to provide "safe" connections
static const char *SafeConfig =
   "PRAGMA <schema>.busy_timeout = 5000;"
   "PRAGMA <schema>.locking_mode = SHARED;"
   "PRAGMA <schema>.synchronous = NORMAL;"
   "PRAGMA <schema>.journal_mode = WAL;"
   "PRAGMA <schema>.wal_autocheckpoint = 0;";

// Configuration to provide "Fast" connections
static const char *FastConfig =
   "PRAGMA <schema>.busy_timeout = 5000;"
   "PRAGMA <schema>.locking_mode = SHARED;"
   "PRAGMA <schema>.synchronous = OFF;"
   "PRAGMA <schema>.journal_mode = OFF;";

DBConnection::DBConnection(
   const std::weak_ptr<AudacityProject> &pProject,
   const std::shared_ptr<DBConnectionErrors> &pErrors,
   CheckpointFailureCallback callback)
: mpProject{ pProject }
, mpErrors{ pErrors }
, mCallback{ std::move(callback) }
{
   mDB = nullptr;
   mCheckpointDB = nullptr;
   mBypass = false;
}

DBConnection::~DBConnection()
{
   wxASSERT(mDB == nullptr);
   if (mDB)
   {
      wxLogMessage("Database left open at connection destruction %s\n",
                   sqlite3_db_filename(mDB, nullptr));
   }
}

void DBConnection::SetBypass( bool bypass )
{
   mBypass = bypass;
}

bool DBConnection::ShouldBypass()
{
   return mBypass;
}

void DBConnection::SetError(
   const TranslatableString &msg, const TranslatableString &libraryError, int errorCode)
{
   mpErrors->mErrorCode = errorCode;

   mpErrors->mLastError = msg;

   mpErrors->mLibraryError = errorCode && libraryError.empty()
      ? XO("(%d): %s").Format(errorCode, sqlite3_errstr(errorCode))
      : libraryError;

   wxLogMessage("DBConnection SetError\n"
                "\tErrorCode: %d\n"
                "\tLastError: %s\n"
                "\tLibraryError: %s",
                 mpErrors->mErrorCode,
                 mpErrors->mLastError.Debug(),
                 mpErrors->mLibraryError.Debug());

   auto logger = AudacityLogger::Get();
   if (logger)
   {
      mpErrors->mLog = logger->GetLog(10);
   }
}

void DBConnection::SetDBError(
   const TranslatableString &msg, const TranslatableString &libraryError, int errorCode)
{
   auto db = DB();

   mpErrors->mErrorCode = errorCode < 0 && db
      ? sqlite3_errcode(db)
      : errorCode;

   mpErrors->mLastError = msg.empty()
      ? XO("(%d): %s").Format(mpErrors->mErrorCode, sqlite3_errstr(mpErrors->mErrorCode))
      : msg;

   mpErrors->mLibraryError = libraryError.empty() && db
      ? Verbatim(sqlite3_errmsg(db))
      : libraryError;

   wxLogMessage("DBConnection SetDBError\n"
                "\tErrorCode: %d\n"
                "\tLastError: %s\n"
                "\tLibraryError: %s",
                 mpErrors->mErrorCode,
                 mpErrors->mLastError.Debug(),
                 mpErrors->mLibraryError.Debug());

   auto logger = AudacityLogger::Get();
   if (logger)
   {
      mpErrors->mLog = logger->GetLog(10);
   }
}

int DBConnection::Open(const FilePath fileName)
{
   wxASSERT(mDB == nullptr);
   int rc;

   // Initialize checkpoint controls
   mCheckpointStop = false;
   mCheckpointPending = false;
   mCheckpointActive = false;
   rc = OpenStepByStep( fileName );
   if ( rc != SQLITE_OK)
   {
      if (mCheckpointDB)
      {
         sqlite3_close(mCheckpointDB);
         mCheckpointDB = nullptr;
      }

      if (mDB)
      {
         sqlite3_close(mDB);
         mDB = nullptr;
      }
   }
   return rc;
}

int DBConnection::OpenStepByStep(const FilePath fileName)
{
   const char *name = fileName.ToUTF8();

   bool success = false;
   int rc = sqlite3_open(name, &mDB);
   if (rc != SQLITE_OK) 
   {
      wxLogMessage("Failed to open primary connection to %s: %d, %s\n",
         fileName,
         rc,
         sqlite3_errstr(rc));
      return rc;
   }

   // Set default mode
   // (See comments in ProjectFileIO::SaveProject() about threading
   rc = SafeMode();
   if (rc != SQLITE_OK)
   {
      SetDBError(XO("Failed to set safe mode on primary connection to %s").Format(fileName));
      return rc;
   }

   rc = sqlite3_open(name, &mCheckpointDB);
   if (rc != SQLITE_OK)
   {
      wxLogMessage("Failed to open checkpoint connection to %s: %d, %s\n",
         fileName,
         rc,
         sqlite3_errstr(rc));
      return rc;
   }

   rc = ModeConfig(mCheckpointDB, "main", SafeConfig);
   if (rc != SQLITE_OK) {
      SetDBError(XO("Failed to set safe mode on checkpoint connection to %s").Format(fileName));
      return rc;
   }

   auto db = mCheckpointDB;
   mCheckpointThread = std::thread(
      [this, db, fileName]{ CheckpointThread(db, fileName); });

   // Install our checkpoint hook
   sqlite3_wal_hook(mDB, CheckpointHook, this);
   return rc;
}

bool DBConnection::Close()
{
   wxASSERT(mDB != nullptr);
   int rc;

   // Protect...
   if (mDB == nullptr)
   {
      return true;
   }

   // Uninstall our checkpoint hook so that no additional checkpoints
   // are sent our way.  (Though this shouldn't really happen.)
   sqlite3_wal_hook(mDB, nullptr, nullptr);

   // Display a progress dialog if there's active or pending checkpoints
   if (mCheckpointPending || mCheckpointActive)
   {
      TranslatableString title = XO("Checkpointing project");

      // Get access to the active project
      auto project = mpProject.lock();
      if (project)
      {
         title = XO("Checkpointing %s").Format(project->GetProjectName());
      }

      // Provides a progress dialog with indeterminate mode
      wxGenericProgressDialog pd(title.Translation(),
                                 XO("This may take several seconds").Translation(),
                                 300000,     // range
                                 nullptr,    // parent
                                 wxPD_APP_MODAL | wxPD_ELAPSED_TIME | wxPD_SMOOTH);

      // Wait for the checkpoints to end
      while (mCheckpointPending || mCheckpointActive)
      {
         wxMilliSleep(50);
         pd.Pulse();
      }
   }

   // Tell the checkpoint thread to shutdown
   {
      std::lock_guard<std::mutex> guard(mCheckpointMutex);
      mCheckpointStop = true;
      mCheckpointCondition.notify_one();
   }

   // And wait for it to do so
   if (mCheckpointThread.joinable())
   {
      mCheckpointThread.join();
   }

   // We're done with the prepared statements
   {
      std::lock_guard<std::mutex> guard(mStatementMutex);
      for (auto stmt : mStatements)
      {
         // No need to process return code, but log it for diagnosis
         rc = sqlite3_finalize(stmt.second);
         if (rc != SQLITE_OK)
         {
            wxLogMessage("Failed to finalize statement on %s\n"
                         "\tErrMsg: %s\n"
                         "\tSQL: %s",
                         sqlite3_db_filename(mDB, nullptr), 
                         sqlite3_errmsg(mDB),
                         stmt.second);
         }
      }
      mStatements.clear();
   }

   // Not much we can do if the closes fail, so just report the error

   // Close the checkpoint connection
   rc = sqlite3_close(mCheckpointDB);
   if (rc != SQLITE_OK)
   {
      wxLogMessage("Failed to close checkpoint connection for %s\n"
                   "\tError: %s\n",
                   sqlite3_db_filename(mCheckpointDB, nullptr),
                   sqlite3_errmsg(mCheckpointDB));
   }
   mCheckpointDB = nullptr;

   // Close the primary connection
   rc = sqlite3_close(mDB);
   if (rc != SQLITE_OK)
   {
      wxLogMessage("Failed to close %s\n"
                   "\tError: %s\n",
                   sqlite3_db_filename(mDB, nullptr),
                   sqlite3_errmsg(mDB));
   }
   mDB = nullptr;

   return true;
}

[[noreturn]] void DBConnection::ThrowException( bool write ) const
{
   // Sqlite3 documentation says returned character string
   // does NOT require freeing by us.
   wxString dbName{ sqlite3_db_filename(mDB, "main") };
   // Now we have an absolute path.  Throw a message box exception that
   // formats a helpful message just as used to be done before sqlite3
   // was used for projects.
   throw FileException{
      write ? FileException::Cause::Write : FileException::Cause::Read,
      dbName
   };
}

int DBConnection::SafeMode(const char *schema /* = "main" */)
{
   return ModeConfig(mDB, schema, SafeConfig);
}

int DBConnection::FastMode(const char *schema /* = "main" */)
{
   return ModeConfig(mDB, schema, FastConfig);
}

int DBConnection::ModeConfig(sqlite3 *db, const char *schema, const char *config)
{
   // Ensure attached DB connection gets configured
   int rc;

   // Replace all schema "keywords" with the schema name
   wxString sql = config;
   sql.Replace(wxT("<schema>"), schema);

   // Set the configuration
   rc = sqlite3_exec(db, sql, nullptr, nullptr, nullptr);
   if (rc != SQLITE_OK)
   {
      // Don't store in connection, just report it
      wxLogMessage("Failed to set mode on %s\n"
                   "\tError: %s\n"
                   "\tSQL: %s",
                   sqlite3_db_filename(mDB, nullptr), 
                   sqlite3_errmsg(mDB),
                   sql);
   }

   return rc;
}

sqlite3 *DBConnection::DB()
{
   wxASSERT(mDB != nullptr);

   return mDB;
}

int DBConnection::GetLastRC() const
{
   return sqlite3_errcode(mDB);
}

const wxString DBConnection::GetLastMessage() const
{
   return sqlite3_errmsg(mDB);
}

sqlite3_stmt *DBConnection::Prepare(enum StatementID id, const char *sql)
{
   std::lock_guard<std::mutex> guard(mStatementMutex);

   int rc;
   // See bug 2673
   // We must not use the same prepared statement from two different threads.
   // Therefore, in the cache, use the thread id too.
   StatementIndex ndx(id, std::this_thread::get_id());

   // Return an existing statement if it's already been prepared
   auto iter = mStatements.find(ndx);
   if (iter != mStatements.end())
   {
      return iter->second;
   }

   // Prepare the statement
   sqlite3_stmt *stmt = nullptr;
   rc = sqlite3_prepare_v3(mDB, sql, -1, SQLITE_PREPARE_PERSISTENT, &stmt, 0);
   if (rc != SQLITE_OK)
   {
      wxLogMessage("Failed to prepare statement for %s\n"
                   "\tError: %s\n"
                   "\tSQL: %s",
                   sqlite3_db_filename(mDB, nullptr), 
                   sqlite3_errmsg(mDB),
                   sql);

      // TODO: Look into why this causes an access violation
      THROW_INCONSISTENCY_EXCEPTION;
   }

   // There are a small number (10 or so) of different id's corresponding 
   // to different SQL statements, see enum StatementID
   // We have relatively few threads running at any one time,
   // e.g. main gui thread, a playback thread, a thread for compacting.
   // However the cache might keep growing, as we start/stop audio,
   // perhaps, if we chose to use a new thread each time.
   // For 3.0.0 I think that's OK.  If it's a data leak it's a slow 
   // enough one.  wxLogDebugs seem to show that the audio play thread
   // is being reused, not recreated with a new ID, i.e. no leak at all.
   // ANSWER-ME Just how serious is the data leak?  How best to fix?

   // Remember the cached statement.
   mStatements.insert({ndx, stmt});

   return stmt;
}

void DBConnection::CheckpointThread(sqlite3 *db, const FilePath &fileName)
{
   int rc = SQLITE_OK;
   bool giveUp = false;

   while (true)
   {
      {
         // Wait for work or the stop signal
         std::unique_lock<std::mutex> lock(mCheckpointMutex);
         mCheckpointCondition.wait(lock,
                                   [&]
                                   {
                                      return mCheckpointPending || mCheckpointStop;
                                   });

         // Requested to stop, so bail
         if (mCheckpointStop)
         {
            break;
         }

         // Capture the number of pages that need checkpointing and reset
         mCheckpointActive = true;
         mCheckpointPending = false;
      }

      // And kick off the checkpoint. This may not checkpoint ALL frames
      // in the WAL.  They'll be gotten the next time around.
      using namespace std::chrono;
      do {
         rc = giveUp ? SQLITE_OK :
            sqlite3_wal_checkpoint_v2(
               db, nullptr, SQLITE_CHECKPOINT_PASSIVE, nullptr, nullptr);
      }
      // Contentions for an exclusive lock on the database are possible,
      // even while the main thread is merely drawing the tracks, which
      // may perform reads
      while (rc == SQLITE_BUSY && (std::this_thread::sleep_for(1ms), true));

      // Reset
      mCheckpointActive = false;

      if (rc != SQLITE_OK)
      {
         wxLogMessage("Failed to perform checkpoint on %s\n"
                      "\tErrCode: %d\n"
                      "\tErrMsg: %s",
                      fileName,
                      sqlite3_errcode(db),
                      sqlite3_errmsg(db));

         // Can't checkpoint -- maybe the device has too little space
         wxFileNameWrapper fName{ fileName };
         auto path = FileNames::AbbreviatePath(fName);
         auto name = fName.GetFullName();
         auto longname = name + "-wal";

         // TODO: Should we return the actual error message if it's not a
         // disk full condition?
         auto message1 = rc == SQLITE_FULL
            ? XO("Could not write to %s.\n").Format(path)
            : TranslatableString{};
         auto message = XO(
            "Disk is full.\n"
            "%s\n"
            "For tips on freeing up space, click the help button."
         ).Format(message1);

         // Stop trying to checkpoint
         giveUp = true;

         // Stop the audio.
         GuardedCall(
            [&message, rc] {
            throw SimpleMessageBoxException{ rc != SQLITE_FULL ? ExceptionType::Internal : ExceptionType::BadEnvironment,
               message, XO("Warning"), "Error:_Disk_full_or_not_writable" }; },
            SimpleGuard<void>{},
            [this](AudacityException * e) {
               // This executes in the main thread.
               if (mCallback)
                  mCallback();
               if (e)
                  e->DelayedHandlerAction();
            }
         );
      }
   }

   return;
}

int DBConnection::CheckpointHook(void *data, sqlite3 *db, const char *schema, int pages)
{
   // Get access to our object
   DBConnection *that = static_cast<DBConnection *>(data);

   // Queue the database pointer for our checkpoint thread to process
   std::lock_guard<std::mutex> guard(that->mCheckpointMutex);
   that->mCheckpointPending = true;
   that->mCheckpointCondition.notify_one();

   return SQLITE_OK;
}

bool TransactionScope::TransactionStart(const wxString &name)
{
   char *errmsg = nullptr;

   int rc = sqlite3_exec(mConnection.DB(),
                         wxT("SAVEPOINT ") + name + wxT(";"),
                         nullptr,
                         nullptr,
                         &errmsg);

   if (errmsg)
   {
      mConnection.SetDBError(
         XO("Failed to create savepoint:\n\n%s").Format(name)
      );
      sqlite3_free(errmsg);
   }

   return rc == SQLITE_OK;
}

bool TransactionScope::TransactionCommit(const wxString &name)
{
   char *errmsg = nullptr;

   int rc = sqlite3_exec(mConnection.DB(),
                         wxT("RELEASE ") + name + wxT(";"),
                         nullptr,
                         nullptr,
                         &errmsg);

   if (errmsg)
   {
      mConnection.SetDBError(
         XO("Failed to release savepoint:\n\n%s").Format(name)
      );
      sqlite3_free(errmsg);
   }

   return rc == SQLITE_OK;
}

bool TransactionScope::TransactionRollback(const wxString &name)
{
   char *errmsg = nullptr;

   int rc = sqlite3_exec(mConnection.DB(),
                         wxT("ROLLBACK TO ") + name + wxT(";"),
                         nullptr,
                         nullptr,
                         &errmsg);

   if (errmsg)
   {
      mConnection.SetDBError(
         XO("Failed to release savepoint:\n\n%s").Format(name)
      );
      sqlite3_free(errmsg);
   }

   return rc == SQLITE_OK;
}

TransactionScope::TransactionScope(
   DBConnection &connection, const char *name)
:  mConnection(connection),
   mName(name)
{
   mInTrans = TransactionStart(mName);
   if ( !mInTrans )
      // To do, improve the message
      throw SimpleMessageBoxException( ExceptionType::Internal,
         XO("Database error.  Sorry, but we don't have more details."), 
         XO("Warning"), 
         "Error:_Disk_full_or_not_writable"
      );
}

TransactionScope::~TransactionScope()
{
   if (mInTrans)
   {
      // Rollback AND REMOVE the transaction
      // -- must do both; rolling back a savepoint only rewinds it
      // without removing it, unlike the ROLLBACK command
      if (!(TransactionRollback(mName) &&
            TransactionCommit(mName) ) )
      {
         // Do not throw from a destructor!
         // This has to be a no-fail cleanup that does the best that it can.
         wxLogMessage("Transaction active at scope destruction");
      }
   }
}

bool TransactionScope::Commit()
{
   if ( !mInTrans )
   {
      wxLogMessage("No active transaction to commit");

      // Misuse of this class
      THROW_INCONSISTENCY_EXCEPTION;
   }

   mInTrans = !TransactionCommit(mName);

   return mInTrans;
}

ConnectionPtr::~ConnectionPtr()
{
   wxASSERT_MSG(!mpConnection, wxT("Project file was not closed at shutdown"));
   if (mpConnection)
   {
      wxLogMessage("Project file was not closed at connection destruction");
   }
}

static const AudacityProject::AttachedObjects::RegisteredFactory
sConnectionPtrKey{
   []( AudacityProject & ){
      // Ignore the argument; this is just a holder of a
      // unique_ptr to DBConnection, which must be filled in later
      // (when we can get a weak_ptr to the project)
      auto result = std::make_shared< ConnectionPtr >();
      return result;
   }
};

ConnectionPtr &ConnectionPtr::Get( AudacityProject &project )
{
   auto &result =
      project.AttachedObjects::Get< ConnectionPtr >( sConnectionPtrKey );
   return result;
}

const ConnectionPtr &ConnectionPtr::Get( const AudacityProject &project )
{
   return Get( const_cast< AudacityProject & >( project ) );
}

