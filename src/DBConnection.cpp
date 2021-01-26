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

#include "FileNames.h"
#include "Internat.h"
#include "Project.h"
#include "FileException.h"
#include "wxFileNameWrapper.h"

// Configuration to provide "safe" connections
static const char *SafeConfig =
   "PRAGMA <schema>.locking_mode = SHARED;"
   "PRAGMA <schema>.synchronous = NORMAL;"
   "PRAGMA <schema>.journal_mode = WAL;"
   "PRAGMA <schema>.wal_autocheckpoint = 0;";

// Configuration to provide "Fast" connections
static const char *FastConfig =
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
   mBypass = false;
}

DBConnection::~DBConnection()
{
   wxASSERT(mDB == nullptr);
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
   const TranslatableString &msg, const TranslatableString &libraryError)
{
   mpErrors->mLastError = msg;
   mpErrors->mLibraryError = libraryError;
}

void DBConnection::SetDBError(
   const TranslatableString &msg, const TranslatableString &libraryError)
{
   mpErrors->mLastError = msg;
   wxLogDebug(wxT("SQLite error: %s"), mpErrors->mLastError.Debug());
   printf("   Lib error: %s", mpErrors->mLastError.Debug().mb_str().data());

   mpErrors->mLibraryError = libraryError.empty()
      ? Verbatim(sqlite3_errmsg(DB())) : libraryError;
   wxLogDebug(wxT("   Lib error: %s"), mpErrors->mLibraryError.Debug());
   printf("   Lib error: %s", mpErrors->mLibraryError.Debug().mb_str().data());
}

bool DBConnection::Open(const char *fileName)
{
   wxASSERT(mDB == nullptr);
   int rc;

   rc = sqlite3_open(fileName, &mDB);
   if (rc != SQLITE_OK)
   {
      sqlite3_close(mDB);
      mDB = nullptr;

      return false;
   }

   // Set default mode
   // (See comments in ProjectFileIO::SaveProject() about threading
   SafeMode();

   // Kick off the checkpoint thread
   mCheckpointStop = false;
   mCheckpointPending = false;
   mCheckpointActive = false;
   mCheckpointThread = std::thread([this]{ CheckpointThread(); });

   // Install our checkpoint hook
   sqlite3_wal_hook(mDB, CheckpointHook, this);

   return mDB;
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
   mCheckpointThread.join();

   // We're done with the prepared statements
   for (auto stmt : mStatements)
   {
      // No need to check return code.
      sqlite3_finalize(stmt.second);
   }
   mStatements.clear();

   // Close the DB
   rc = sqlite3_close(mDB);
   if (rc != SQLITE_OK)
   {
      // I guess we could try to recover by repreparing statements and reinstalling
      // the hook, but who knows if that would work either.
      //
      // Should we throw an error???
      //
      // LLL: Probably not worthwhile since the DB will just be recovered when
      // next opened.
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

bool DBConnection::SafeMode(const char *schema /* = "main" */)
{
   return ModeConfig(mDB, schema, SafeConfig);
}

bool DBConnection::FastMode(const char *schema /* = "main" */)
{
   return ModeConfig(mDB, schema, FastConfig);
}

bool DBConnection::ModeConfig(sqlite3 *db, const char *schema, const char *config)
{
   // Ensure attached DB connection gets configured
   int rc;

   // Replace all schema "keywords" with the schema name
   wxString sql = config;
   sql.Replace(wxT("<schema>"), schema);

   // Set the configuration
   rc = sqlite3_exec(db, sql, nullptr, nullptr, nullptr);

   return rc == SQLITE_OK;
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
   int rc;

   // Return an existing statement if it's already been prepared
   auto iter = mStatements.find(id);
   if (iter != mStatements.end())
   {
      return iter->second;
   }

   // Prepare the statement
   sqlite3_stmt *stmt = nullptr;
   rc = sqlite3_prepare_v3(mDB, sql, -1, SQLITE_PREPARE_PERSISTENT, &stmt, 0);
   if (rc != SQLITE_OK)
   {
      wxLogDebug("prepare error %s", sqlite3_errmsg(mDB));
      THROW_INCONSISTENCY_EXCEPTION;
   }

   // And remember it
   mStatements.insert({id, stmt});

   return stmt;
}

sqlite3_stmt *DBConnection::GetStatement(enum StatementID id)
{
   // Look it up
   auto iter = mStatements.find(id);

   // It should always be there
   wxASSERT(iter != mStatements.end());

   // Return it
   return iter->second;
}


void DBConnection::CheckpointThread()
{
   // Open another connection to the DB to prevent blocking the main thread.
   //
   // If it fails, then we won't checkpoint until the main thread closes
   // the associated DB.
   sqlite3 *db = nullptr;
   const auto name = sqlite3_db_filename(mDB, nullptr);
   bool giveUp = false;
   if (sqlite3_open(name, &db) == SQLITE_OK)
   {
      // Configure it to be safe
      ModeConfig(db, "main", SafeConfig);

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
         int rc;
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

         if (rc != SQLITE_OK) {
            // Can't checkpoint -- maybe the device has too little space
            wxFileNameWrapper fName{ name };
            auto path = FileNames::AbbreviatePath(fName);
            auto name = fName.GetFullName();
            auto longname = name + "-wal";
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
               [&message] {
               throw SimpleMessageBoxException{
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
   }

   // All done (always close)
   sqlite3_close(db);

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
      throw SimpleMessageBoxException( 
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
      }
   }
}

bool TransactionScope::Commit()
{
   if ( !mInTrans )
      // Misuse of this class
      THROW_INCONSISTENCY_EXCEPTION;

   mInTrans = !TransactionCommit(mName);

   return mInTrans;
}

ConnectionPtr::~ConnectionPtr()
{
   wxASSERT_MSG(!mpConnection, wxT("Project file was not closed at shutdown"));
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

