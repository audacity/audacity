/**********************************************************************

Audacity: A Digital Audio Editor

DBConection.h

Paul Licameli -- split from ProjectFileIO.h

**********************************************************************/

#ifndef __AUDACITY_DB_CONNECTION__
#define __AUDACITY_DB_CONNECTION__

#include <atomic>
#include <condition_variable>
#include <map>
#include <memory>
#include <mutex>
#include <thread>

#include "ClientData.h"

struct sqlite3;
struct sqlite3_stmt;
class wxString;
class AudacityProject;

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
      DeleteSampleBlock,
      GetRootPage,
      GetDBPage
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
   std::atomic_bool mCheckpointPending{ false };
   std::atomic_bool mCheckpointActive{ false };

   std::map<enum StatementID, sqlite3_stmt *> mStatements;

   // Bypass transactions if database will be deleted after close
   bool mBypass;
};

using Connection = std::unique_ptr<DBConnection>;

// This object attached to the project simply holds the pointer to the
// project's current database connection, which is initialized on demand,
// and may be redirected, temporarily or permanently, to another connection
// when backing the project up or saving or saving-as.
class ConnectionPtr final
   : public ClientData::Base
   , public std::enable_shared_from_this< ConnectionPtr >
{
public:
   static ConnectionPtr &Get( AudacityProject &project );
   static const ConnectionPtr &Get( const AudacityProject &project );

   ~ConnectionPtr() override;

   Connection mpConnection;
};

#endif
