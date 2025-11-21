/*!********************************************************************

Audacity: A Digital Audio Editor

@file DBConnection.h
@brief Declare DBConnection, which maintains database connection and associated status and background thread

Paul Licameli -- split from ProjectFileIO.h

**********************************************************************/

#ifndef __AUDACITY_DB_CONNECTION__
#define __AUDACITY_DB_CONNECTION__

#include <atomic>
#include <condition_variable>
#include <functional>
#include <map>
#include <memory>
#include <mutex>
#include <thread>
#include <chrono>

#include "ClientData.h"
#include "Identifier.h"

struct sqlite3;
struct sqlite3_stmt;
class wxString;
class AudacityProject;

struct DBConnectionErrors
{
    TranslatableString mLastError;
    TranslatableString mLibraryError;
    int mErrorCode { 0 };
    wxString mLog;
};

class PROJECT_FILE_IO_API DBConnection final
{
public:
    using CheckpointFailureCallback = std::function<void ()>;

    DBConnection(
        const std::weak_ptr<AudacityProject>& pProject, const std::shared_ptr<DBConnectionErrors>& pErrors, CheckpointFailureCallback callback /*!<
         Invoked in the main thread in idle time after detection of
         checkpoint failure, which might have been in a worker thread
      */
        );
    ~DBConnection();

    int Open(const FilePath fileName);
    bool Close();

    //! throw and show appropriate message box
    [[noreturn]] void ThrowException(
        bool write //!< If true, a database update failed; if false, only a SELECT failed
        ) const;

    int SafeMode(const char* schema = "main");
    int FastMode(const char* schema = "main");
    int SetPageSize(const char* schema = "main");

    bool Assign(sqlite3* handle);
    sqlite3* Detach();

    sqlite3* DB();

    int GetLastRC() const;
    const wxString GetLastMessage() const;

    enum StatementID
    {
        GetSamples,
        GetSummary256,
        GetSummary64k,
        LoadSampleBlock,
        InsertSampleBlock,
        DeleteSampleBlock,
        GetSampleBlockSize,
        GetAllSampleBlocksSize
    };
    sqlite3_stmt* Prepare(enum StatementID id, const char* sql);

    void SetBypass(bool bypass);
    bool ShouldBypass();

    //! Just set stored errors
    void SetError(
        const TranslatableString& msg, const TranslatableString& libraryError = {}, int errorCode = {});

    //! Set stored errors and write to log; and default libraryError to what database library reports
    void SetDBError(
        const TranslatableString& msg, const TranslatableString& libraryError = {}, int errorCode = -1);

private:
    int OpenStepByStep(const FilePath fileName);
    int ModeConfig(sqlite3* db, const char* schema, const char* config);

    void CheckpointThread(sqlite3* db, const FilePath& fileName);
    static int CheckpointHook(void* data, sqlite3* db, const char* schema, int pages);

private:
    std::weak_ptr<AudacityProject> mpProject;
    sqlite3* mDB;
    sqlite3* mCheckpointDB;

    std::thread mCheckpointThread;
    std::condition_variable mCheckpointCondition;
    std::mutex mCheckpointMutex;
    std::atomic_bool mCheckpointStop{ false };
    std::atomic_bool mCheckpointPending{ false };
    std::atomic_bool mCheckpointActive{ false };

    std::mutex mStatementMutex;
    using StatementIndex = std::pair<enum StatementID, std::thread::id>;
    std::map<StatementIndex, sqlite3_stmt*> mStatements;

    std::shared_ptr<DBConnectionErrors> mpErrors;
    CheckpointFailureCallback mCallback;

    // Bypass transactions if database will be deleted after close
    bool mBypass;
};

using Connection = std::unique_ptr<DBConnection>;

// This object attached to the project simply holds the pointer to the
// project's current database connection, which is initialized on demand,
// and may be redirected, temporarily or permanently, to another connection
// when backing the project up or saving or saving-as.
class ConnectionPtr final : public ClientData::Base, public std::enable_shared_from_this< ConnectionPtr >
{
public:
    static ConnectionPtr& Get(AudacityProject& project);
    static const ConnectionPtr& Get(const AudacityProject& project);

    ~ConnectionPtr() override;

    Connection mpConnection;
};

#endif
