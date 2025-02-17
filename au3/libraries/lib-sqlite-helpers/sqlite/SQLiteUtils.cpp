/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: SQLiteUtils.cpp
 * SPDX-FileContributor: Dmitry Vedenko
 */

#include "SQLiteUtils.h"

#include "sqlite3.h"

namespace audacity::sqlite {
namespace {
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
        mError = Error(sqlite3_config(SQLITE_CONFIG_URI, 1));

        if (mError.IsError()) {
            return;
        }

        mError
            =Error(sqlite3_config(SQLITE_CONFIG_LOG, SQLiteLogCallback, this));

        if (mError.IsError()) {
            return;
        }

        mError = Error(sqlite3_initialize());

#ifdef NO_SHM
        if (mError.IsError()) {
            return;
        }

        // Use the "unix-excl" VFS to make access to the DB exclusive.  This
        // gets rid of the "<database name>-shm" shared memory file.
        //
        // Though it shouldn't, it doesn't matter if this fails.
        auto vfs = sqlite3_vfs_find("unix-excl");
        if (vfs) {
            sqlite3_vfs_register(vfs, 1);
        }
#endif
    }

    ~SQLiteIniter()
    {
        // This function must be called single-threaded only
        // It returns a value, but there's nothing we can do with it
        (void)sqlite3_shutdown();
    }

    static void SQLiteLogCallback(void* initer, int code, const char* msg)
    {
        SQLiteIniter* self = static_cast<SQLiteIniter*>(initer);

        if (self->mLogCallback) {
            self->mLogCallback(code, msg);
        }
    }

    Error GetError() const noexcept
    {
        return mError;
    }

    void SetLogCallback(LogCallback callback)
    {
        mLogCallback = std::move(callback);
    }

private:
    Error mError;
    LogCallback mLogCallback;
};

static SQLiteIniter& GetIniter()
{
    static SQLiteIniter sIniter;
    return sIniter;
}
} // namespace

Error Initialize() noexcept
{
    return GetIniter().GetError();
}

void SetLogCallback(LogCallback callback)
{
    GetIniter().SetLogCallback(std::move(callback));
}
} // namespace audacity::sqlite
