/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: Error.cpp
 * SPDX-FileContributor: Dmitry Vedenko
 */
#include "Error.h"

#include <cassert>

#include "au3-exceptions/AudacityException.h"
#include "sqlite3.h"

namespace audacity::sqlite {
Error::Error() noexcept
    : mCode(SQLITE_OK)
{
}

Error::Error(int code) noexcept
    : mCode(code)
{
}

bool Error::IsError() const noexcept
{
    return mCode != SQLITE_OK && mCode != SQLITE_DONE && mCode != SQLITE_ROW;
}

bool Error::IsOk() const noexcept
{
    return !IsError();
}

Error::operator bool() const noexcept
{
    return IsOk();
}

void sqlite::Error::Raise() const
{
    assert(IsError());

    throw SimpleMessageBoxException(
              ExceptionType::Internal,
              TranslatableString::untranslatable("(%1) %2").arg(GetCode()).arg(GetErrorString()),
              TranslatableString("sqlite-helpers", "SQLite3 error"));
}

int Error::GetCode() const noexcept
{
    return mCode;
}

TranslatableString Error::GetErrorString() const
{
    switch (mCode) {
    case SQLITE_OK:
        /*: database operation was successful */
        return TranslatableString("sqlite-helpers", "No error");
    case SQLITE_ERROR:
        /*: database operation has failed, but there is no specific reason */
        return TranslatableString("sqlite-helpers", "Generic error");
    case SQLITE_INTERNAL:
        /*: database operation has failed due to the internal error */
        return TranslatableString("sqlite-helpers", "Internal logic error in SQLite");
    case SQLITE_PERM:
        /*: database operation has failed due to the permission error */
        return TranslatableString("sqlite-helpers", "Access permission denied");
    case SQLITE_ABORT:
        /*: database operation was aborted by the callback */
        return TranslatableString("sqlite-helpers", "Callback routine requested an abort");
    case SQLITE_BUSY:
        /*: database operation has failed because database is locked */
        return TranslatableString("sqlite-helpers", "The database file is locked");
    case SQLITE_LOCKED:
        /*: database operation has failed because table is locked */
        return TranslatableString("sqlite-helpers", "A table in the database is locked");
    case SQLITE_NOMEM:
        /*: database operation has failed due to the lack of memory */
        return TranslatableString("sqlite-helpers", "A malloc() failed");
    case SQLITE_READONLY:
        /*: database operation has failed because database is read-only */
        return TranslatableString("sqlite-helpers", "Attempt to write a read-only database");
    case SQLITE_INTERRUPT:
        /*: database operation was interrupted */
        return TranslatableString("sqlite-helpers", "Operation terminated");
    case SQLITE_IOERR:
        /*: database operation has failed due to the I/O failure */
        return TranslatableString("sqlite-helpers", "I/O error occurred");
    case SQLITE_CORRUPT:
        /*: database operation has failed due to the database corruption */
        return TranslatableString("sqlite-helpers", "The database disk image is malformed");
    case SQLITE_NOTFOUND:
        /*: database operation has failed because the requested item was not found */
        return TranslatableString("sqlite-helpers", "File not found");
    case SQLITE_FULL:
        /*: database operation has failed because the drive is full */
        return TranslatableString("sqlite-helpers", "Insertion failed because the drive is full");
    case SQLITE_CANTOPEN:
        /*: database operation has failed because the file cannot be opened */
        return TranslatableString("sqlite-helpers", "Unable to open the database file");
    case SQLITE_PROTOCOL:
        /*: database operation has failed because the lock protocol has failed */
        return TranslatableString("sqlite-helpers", "Database lock protocol error");
    case SQLITE_SCHEMA:
        /*: database operation has failed because the database schema has changed */
        return TranslatableString("sqlite-helpers", "The database schema changed");
    case SQLITE_TOOBIG:
        /*: database operation has failed because the string or BLOB exceeds size limit */
        return TranslatableString("sqlite-helpers", "String or BLOB exceeds size limit");
    case SQLITE_CONSTRAINT:
        /*: database operation has failed due to the constraint violation */
        return TranslatableString("sqlite-helpers", "Abort due to constraint violation");
    case SQLITE_MISMATCH:
        /*: database operation has failed due to the data type mismatch */
        return TranslatableString("sqlite-helpers", "Data type mismatch");
    case SQLITE_MISUSE:
        /*: database operation has failed due to the library misuse */
        return TranslatableString("sqlite-helpers", "Library used incorrectly");
    case SQLITE_NOLFS:
        /*: database operation has failed because the large file support is disabled */
        return TranslatableString("sqlite-helpers", "The large file support is disabled");
    case SQLITE_AUTH:
        /*: database operation has failed due to the authorization error */
        return TranslatableString("sqlite-helpers", "Authorization denied");
    case SQLITE_FORMAT:
        /*: database operation has failed due to the format error */
        return TranslatableString("sqlite-helpers", "Not used");
    case SQLITE_RANGE:
        /*: database operation has failed because the parameter is out of range */
        return TranslatableString("sqlite-helpers", "2nd parameter to sqlite3_bind out of range");
    case SQLITE_NOTADB:
        /*: database operation has failed because the file opened is not a database file */
        return TranslatableString("sqlite-helpers", "File opened that is not a database file ");
    default:
        /*: database operation has failed due to the unknown error */
        return TranslatableString("sqlite-helpers", "Unknown error");
    }
}
} // namespace audacity::sqlite
