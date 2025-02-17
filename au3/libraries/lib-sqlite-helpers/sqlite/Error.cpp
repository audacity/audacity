/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: Error.cpp
 * SPDX-FileContributor: Dmitry Vedenko
 */
#include "Error.h"

#include <cassert>

#include "AudacityException.h"
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
              Verbatim("(%d) %s").Format(GetCode(), GetErrorString()),
              XO("SQLite3 error"));
}

int Error::GetCode() const noexcept
{
    return mCode;
}

TranslatableString Error::GetErrorString() const
{
    switch (mCode) {
    case SQLITE_OK:
        /* i18n-hint: database operation was successful */
        return XO("No error");
    case SQLITE_ERROR:
        /* i18n-hint: database operation has failed, but there is no specific reason */
        return XO("Generic error");
    case SQLITE_INTERNAL:
        /* i18n-hint: database operation has failed due to the internal error */
        return XO("Internal logic error in SQLite");
    case SQLITE_PERM:
        /* i18n-hint: database operation has failed due to the permission error */
        return XO("Access permission denied");
    case SQLITE_ABORT:
        /* i18n-hint: database operation was aborted by the callback */
        return XO("Callback routine requested an abort");
    case SQLITE_BUSY:
        /* i18n-hint: database operation has failed because database is locked */
        return XO("The database file is locked");
    case SQLITE_LOCKED:
        /* i18n-hint: database operation has failed because table is locked */
        return XO("A table in the database is locked");
    case SQLITE_NOMEM:
        /* i18n-hint: database operation has failed due to the lack of memory */
        return XO("A malloc() failed");
    case SQLITE_READONLY:
        /* i18n-hint: database operation has failed because database is read-only */
        return XO("Attempt to write a read-only database");
    case SQLITE_INTERRUPT:
        /* i18n-hint: database operation was interrupted */
        return XO("Operation terminated");
    case SQLITE_IOERR:
        /* i18n-hint: database operation has failed due to the I/O failure */
        return XO("I/O error occurred");
    case SQLITE_CORRUPT:
        /* i18n-hint: database operation has failed due to the database corruption */
        return XO("The database disk image is malformed");
    case SQLITE_NOTFOUND:
        /* i18n-hint: database operation has failed because the requested item was not found */
        return XO("File not found");
    case SQLITE_FULL:
        /* i18n-hint: database operation has failed because the drive is full */
        return XO("Insertion failed because the drive is full");
    case SQLITE_CANTOPEN:
        /* i18n-hint: database operation has failed because the file cannot be opened */
        return XO("Unable to open the database file");
    case SQLITE_PROTOCOL:
        /* i18n-hint: database operation has failed because the lock protocol has failed */
        return XO("Database lock protocol error");
    case SQLITE_SCHEMA:
        /* i18n-hint: database operation has failed because the database schema has changed */
        return XO("The database schema changed");
    case SQLITE_TOOBIG:
        /* i18n-hint: database operation has failed because the string or BLOB exceeds size limit */
        return XO("String or BLOB exceeds size limit");
    case SQLITE_CONSTRAINT:
        /* i18n-hint: database operation has failed due to the constraint violation */
        return XO("Abort due to constraint violation");
    case SQLITE_MISMATCH:
        /* i18n-hint: database operation has failed due to the data type mismatch */
        return XO("Data type mismatch");
    case SQLITE_MISUSE:
        /* i18n-hint: database operation has failed due to the library misuse */
        return XO("Library used incorrectly");
    case SQLITE_NOLFS:
        /* i18n-hint: database operation has failed because the large file support is disabled */
        return XO("The large file support is disabled");
    case SQLITE_AUTH:
        /* i18n-hint: database operation has failed due to the authorization error */
        return XO("Authorization denied");
    case SQLITE_FORMAT:
        /* i18n-hint: database operation has failed due to the format error */
        return XO("Not used");
    case SQLITE_RANGE:
        /* i18n-hint: database operation has failed because the parameter is out of range */
        return XO("2nd parameter to sqlite3_bind out of range");
    case SQLITE_NOTADB:
        /* i18n-hint: database operation has failed because the file opened is not a database file */
        return XO("File opened that is not a database file ");
    default:
        /* i18n-hint: database operation has failed due to the unknown error */
        return XO("Unknown error");
    }
}
} // namespace audacity::sqlite
