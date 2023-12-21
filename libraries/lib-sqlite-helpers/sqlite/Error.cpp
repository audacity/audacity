/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: Error.cpp
 * SPDX-FileContributor: Dmitry Vedenko
 */
#include "Error.h"

#include <cassert>

#include "AudacityException.h"
#include "sqlite3.h"

namespace sqlite
{
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
   switch (mCode)
   {
   case SQLITE_OK:
      return XO("No error");
   case SQLITE_ERROR:
      return XO("Generic error");
   case SQLITE_INTERNAL:
      return XO("Internal logic error in SQLite");
   case SQLITE_PERM:
      return XO("Access permission denied");
   case SQLITE_ABORT:
      return XO("Callback routine requested an abort");
   case SQLITE_BUSY:
      return XO("The database file is locked");
   case SQLITE_LOCKED:
      return XO("A table in the database is locked");
   case SQLITE_NOMEM:
      return XO("A malloc() failed");
   case SQLITE_READONLY:
      return XO("Attempt to write a read-only database");
   case SQLITE_INTERRUPT:
      return XO("Operation terminated");
   case SQLITE_IOERR:
      return XO("Some kind of disk I/O error occurred");
   case SQLITE_CORRUPT:
      return XO("The database disk image is malformed");
   case SQLITE_NOTFOUND:
      return XO("Unknown opcode returned by a file system");
   case SQLITE_FULL:
      return XO("Insertion failed because database is full");
   case SQLITE_CANTOPEN:
      return XO("Unable to open the database file");
   case SQLITE_PROTOCOL:
      return XO("Database lock protocol error");
   case SQLITE_SCHEMA:
      return XO("The database schema changed");
   case SQLITE_TOOBIG:
      return XO("String or BLOB exceeds size limit");
   case SQLITE_CONSTRAINT:
      return XO("Abort due to constraint violation");
   case SQLITE_MISMATCH:
      return XO("Data type mismatch");
   case SQLITE_MISUSE:
      return XO("Library used incorrectly");
   case SQLITE_NOLFS:
      return XO("Uses OS features not supported on host");
   case SQLITE_AUTH:
      return XO("Authorization denied");
   case SQLITE_FORMAT:
      return XO("Not used");
   case SQLITE_RANGE:
      return XO("2nd parameter to sqlite3_bind out of range");
   case SQLITE_NOTADB:
      return XO("File opened that is not a database file ");
   default:
      return XO("Unknown error");
   }
}
} // namespace sqlite
