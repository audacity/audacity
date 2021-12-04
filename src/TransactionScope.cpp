/*!********************************************************************

Audacity: A Digital Audio Editor

@file TransactionScope.cpp

Paul Licameli -- split from DBConnection.cpp

**********************************************************************/

#include "TransactionScope.h"

#include "DBConnection.h"

#include "sqlite3.h"
#include "SentryHelper.h"
#include <wx/log.h>

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
      ADD_EXCEPTION_CONTEXT("sqlite3.rc", std::to_string(rc));
      ADD_EXCEPTION_CONTEXT("sqlite3.context", "TransactionScope::TransactionStart");

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
      ADD_EXCEPTION_CONTEXT("sqlite3.rc", std::to_string(rc));
      ADD_EXCEPTION_CONTEXT("sqlite3.context", "TransactionScope::TransactionCommit");

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
      ADD_EXCEPTION_CONTEXT("sqlite3.rc", std::to_string(rc));
      ADD_EXCEPTION_CONTEXT("sqlite3.context", "TransactionScope::TransactionRollback");

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

   return !mInTrans;
}
