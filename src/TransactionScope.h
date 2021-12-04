/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file TransactionScope.h

  Paul Licameli split from DBConnection.h

**********************************************************************/

#ifndef __AUDACITY_TRANSACTION_SCOPE__
#define __AUDACITY_TRANSACTION_SCOPE__

#include <wx/string.h>
class DBConnection;

//! RAII for a database transaction, possibly nested
/*! Make a savepoint (a transaction, possibly nested) with the given name;
    roll it back at destruction time, unless an explicit Commit() happened first.
    Commit() must not be called again after one successful call.
    An exception is thrown from the constructor if the transaction cannot open.
 */
class AUDACITY_DLL_API TransactionScope
{
public:
   TransactionScope(DBConnection &connection, const char *name);
   ~TransactionScope();

   bool Commit();

private:
   bool TransactionStart(const wxString &name);
   bool TransactionCommit(const wxString &name);
   bool TransactionRollback(const wxString &name);

   DBConnection &mConnection;
   bool mInTrans;
   wxString mName;
};

#endif
