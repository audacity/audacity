/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file TransactionScope.h

  Paul Licameli split from DBConnection.h

**********************************************************************/
#ifndef __AUDACITY_TRANSACTION_SCOPE__
#define __AUDACITY_TRANSACTION_SCOPE__

#include "GlobalVariable.h"
class AudacityProject;
#include <functional>
#include <memory>
#include <wx/string.h>

class TransactionScopeImpl;

//! RAII for a database transaction, possibly nested
/*! Make a savepoint (a transaction, possibly nested) with the given name;
    roll it back at destruction time, unless an explicit Commit() happened first.
    Commit() must not be called again after one successful call.
    An exception is thrown from the constructor if the transaction cannot open.
 */
class TRANSACTIONS_API TransactionScope
{
public:
    //! Type of function supplying implementation of steps
    struct TRANSACTIONS_API Factory : GlobalHook<Factory,
                                                 std::unique_ptr<TransactionScopeImpl>(AudacityProject&)
                                                 > {};

    //! Construct from a project
    /*!
     If no implementation factory is installed, or the factory returns null,
     then this object does nothing */
    TransactionScope(AudacityProject& project, const char* name);

    //! Rollback transaction if it was not yet committed
    ~TransactionScope();

    //! Commit the transaction
    bool Commit();

private:
    std::unique_ptr<TransactionScopeImpl> mpImpl;
    bool mInTrans;
    wxString mName;
};

//! Abstract base class for implementation of steps of TransactionScope
class TRANSACTIONS_API TransactionScopeImpl
{
public:
    virtual ~TransactionScopeImpl();
    //! @return success; if false, TransactionScope ctor throws
    virtual bool TransactionStart(const wxString& name) = 0;
    //! @return success
    virtual bool TransactionCommit(const wxString& name) = 0;
    //! @return success
    virtual bool TransactionRollback(const wxString& name) = 0;
};

#endif
