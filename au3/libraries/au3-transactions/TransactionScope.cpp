/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file TransactionScope.cpp

  Paul Licameli split from DBConnection.cpp

**********************************************************************/

#include "TransactionScope.h"
#include "InconsistencyException.h"
#include <wx/log.h>

TransactionScopeImpl::~TransactionScopeImpl() = default;

TransactionScope::TransactionScope(
    AudacityProject& project, const char* name)
    :  mName(name)
{
    mpImpl = Factory::Call(project);
    if (!mpImpl) {
        return;
    }

    mInTrans = mpImpl->TransactionStart(mName);
    if (!mInTrans) {
        // To do, improve the message
        throw SimpleMessageBoxException(ExceptionType::Internal,
                                        XO("Database error.  Sorry, but we don't have more details."),
                                        XO("Warning"),
                                        "Error:_Disk_full_or_not_writable"
                                        );
    }
}

TransactionScope::~TransactionScope()
{
    if (mpImpl && mInTrans) {
        if (!mpImpl->TransactionRollback(mName)) {
            // Do not throw from a destructor!
            // This has to be a no-fail cleanup that does the best that it can.
            wxLogMessage("Transaction active at scope destruction");
        }
    }
}

bool TransactionScope::Commit()
{
    if (!mpImpl) {
        return false;
    } else if (!mInTrans) {
        wxLogMessage("No active transaction to commit");
        // Misuse of this class
        THROW_INCONSISTENCY_EXCEPTION;
    }

    // If commit is unsuccessful, consider us still in a commit, for the dtor
    // later
    mInTrans = !mpImpl->TransactionCommit(mName);

    return !mInTrans;
}
