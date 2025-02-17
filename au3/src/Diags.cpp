/**********************************************************************

  Audacity: A Digital Audio Editor

  Diags.cpp

  James Crook


********************************************************************//**

\class Diags
\brief Processing of the macros for recording bad events or performance
monitoring.

The idea of these macros is that we can include them in release
code at no risk.  They

a) have almost zero performance impact.
b) will not flood the log with events.

This is achieved by a countdown which stops recording in the log
when the countdown is finished.  The countdwon continues to
count down so that we track how many times the event happens.


*//********************************************************************/

#include "Diags.h"

#include <wx/defs.h>
#include <wx/hash.h>
#include <wx/log.h>

#include <wx/stopwatch.h>

static wxStopWatch MasterWatch;
static bool bStopWatchStarted = false;

void diagnostics_do_diag(t_diag_struct* pDiag)
{
    wxLog* pLog = wxLog::SetActiveTarget(NULL);
    // this macro is empty if wxWidgets is not compiled in debug mode
    wxLogDebug(wxT("%s"), pDiag->pMessage);
    wxLog::SetActiveTarget(pLog);
}

void diagnostics_do_diag_mem(t_diag_struct* pDiag, long amount)
{
    wxLog* pLog = wxLog::SetActiveTarget(NULL);
    wxLogDebug(wxT("%s %l"), pDiag->pMessage, amount);
    wxLog::SetActiveTarget(pLog);
    pDiag->total += amount;
    pDiag->most_recent = amount;
    if (pDiag->countdown == (pDiag->initial_count - 1)) {
        pDiag->most = amount;
        pDiag->least = amount;
    } else if (amount > pDiag->most) {
        pDiag->most = amount;
    } else if (amount < pDiag->least) {
        pDiag->least = amount;
    }
}

void diagnostics_do_perfmon_start(t_diag_struct* pDiag, t_diag_struct** pRememberMe)
{
    if (*pRememberMe == NULL) {
        *pRememberMe = pDiag;
        if (!bStopWatchStarted) {
            bStopWatchStarted = true;
            MasterWatch.Start();
        }
    }
    pDiag->most_recent = MasterWatch.Time();
}

void diagnostics_do_perfmon_stop(t_diag_struct** ppDiag)
{
    t_diag_struct* pDiag = *ppDiag;
    *ppDiag = NULL;
    long amount = MasterWatch.Time() - pDiag->most_recent;
    pDiag->total += amount;
    pDiag->most_recent = amount;
    if (pDiag->countdown == (pDiag->initial_count - 1)) {
        pDiag->most = amount;
        pDiag->least = amount;
    } else if (amount > pDiag->most) {
        pDiag->most = amount;
    } else if (amount < pDiag->least) {
        pDiag->least = amount;
    }
    wxLog* pLog = wxLog::SetActiveTarget(NULL);
    wxLogDebug(wxT("%s %f seconds"), pDiag->pMessage, ((float)amount) / 1000.0f);
    wxLog::SetActiveTarget(pLog);
}

void diag_sample_test()
{
    DIAG("Flip counter");// Flip counter will show in log ten times, then just count.
}
