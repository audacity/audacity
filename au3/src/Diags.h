/**********************************************************************

  Audacity: A Digital Audio Editor

  Diags.h

  James Crook

  Provides Macros for recording bad events and performance monitoring.
  These macros have such low cost that they can be used in release code.
  They will take minuscule processing time after the first ten times.

**********************************************************************/

#ifndef __AUDACITY_DIAGS__
#define __AUDACITY_DIAGS__

typedef long t_diag_timer;

struct t_diag_struct {
    long countdown;
    long initial_count;
    long total;
    long most_recent;
    long least;
    long most;
    const wchar_t* pMessage;
};

extern void diagnostics_do_diag(t_diag_struct* pDiag);
extern void diagnostics_do_diag_mem(t_diag_struct* pDiag, long amount);
extern void diagnostics_do_perfmon_start(t_diag_struct* pDiag, t_diag_struct** ppRememberMe);
extern void diagnostics_do_perfmon_stop(t_diag_struct** ppDiag);

// A constant that sets the maximum number of times we log the message.
#define DEFAULT_LOG_COUNT (10)

// USAGE:
// Each of these will do something the first ten times, then just count.
// They can be reactivated by a GUI.
//
// Use DIAG for a simple message. Usually for something bad like an overrun.
// Use TRACK_MEM to track hungry memory usage, RAM or disk.
// Use TIMER_START and STOP to time an interval.
// For the above two, you will need a MAKE_TIMER( timername ) first.

// The 'timername' created here is almost free.
// It's a pointer that allows both START and STOP to use the same struct.
#define MAKE_TIMER(timername) \
    static t_diag_struct* timername = NULL;

// Note that in all three macros:
// The {} ensure diag name is not visible outside
// static ensures struct is initialised just once.
// No function is called after the countdown is counted out.
#define DIAG(message) { \
        static t_diag_struct diag = { DEFAULT_LOG_COUNT, DEFAULT_LOG_COUNT, 0, 0, 0, 0, wxT(message) }; \
        if (--diag.countdown >= 0) \
        diagnostics_do_diag(&diag); \
}

#define TRACK_MEM(message, amount) { \
        static t_diag_struct diag = { DEFAULT_LOG_COUNT, DEFAULT_LOG_COUNT, 0, 0, 0, 0, wxT(message) }; \
        if (--diag.countdown >= 0) \
        diagnostics_do_diag_mem(&diag, amount); \
}

#define TIMER_START(message, timername) \
    MAKE_TIMER(timername); { \
        static t_diag_struct diag = { DEFAULT_LOG_COUNT, DEFAULT_LOG_COUNT, 0, 0, 0, 0, wxT(message) }; \
        if (--diag.countdown >= 0) \
        diagnostics_do_perfmon_start(&diag, &timername); \
    }

#define TIMER_STOP(timername){ \
        if (timername != NULL) \
        diagnostics_do_perfmon_stop(&timername); \
}

#endif
