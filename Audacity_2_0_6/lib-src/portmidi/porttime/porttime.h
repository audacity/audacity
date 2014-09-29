/* porttime.h -- portable interface to millisecond timer */

/* CHANGE LOG FOR PORTTIME
  10-Jun-03 Mark Nelson & RBD
    boost priority of timer thread in ptlinux.c implementation
 */

/* Should there be a way to choose the source of time here? */

#ifdef WIN32
#ifndef INT32_DEFINED
// rather than having users install a special .h file for windows, 
// just put the required definitions inline here. portmidi.h uses
// these too, so the definitions are (unfortunately) duplicated there
typedef int int32_t;
typedef unsigned int uint32_t;
#define INT32_DEFINED
#endif
#else
#include <stdint.h> // needed for int32_t
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifndef PMEXPORT
#ifdef _WINDLL
#define PMEXPORT __declspec(dllexport)
#else
#define PMEXPORT 
#endif
#endif

typedef enum {
    ptNoError = 0,         /* success */
    ptHostError = -10000,  /* a system-specific error occurred */
    ptAlreadyStarted,      /* cannot start timer because it is already started */
    ptAlreadyStopped,      /* cannot stop timer because it is already stopped */
    ptInsufficientMemory   /* memory could not be allocated */
} PtError;


typedef int32_t PtTimestamp;

typedef void (PtCallback)( PtTimestamp timestamp, void *userData );

/*
    Pt_Start() starts a real-time service.

    resolution is the timer resolution in ms. The time will advance every
    resolution ms.

    callback is a function pointer to be called every resolution ms.

    userData is passed to callback as a parameter.

    return value:
    Upon success, returns ptNoError. See PtError for other values.
*/
PMEXPORT PtError Pt_Start(int resolution, PtCallback *callback, void *userData);

/*
    Pt_Stop() stops the timer.

    return value:
    Upon success, returns ptNoError. See PtError for other values.
*/
PMEXPORT PtError Pt_Stop();

/*
    Pt_Started() returns true iff the timer is running.
*/
PMEXPORT int Pt_Started();

/* 
    Pt_Time() returns the current time in ms.
*/
PMEXPORT PtTimestamp Pt_Time();

/*
    Pt_Sleep() pauses, allowing other threads to run.

    duration is the length of the pause in ms. The true duration 
    of the pause may be rounded to the nearest or next clock tick
    as determined by resolution in Pt_Start().
*/
PMEXPORT void Pt_Sleep(int32_t duration);

#ifdef __cplusplus
}
#endif
