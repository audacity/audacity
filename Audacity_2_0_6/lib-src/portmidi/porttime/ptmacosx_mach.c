/* ptmacosx.c -- portable timer implementation for mac os x */

#include <stdlib.h>
#include <stdio.h>
#include <CoreAudio/HostTime.h>

#import <mach/mach.h>
#import <mach/mach_error.h>
#import <mach/mach_time.h>
#import <mach/clock.h>
#include <unistd.h>

#include "porttime.h"
#include "sys/time.h"
#include "pthread.h"

#define NSEC_PER_MSEC 1000000
#define THREAD_IMPORTANCE 30

static int time_started_flag = FALSE;
static UInt64 start_time;
static pthread_t pt_thread_pid;

/* note that this is static data -- we only need one copy */
typedef struct {
    int id;
    int resolution;
    PtCallback *callback;
    void *userData;
} pt_callback_parameters;

static int pt_callback_proc_id = 0;

static void *Pt_CallbackProc(void *p)
{
    pt_callback_parameters *parameters = (pt_callback_parameters *) p;
    int mytime = 1;

    kern_return_t error;
    thread_extended_policy_data_t extendedPolicy;
    thread_precedence_policy_data_t precedencePolicy;

    extendedPolicy.timeshare = 0;
    error = thread_policy_set(mach_thread_self(), THREAD_EXTENDED_POLICY,
                              (thread_policy_t)&extendedPolicy,
                              THREAD_EXTENDED_POLICY_COUNT);
    if (error != KERN_SUCCESS) {
        mach_error("Couldn't set thread timeshare policy", error);
    }

    precedencePolicy.importance = THREAD_IMPORTANCE;
    error = thread_policy_set(mach_thread_self(), THREAD_PRECEDENCE_POLICY,
                              (thread_policy_t)&precedencePolicy,
                              THREAD_PRECEDENCE_POLICY_COUNT);
    if (error != KERN_SUCCESS) {
        mach_error("Couldn't set thread precedence policy", error);
    }
    
    
    /* to kill a process, just increment the pt_callback_proc_id */
    /* printf("pt_callback_proc_id %d, id %d\n", pt_callback_proc_id, parameters->id); */
    while (pt_callback_proc_id == parameters->id) {
        /* wait for a multiple of resolution ms */
        UInt64 wait_time;
        int delay = mytime++ * parameters->resolution - Pt_Time();
	PtTimestamp timestamp;
        if (delay < 0) delay = 0;
        wait_time = AudioConvertNanosToHostTime((UInt64)delay * NSEC_PER_MSEC);
        wait_time += AudioGetCurrentHostTime();
        error = mach_wait_until(wait_time);
	timestamp = Pt_Time();
        (*(parameters->callback))(timestamp, parameters->userData);
    }
    free(parameters);
    return NULL;
}


PtError Pt_Start(int resolution, PtCallback *callback, void *userData)
{
    if (time_started_flag) return ptAlreadyStarted;
    start_time = AudioGetCurrentHostTime();
    
    if (callback) {
        int res;
        pt_callback_parameters *parms;

        parms = (pt_callback_parameters *) malloc(sizeof(pt_callback_parameters));
        if (!parms) return ptInsufficientMemory;
        parms->id = pt_callback_proc_id;
        parms->resolution = resolution;
        parms->callback = callback;
        parms->userData = userData;
        res = pthread_create(&pt_thread_pid, NULL, Pt_CallbackProc, parms);
        if (res != 0) return ptHostError;
    }
    
    time_started_flag = TRUE;
    return ptNoError;
}


PtError Pt_Stop()
{
    /* printf("Pt_Stop called\n"); */
    pt_callback_proc_id++;
    pthread_join(pt_thread_pid, NULL);
    time_started_flag = FALSE;
    return ptNoError;
}


int Pt_Started()
{
    return time_started_flag;
}


PtTimestamp Pt_Time()
{
    UInt64 clock_time, nsec_time;
    clock_time = AudioGetCurrentHostTime() - start_time;
    nsec_time = AudioConvertHostTimeToNanos(clock_time);
    return (PtTimestamp)(nsec_time / NSEC_PER_MSEC);
}


void Pt_Sleep(int32_t duration)
{
    usleep(duration * 1000);
}
