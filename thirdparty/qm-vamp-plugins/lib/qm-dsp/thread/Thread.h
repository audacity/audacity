/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    QM DSP Library

    Centre for Digital Music, Queen Mary, University of London.
    This file copyright Chris Cannam, used with permission.
*/

#ifndef QM_DSP_THREAD_H
#define QM_DSP_THREAD_H

#ifdef _WIN32
#include <windows.h>
#else /* !_WIN32 */
#ifdef USE_PTHREADS
#include <pthread.h>
#else
#error Must have either _WIN32 or USE_PTHREADS defined
#endif /* USE_PTHREADS */
#endif /* !_WIN32 */

#include <string>

//#define DEBUG_THREAD 1
//#define DEBUG_MUTEX 1
//#define DEBUG_CONDITION 1

class Thread
{
public:
#ifdef _WIN32
    typedef HANDLE Id;
#else
#ifdef USE_PTHREADS
    typedef pthread_t Id;
#endif
#endif

    Thread();
    virtual ~Thread();

    Id id();

    void start();
    void wait();

    static bool threadingAvailable();

protected:
    virtual void run() = 0;

private:
#ifdef _WIN32
    HANDLE m_id;
    bool m_extant;
    static DWORD WINAPI staticRun(LPVOID lpParam);
#else
#ifdef USE_PTHREADS
    pthread_t m_id;
    bool m_extant;
    static void *staticRun(void *);
#endif
#endif
};

class Mutex
{
public:
    Mutex();
    ~Mutex();

    void lock();
    void unlock();
    bool trylock();

private:
#ifdef _WIN32
    HANDLE m_mutex;
#ifndef NO_THREAD_CHECKS
    DWORD m_lockedBy;
#endif
#else
#ifdef USE_PTHREADS
    pthread_mutex_t m_mutex;
#ifndef NO_THREAD_CHECKS
    pthread_t m_lockedBy;
    bool m_locked;
#endif
#endif
#endif
};

class MutexLocker
{
public:
    MutexLocker(Mutex *);
    ~MutexLocker();

private:
    Mutex *m_mutex;
};

class Condition
{
public:
    Condition(std::string name);
    ~Condition();

    // Condition bundles a pthread-style condition variable and mutex
    // into one class.

    // To wait on a condition, call lock(), test termination variables
    // as appropriate, and then wait().  The condition will be
    // unlocked for the duration of the wait() call, which will end
    // when the condition is signalled.  The condition will be locked
    // again when wait() returns.
    //
    // To signal a condition, call signal().  If the waiting thread
    // will be performing tests between its own lock() and wait(),
    // then the signalling thread should also lock() before it signals
    // (and then unlock afterwards).  If the signalling thread always
    // locks the mutex during signalling, then the waiting thread
    // knows that signals will only happen during wait() and not be
    // missed at other times.

    void lock();
    void unlock();
    void wait(int us = 0);

    void signal();
    
private:

#ifdef _WIN32
    HANDLE m_mutex;
    HANDLE m_condition;
    bool m_locked;
#else
#ifdef USE_PTHREADS
    pthread_mutex_t m_mutex;
    pthread_cond_t m_condition;
    bool m_locked;
#endif
#endif
#ifdef DEBUG_CONDITION
    std::string m_name;
#endif
};

#endif
