/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    QM DSP Library

    Centre for Digital Music, Queen Mary, University of London.
    This file copyright Chris Cannam, used with permission.
*/

#include "Thread.h"

#include <iostream>
#include <cstdlib>

#ifdef USE_PTHREADS
#include <sys/time.h>
#include <time.h>
#endif

using std::cerr;
using std::endl;
using std::string;

#ifdef _WIN32

Thread::Thread() :
    m_id(0),
    m_extant(false)
{
#ifdef DEBUG_THREAD
    cerr << "THREAD DEBUG: Created thread object " << this << endl;
#endif
}

Thread::~Thread()
{
#ifdef DEBUG_THREAD
    cerr << "THREAD DEBUG: Destroying thread object " << this << ", id " << m_id << endl;
#endif
    if (m_extant) {
        WaitForSingleObject(m_id, INFINITE);
    }
#ifdef DEBUG_THREAD
    cerr << "THREAD DEBUG: Destroyed thread object " << this << endl;
#endif
}

void
Thread::start()
{
    m_id = CreateThread(NULL, 0, staticRun, this, 0, 0);
    if (!m_id) {
        cerr << "ERROR: thread creation failed" << endl;
        exit(1);
    } else {
#ifdef DEBUG_THREAD
        cerr << "THREAD DEBUG: Created thread " << m_id << " for thread object " << this << endl;
#endif
        m_extant = true;
    }
}    

void 
Thread::wait()
{
    if (m_extant) {
#ifdef DEBUG_THREAD
        cerr << "THREAD DEBUG: Waiting on thread " << m_id << " for thread object " << this << endl;
#endif
        WaitForSingleObject(m_id, INFINITE);
#ifdef DEBUG_THREAD
        cerr << "THREAD DEBUG: Waited on thread " << m_id << " for thread object " << this << endl;
#endif
        m_extant = false;
    }
}

Thread::Id
Thread::id()
{
    return m_id;
}

bool
Thread::threadingAvailable()
{
    return true;
}

DWORD
Thread::staticRun(LPVOID arg)
{
    Thread *thread = static_cast<Thread *>(arg);
#ifdef DEBUG_THREAD
    cerr << "THREAD DEBUG: " << (void *)GetCurrentThreadId() << ": Running thread " << thread->m_id << " for thread object " << thread << endl;
#endif
    thread->run();
    return 0;
}

Mutex::Mutex()
#ifndef NO_THREAD_CHECKS
    :
    m_lockedBy(-1)
#endif
{
    m_mutex = CreateMutex(NULL, FALSE, NULL);
#ifdef DEBUG_MUTEX
    cerr << "MUTEX DEBUG: " << (void *)GetCurrentThreadId() << ": Initialised mutex " << &m_mutex << endl;
#endif
}

Mutex::~Mutex()
{
#ifdef DEBUG_MUTEX
    cerr << "MUTEX DEBUG: " << (void *)GetCurrentThreadId() << ": Destroying mutex " << &m_mutex << endl;
#endif
    CloseHandle(m_mutex);
}

void
Mutex::lock()
{
#ifndef NO_THREAD_CHECKS
    DWORD tid = GetCurrentThreadId();
    if (m_lockedBy == tid) {
        cerr << "ERROR: Deadlock on mutex " << &m_mutex << endl;
    }
#endif
#ifdef DEBUG_MUTEX
    cerr << "MUTEX DEBUG: " << (void *)tid << ": Want to lock mutex " << &m_mutex << endl;
#endif
    WaitForSingleObject(m_mutex, INFINITE);
#ifndef NO_THREAD_CHECKS
    m_lockedBy = tid;
#endif
#ifdef DEBUG_MUTEX
    cerr << "MUTEX DEBUG: " << (void *)tid << ": Locked mutex " << &m_mutex << endl;
#endif
}

void
Mutex::unlock()
{
#ifndef NO_THREAD_CHECKS
    DWORD tid = GetCurrentThreadId();
    if (m_lockedBy != tid) {
        cerr << "ERROR: Mutex " << &m_mutex << " not owned by unlocking thread" << endl;
        return;
    }
#endif
#ifdef DEBUG_MUTEX
    cerr << "MUTEX DEBUG: " << (void *)tid << ": Unlocking mutex " << &m_mutex << endl;
#endif
#ifndef NO_THREAD_CHECKS
    m_lockedBy = -1;
#endif
    ReleaseMutex(m_mutex);
}

bool
Mutex::trylock()
{
#ifndef NO_THREAD_CHECKS
    DWORD tid = GetCurrentThreadId();
#endif
    DWORD result = WaitForSingleObject(m_mutex, 0);
    if (result == WAIT_TIMEOUT || result == WAIT_FAILED) {
#ifdef DEBUG_MUTEX
        cerr << "MUTEX DEBUG: " << (void *)tid << ": Mutex " << &m_mutex << " unavailable" << endl;
#endif
        return false;
    } else {
#ifndef NO_THREAD_CHECKS
        m_lockedBy = tid;
#endif
#ifdef DEBUG_MUTEX
        cerr << "MUTEX DEBUG: " << (void *)tid << ": Locked mutex " << &m_mutex << " (from trylock)" << endl;
#endif
        return true;
    }
}

Condition::Condition(string name) :
    m_locked(false)
#ifdef DEBUG_CONDITION
    , m_name(name)
#endif
{
    m_mutex = CreateMutex(NULL, FALSE, NULL);
    m_condition = CreateEvent(NULL, FALSE, FALSE, NULL);
#ifdef DEBUG_CONDITION
    cerr << "CONDITION DEBUG: " << (void *)GetCurrentThreadId() << ": Initialised condition " << &m_condition << " \"" << m_name << "\"" << endl;
#endif
}

Condition::~Condition()
{
#ifdef DEBUG_CONDITION
    cerr << "CONDITION DEBUG: " << (void *)GetCurrentThreadId() << ": Destroying condition " << &m_condition << " \"" << m_name << "\"" << endl;
#endif
    if (m_locked) ReleaseMutex(m_mutex);
    CloseHandle(m_condition);
    CloseHandle(m_mutex);
}

void
Condition::lock()
{
#ifdef DEBUG_CONDITION
    cerr << "CONDITION DEBUG: " << (void *)GetCurrentThreadId() << ": Want to lock " << &m_condition << " \"" << m_name << "\"" << endl;
#endif
    WaitForSingleObject(m_mutex, INFINITE);
    m_locked = true;
#ifdef DEBUG_CONDITION
    cerr << "CONDITION DEBUG: " << (void *)GetCurrentThreadId() << ": Locked " << &m_condition << " \"" << m_name << "\"" << endl;
#endif
}

void
Condition::unlock()
{
    if (!m_locked) {
#ifdef DEBUG_CONDITION
        cerr << "CONDITION DEBUG: " << (void *)GetCurrentThreadId() << ": Not locked " << &m_condition << " \"" << m_name << "\"" << endl;
#endif
        return;
    }
#ifdef DEBUG_CONDITION
    cerr << "CONDITION DEBUG: " << (void *)GetCurrentThreadId() << ": Unlocking " << &m_condition << " \"" << m_name << "\"" << endl;
#endif
    m_locked = false;
    ReleaseMutex(m_mutex);
}

void 
Condition::wait(int us)
{
    if (us == 0) {

#ifdef DEBUG_CONDITION
        cerr << "CONDITION DEBUG: " << (void *)GetCurrentThreadId() << ": Waiting on " << &m_condition << " \"" << m_name << "\"" << endl;
#endif
        SignalObjectAndWait(m_mutex, m_condition, INFINITE, FALSE);
        WaitForSingleObject(m_mutex, INFINITE);

    } else {

        DWORD ms = us / 1000;
        if (us > 0 && ms == 0) ms = 1;
    
#ifdef DEBUG_CONDITION
        cerr << "CONDITION DEBUG: " << (void *)GetCurrentThreadId() << ": Timed waiting on " << &m_condition << " \"" << m_name << "\"" << endl;
#endif
        SignalObjectAndWait(m_mutex, m_condition, ms, FALSE);
        WaitForSingleObject(m_mutex, INFINITE);
    }

#ifdef DEBUG_CONDITION
    cerr << "CONDITION DEBUG: " << (void *)GetCurrentThreadId() << ": Wait done on " << &m_condition << " \"" << m_name << "\"" << endl;
#endif

    m_locked = true;
}

void
Condition::signal()
{
#ifdef DEBUG_CONDITION
    cerr << "CONDITION DEBUG: " << (void *)GetCurrentThreadId() << ": Signalling " << &m_condition << " \"" << m_name << "\"" << endl;
#endif
    SetEvent(m_condition);
}

#else /* !_WIN32 */

#ifdef USE_PTHREADS

Thread::Thread() :
    m_id(0),
    m_extant(false)
{
#ifdef DEBUG_THREAD
    cerr << "THREAD DEBUG: Created thread object " << this << endl;
#endif
}

Thread::~Thread()
{
#ifdef DEBUG_THREAD
    cerr << "THREAD DEBUG: Destroying thread object " << this << ", id " << m_id << endl;
#endif
    if (m_extant) {
        pthread_join(m_id, 0);
    }
#ifdef DEBUG_THREAD
    cerr << "THREAD DEBUG: Destroyed thread object " << this << endl;
#endif
}

void
Thread::start()
{
    if (pthread_create(&m_id, 0, staticRun, this)) {
        cerr << "ERROR: thread creation failed" << endl;
        exit(1);
    } else {
#ifdef DEBUG_THREAD
        cerr << "THREAD DEBUG: Created thread " << m_id << " for thread object " << this << endl;
#endif
        m_extant = true;
    }
}    

void 
Thread::wait()
{
    if (m_extant) {
#ifdef DEBUG_THREAD
        cerr << "THREAD DEBUG: Waiting on thread " << m_id << " for thread object " << this << endl;
#endif
        pthread_join(m_id, 0);
#ifdef DEBUG_THREAD
        cerr << "THREAD DEBUG: Waited on thread " << m_id << " for thread object " << this << endl;
#endif
        m_extant = false;
    }
}

Thread::Id
Thread::id()
{
    return m_id;
}

bool
Thread::threadingAvailable()
{
    return true;
}

void *
Thread::staticRun(void *arg)
{
    Thread *thread = static_cast<Thread *>(arg);
#ifdef DEBUG_THREAD
    cerr << "THREAD DEBUG: " << (void *)pthread_self() << ": Running thread " << thread->m_id << " for thread object " << thread << endl;
#endif
    thread->run();
    return 0;
}

Mutex::Mutex()
#ifndef NO_THREAD_CHECKS
    :
    m_lockedBy(0),
    m_locked(false)
#endif
{
    pthread_mutex_init(&m_mutex, 0);
#ifdef DEBUG_MUTEX
    cerr << "MUTEX DEBUG: " << (void *)pthread_self() << ": Initialised mutex " << &m_mutex << endl;
#endif
}

Mutex::~Mutex()
{
#ifdef DEBUG_MUTEX
    cerr << "MUTEX DEBUG: " << (void *)pthread_self() << ": Destroying mutex " << &m_mutex << endl;
#endif
    pthread_mutex_destroy(&m_mutex);
}

void
Mutex::lock()
{
#ifndef NO_THREAD_CHECKS
    pthread_t tid = pthread_self();
    if (m_locked && m_lockedBy == tid) {
        cerr << "ERROR: Deadlock on mutex " << &m_mutex << endl;
    }
#endif
#ifdef DEBUG_MUTEX
    cerr << "MUTEX DEBUG: " << (void *)tid << ": Want to lock mutex " << &m_mutex << endl;
#endif
    pthread_mutex_lock(&m_mutex);
#ifndef NO_THREAD_CHECKS
    m_lockedBy = tid;
    m_locked = true;
#endif
#ifdef DEBUG_MUTEX
    cerr << "MUTEX DEBUG: " << (void *)tid << ": Locked mutex " << &m_mutex << endl;
#endif
}

void
Mutex::unlock()
{
#ifndef NO_THREAD_CHECKS
    pthread_t tid = pthread_self();
    if (!m_locked) {
        cerr << "ERROR: Mutex " << &m_mutex << " not locked in unlock" << endl;
        return;
    } else if (m_lockedBy != tid) {
        cerr << "ERROR: Mutex " << &m_mutex << " not owned by unlocking thread" << endl;
        return;
    }
#endif
#ifdef DEBUG_MUTEX
    cerr << "MUTEX DEBUG: " << (void *)tid << ": Unlocking mutex " << &m_mutex << endl;
#endif
#ifndef NO_THREAD_CHECKS
    m_locked = false;
#endif
    pthread_mutex_unlock(&m_mutex);
}

bool
Mutex::trylock()
{
#ifndef NO_THREAD_CHECKS
    pthread_t tid = pthread_self();
#endif
    if (pthread_mutex_trylock(&m_mutex)) {
#ifdef DEBUG_MUTEX
        cerr << "MUTEX DEBUG: " << (void *)tid << ": Mutex " << &m_mutex << " unavailable" << endl;
#endif
        return false;
    } else {
#ifndef NO_THREAD_CHECKS
        m_lockedBy = tid;
        m_locked = true;
#endif
#ifdef DEBUG_MUTEX
        cerr << "MUTEX DEBUG: " << (void *)tid << ": Locked mutex " << &m_mutex << " (from trylock)" << endl;
#endif
        return true;
    }
}

Condition::Condition(string
#ifdef DEBUG_CONDITION
                     name
#endif
    ) :
    m_locked(false)
#ifdef DEBUG_CONDITION
    , m_name(name)
#endif
{
    pthread_mutex_init(&m_mutex, 0);
    pthread_cond_init(&m_condition, 0);
#ifdef DEBUG_CONDITION
    cerr << "CONDITION DEBUG: " << (void *)pthread_self() << ": Initialised condition " << &m_condition << " \"" << m_name << "\"" << endl;
#endif
}

Condition::~Condition()
{
#ifdef DEBUG_CONDITION
    cerr << "CONDITION DEBUG: " << (void *)pthread_self() << ": Destroying condition " << &m_condition << " \"" << m_name << "\"" << endl;
#endif
    if (m_locked) pthread_mutex_unlock(&m_mutex);
    pthread_cond_destroy(&m_condition);
    pthread_mutex_destroy(&m_mutex);
}

void
Condition::lock()
{
#ifdef DEBUG_CONDITION
    cerr << "CONDITION DEBUG: " << (void *)pthread_self() << ": Want to lock " << &m_condition << " \"" << m_name << "\"" << endl;
#endif
    pthread_mutex_lock(&m_mutex);
    m_locked = true;
#ifdef DEBUG_CONDITION
    cerr << "CONDITION DEBUG: " << (void *)pthread_self() << ": Locked " << &m_condition << " \"" << m_name << "\"" << endl;
#endif
}

void
Condition::unlock()
{
    if (!m_locked) {
#ifdef DEBUG_CONDITION
        cerr << "CONDITION DEBUG: " << (void *)pthread_self() << ": Not locked " << &m_condition << " \"" << m_name << "\"" << endl;
#endif
        return;
    }
#ifdef DEBUG_CONDITION
    cerr << "CONDITION DEBUG: " << (void *)pthread_self() << ": Unlocking " << &m_condition << " \"" << m_name << "\"" << endl;
#endif
    m_locked = false;
    pthread_mutex_unlock(&m_mutex);
}

void 
Condition::wait(int us)
{
    if (us == 0) {

#ifdef DEBUG_CONDITION
        cerr << "CONDITION DEBUG: " << (void *)pthread_self() << ": Waiting on " << &m_condition << " \"" << m_name << "\"" << endl;
#endif
        pthread_cond_wait(&m_condition, &m_mutex);

    } else {

        struct timeval now;
        gettimeofday(&now, 0);

        now.tv_usec += us;
        while (now.tv_usec > 1000000) {
            now.tv_usec -= 1000000;
            ++now.tv_sec;
        }

        struct timespec timeout;
        timeout.tv_sec = now.tv_sec;
        timeout.tv_nsec = now.tv_usec * 1000;
    
#ifdef DEBUG_CONDITION
        cerr << "CONDITION DEBUG: " << (void *)pthread_self() << ": Timed waiting on " << &m_condition << " \"" << m_name << "\"" << endl;
#endif
        pthread_cond_timedwait(&m_condition, &m_mutex, &timeout);
    }

#ifdef DEBUG_CONDITION
    cerr << "CONDITION DEBUG: " << (void *)pthread_self() << ": Wait done on " << &m_condition << " \"" << m_name << "\"" << endl;
#endif

    m_locked = true;
}

void
Condition::signal()
{
#ifdef DEBUG_CONDITION
    cerr << "CONDITION DEBUG: " << (void *)pthread_self() << ": Signalling " << &m_condition << " \"" << m_name << "\"" << endl;
#endif
    pthread_cond_signal(&m_condition);
}

#else /* !USE_PTHREADS */

Thread::Thread()
{
}

Thread::~Thread()
{
}

void
Thread::start()
{
    abort();
}    

void 
Thread::wait()
{
    abort();
}

Thread::Id
Thread::id()
{
    abort();
}

bool
Thread::threadingAvailable()
{
    return false;
}

Mutex::Mutex()
{
}

Mutex::~Mutex()
{
}

void
Mutex::lock()
{
    abort();
}

void
Mutex::unlock()
{
    abort();
}

bool
Mutex::trylock()
{
    abort();
}

Condition::Condition(const char *)
{
}

Condition::~Condition()
{
}

void
Condition::lock()
{
    abort();
}

void 
Condition::wait(int us)
{
    abort();
}

void
Condition::signal()
{
    abort();
}

#endif /* !USE_PTHREADS */
#endif /* !_WIN32 */

MutexLocker::MutexLocker(Mutex *mutex) :
    m_mutex(mutex)
{
    if (m_mutex) {
        m_mutex->lock();
    }
}

MutexLocker::~MutexLocker()
{
    if (m_mutex) {
        m_mutex->unlock();
    }
}

