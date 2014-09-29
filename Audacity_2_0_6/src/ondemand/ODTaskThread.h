/**********************************************************************

  Audacity: A Digital Audio Editor

  ODTaskThread.h

  Created by Michael Chinen (mchinen) on 6/8/08
  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

******************************************************************//**

\class ODTaskThread
\brief A thread that executes a part of the task specfied by an ODTask.

*//*******************************************************************/





#ifndef __AUDACITY_ODTASKTHREAD__
#define __AUDACITY_ODTASKTHREAD__
#include <wx/thread.h>

#include "../Audacity.h"	// contains the set-up of AUDACITY_DLL_API

class ODTask;

#ifdef __WXMAC__

// On Mac OS X, it's better not to use the wxThread class.
// We use our own implementation based on pthreads instead.

#include <pthread.h>
#include <time.h>

class ODTaskThread {
 public:
   typedef int ExitCode;
   ODTaskThread(ODTask* task);
   /*ExitCode*/ void Entry();
   void Create() {}
   void Delete() {
      mDestroy = true;
      pthread_join(mThread, NULL);
   }
   bool TestDestroy() { return mDestroy; }
   void Sleep(int ms) {
      struct timespec spec;
      spec.tv_sec = 0;
      spec.tv_nsec = ms * 1000 * 1000;
      nanosleep(&spec, NULL);
   }
   static void *callback(void *p) {
      ODTaskThread *th = (ODTaskThread *)p;
#if defined(__WXMAC__)
      /*return (void *)*/ th->Entry();
      return NULL;
#else
      return (void *) th->Entry();
#endif
   }
   void Run() {
      pthread_create(&mThread, NULL, callback, this);
   }

   ///Specifies the priority the thread will run at.  Currently doesn't work.
   ///@param priority value from 0 (min priority) to 100 (max priority)
   void SetPriority(int priority)
   {
      mPriority=priority;
   }

 private:
   int mPriority;
   bool mDestroy;
   pthread_t mThread;

   ODTask* mTask;
};

class ODLock {
 public:
   ODLock(){
      mutex = (pthread_mutex_t *) malloc (sizeof (pthread_mutex_t));
      pthread_mutex_init (mutex, NULL);
   }

   void Lock()
   {
      pthread_mutex_lock (mutex);
   }

   void Unlock()
   {
      pthread_mutex_unlock (mutex);
   }

   virtual ~ODLock()
   {
      pthread_mutex_destroy (mutex);
      free(mutex);
   }

private:
   friend class ODCondition; //needs friendship for wait()
   pthread_mutex_t *mutex ;
};

class ODCondition
{
public:
   ODCondition(ODLock *lock);
   virtual ~ODCondition();
   void Signal();
   void Broadcast();
   void Wait();

protected:
   pthread_cond_t *condition;
   ODLock* m_lock;
};

#else


class ODTaskThread : public wxThread
{
public:
   ///Constructs a ODTaskThread
   ///@param task the task to be launched as an
   ODTaskThread(ODTask* task);


protected:
   ///Executes a part of the task
   virtual void* Entry();
   ODTask* mTask;

};


//a wrapper for wxMutex.
class AUDACITY_DLL_API ODLock : public wxMutex
{
public:
   ///Constructs a ODTaskThread
   ///@param task the task to be launched as an
   ODLock(){}
  virtual ~ODLock(){}
};

class ODCondition : public wxCondition
{
public:
   ODCondition(ODLock *lock):wxCondition(*lock){}
   virtual ~ODCondition(){}
   //these calls are implemented in wxCondition:
   //void Signal();
   //void Broadcast();
   //void Wait();

protected:
};

#endif // __WXMAC__

#endif //__AUDACITY_ODTASKTHREAD__

