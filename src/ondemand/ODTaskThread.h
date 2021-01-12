/**********************************************************************

  Audacity: A Digital Audio Editor

  ODTaskThread.h

  Created by Michael Chinen (mchinen) on 6/8/08
  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

******************************************************************//**

\class ODTaskThread
\brief A thread that executes a part of the task specified by an ODTask.

*//*******************************************************************/





#ifndef __AUDACITY_ODTASKTHREAD__
#define __AUDACITY_ODTASKTHREAD__

#include "../Audacity.h"	// contains the set-up of AUDACITY_DLL_API

#include <wx/thread.h> // to inherit

#include "../MemoryX.h"

class ODTask;

#ifdef __WXMAC__

#include <pthread.h>
#include <time.h>

class ODLock {
 public:
   ODLock(){
      pthread_mutex_init (&mutex, NULL);
   }

   void Lock()
   {
      pthread_mutex_lock (&mutex);
   }

   // Returns 0 iff the lock was acquired.
   bool TryLock()
   {
      return pthread_mutex_trylock (&mutex);
   }

   void Unlock()
   {
      pthread_mutex_unlock (&mutex);
   }

   virtual ~ODLock()
   {
      pthread_mutex_destroy (&mutex);
   }

private:
   friend class ODCondition; //needs friendship for wait()
   pthread_mutex_t mutex;
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
   pthread_cond_t condition;
   ODLock* m_lock;
};

#else


//a wrapper for wxMutex.
class AUDACITY_DLL_API ODLock final : public wxMutex
{
public:
   ///Constructs a ODTaskThread
   ///@param task the task to be launched as an
   ODLock(){}
  virtual ~ODLock(){}
};

class ODCondition final : public wxCondition
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

// class ODLocker
// So you can use the RAII idiom with ODLock, on whatever platform
// Construct with pointer to the lock, or default-construct and later
// reset()
// If constructed with only a try-lock, and the lock was not acquired,
// then it returns false when cast to bool
struct ODUnlocker { void operator () (ODLock *p) const { if(p) p->Unlock(); } };
using ODLockerBase = std::unique_ptr<ODLock, ODUnlocker>;
class ODLocker : public ODLockerBase {
public:
   // Lock any bare pointer to ODLock at construction time or when resetting.
   explicit ODLocker(ODLock *p = nullptr, bool tryOnly = false)
   {
      reset(p, tryOnly);
   }

   void reset(ODLock *p = nullptr, bool tryOnly = false)
   {
      ODLockerBase::reset(p);
      if(p) {
         if (tryOnly) {
            if (p->TryLock() != 0)
               ODLockerBase::reset(nullptr);
         }
         else
            p->Lock();
      }
   }

   // Assume already locked when moving ODLocker.  Don't lock again.
   ODLocker(ODLocker&& that) : ODLockerBase { std::move(that) } {}
   ODLocker &operator= (ODLocker && that) {
      ODLockerBase::operator= ( std::move(that) );
      return *this;
   }

   ODLocker(const ODLocker &that) PROHIBITED;
   ODLocker &operator= (const ODLocker &that) PROHIBITED;
};

#endif //__AUDACITY_ODTASKTHREAD__

