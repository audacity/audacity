/**********************************************************************

  Audacity: A Digital Audio Editor

  ODTaskThread.cpp

  Created by Michael Chinen (mchinen) on 6/8/08
  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

******************************************************************//**

\class ODTaskThread
\brief A thread that executes a part of the task specfied by an ODTask.

*//*******************************************************************/


#include "ODTaskThread.h"
#include "ODTask.h"
#include "ODManager.h"


ODTaskThread::ODTaskThread(ODTask* task)
#ifndef __WXMAC__
: wxThread()
#endif
{
   mTask=task;
#ifdef __WXMAC__
   mDestroy = false;
   mThread = NULL;
#endif

}

#ifdef __WXMAC__

void ODTaskThread::Entry()
#else
void *ODTaskThread::Entry()

#endif
{
   //TODO: Figure out why this has no effect at all.
   //wxThread::This()->SetPriority( 40);
   //Do at least 5 percent of the task
   mTask->DoSome(0.05f);

   //release the thread count so that the ODManager knows how many active threads are alive.
   ODManager::Instance()->DecrementCurrentThreads();


#ifndef __WXMAC__
   return NULL;
#endif
}

#ifdef __WXMAC__
ODCondition::ODCondition(ODLock *lock)
{
   pthread_cond_init(&condition, NULL);
   m_lock=lock;
}
ODCondition::~ODCondition()
{
   pthread_cond_destroy (&condition);
}

void ODCondition::Signal()
{
   pthread_cond_signal(&condition);
}

void ODCondition::Broadcast()
{
   pthread_cond_broadcast(&condition);
}
void ODCondition::Wait()
{
   pthread_cond_wait(&condition, &m_lock->mutex);
}

#endif

