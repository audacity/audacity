/**********************************************************************

  Audacity: A Digital Audio Editor

  ODTaskThread.cpp

  Created by Michael Chinen (mchinen) on 6/8/08
  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

******************************************************************//**

\class ODTaskThread
\brief A thread that executes a part of the task specified by an ODTask.

*//*******************************************************************/


#include "ODTaskThread.h"


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

