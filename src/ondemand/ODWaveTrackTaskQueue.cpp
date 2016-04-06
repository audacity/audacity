/**********************************************************************

  Audacity: A Digital Audio Editor

  ODWaveTrackTaskQueue.h

  Created by Michael Chinen (mchinen) on 6/11/08
  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

******************************************************************//**

\class ODWaveTrackTaskQueue
\brief watches over all to be done (not yet started and started but not finished)
tasks associated with a WaveTrack.

*//*******************************************************************/

#include "../Audacity.h"
#include "ODWaveTrackTaskQueue.h"
#include "ODTask.h"
#include "ODManager.h"
/// Constructs an ODWaveTrackTaskQueue
ODWaveTrackTaskQueue::ODWaveTrackTaskQueue()
{
}

ODWaveTrackTaskQueue::~ODWaveTrackTaskQueue()
{
   //we need to DELETE all ODTasks.  We will have to block or wait until block for the active ones.
   for(unsigned int i=0;i<mTasks.size();i++)
   {
      mTasks[i]->TerminateAndBlock();//blocks if active.
      //small chance we may have rea-added the task back into the queue from a diff thread.  - so remove it if we have.
      ODManager::Instance()->RemoveTaskIfInQueue(mTasks[i].get());
      mTasks[i].reset();
   }

}

///returns whether or not this queue's task list and another's can merge together, as when we make two mono tracks stereo.
bool ODWaveTrackTaskQueue::CanMergeWith(ODWaveTrackTaskQueue* otherQueue)
{
   //have to be very careful when dealing with two lists that need to be locked.
   if(GetNumTasks()!=otherQueue->GetNumTasks())
      return false;

   mTasksMutex.Lock();
   for(unsigned int i=0;i<mTasks.size();i++)
   {
      if(!mTasks[i]->CanMergeWith(otherQueue->GetTask(i)))
      {
         mTasksMutex.Unlock();
         return false;
      }
   }
   mTasksMutex.Unlock();
   return true;
}

///add track to the masterTrack's queue - this will allow future ODScheduling to affect them together.
/// sets the NeedODUpdateFlag since we don't want the head task to finish without haven't dealt with the depednent
///
///@param track the track to bring into the tasks AND tracklist for this queue
void ODWaveTrackTaskQueue::MergeWaveTrack(WaveTrack* track)
{
   AddWaveTrack(track);
   mTasksMutex.Lock();
   for(unsigned int i=0;i<mTasks.size();i++)
   {
      mTasks[i]->AddWaveTrack(track);
      mTasks[i]->SetNeedsODUpdate();
   }
   mTasksMutex.Unlock();

}

///returns true if the argument is in the WaveTrack list.
bool ODWaveTrackTaskQueue::ContainsWaveTrack(WaveTrack* track)
{
   mTracksMutex.Lock();
   for(unsigned int i=0;i<mTracks.size();i++)
   {
      if(mTracks[i]==track)
      {
         mTracksMutex.Unlock();
         return true;
      }
   }
   mTracksMutex.Unlock();
   return false;
}
///Adds a track to the associated list.
void ODWaveTrackTaskQueue::AddWaveTrack(WaveTrack* track)
{

   mTracksMutex.Lock();
   if(track)
      mTracks.push_back(track);

   mTracksMutex.Unlock();
}

void ODWaveTrackTaskQueue::AddTask(movable_ptr<ODTask> &&mtask)
{
   ODTask *task = mtask.get();

   mTasksMutex.Lock();
   mTasks.push_back(std::move(mtask));
   mTasksMutex.Unlock();

   //take all of the tracks in the task.
   mTracksMutex.Lock();
   for(int i=0;i<task->GetNumWaveTracks();i++)
   {
      //task->GetWaveTrack(i) may return NULL, but we handle it by checking before using.
      //The other worry that the WaveTrack returned and was deleted in the meantime is also
      //handled since mQueuesMutex is locked one up in the stack from here,
      //and WaveTrack deletion is bound to that.
      mTracks.push_back(task->GetWaveTrack(i));
   }

   mTracksMutex.Unlock();

}

///Removes a track from the list.  Also notifies mTasks to stop using references
///to the instance in a thread-safe manner (may block)
void ODWaveTrackTaskQueue::RemoveWaveTrack(WaveTrack* track)
{
   if(track)
   {

      mTasksMutex.Lock();
      for(unsigned int i=0;i<mTasks.size();i++)
         mTasks[i]->StopUsingWaveTrack(track);
      mTasksMutex.Unlock();

      mTracksMutex.Lock();
      for(unsigned int i=0;i<mTracks.size();i++)
         if(mTracks[i]==track)
            mTracks.erase(mTracks.begin()+i--);//decrement i after the removal.

      mTracksMutex.Unlock();
   }
}

//if the wavetrack is in this queue, and is not the only wavetrack, clones the tasks and schedules it.
void ODWaveTrackTaskQueue::MakeWaveTrackIndependent(WaveTrack* track)
{

   mTracksMutex.Lock();
   if(mTracks.size()<2)
   {
      //if there is only one track, it is already independent.
      mTracksMutex.Unlock();
      return;
   }

   for(unsigned int i=0;i<mTracks.size();i++)
   {
      if(mTracks[i]==track)
      {
         mTracksMutex.Unlock();//release the lock, since RemoveWaveTrack is a public threadsafe method.
         RemoveWaveTrack(mTracks[i]);

         //clone the items in order and add them to the ODManager.
         mTasksMutex.Lock();
         for(unsigned int j=0;j<mTasks.size();j++)
         {
            auto task = mTasks[j]->Clone();
            task->AddWaveTrack(track);
            //AddNewTask requires us to relinquish this lock. However, it is safe because ODManager::MakeWaveTrackIndependent
            //has already locked the m_queuesMutex.
            mTasksMutex.Unlock();
            //AddNewTask locks the m_queuesMutex which is already locked by ODManager::MakeWaveTrackIndependent,
            //so we pass a boolean flag telling it not to lock again.
            ODManager::Instance()->AddNewTask(std::move(task), false);
            mTasksMutex.Lock();
         }
         mTasksMutex.Unlock();
         mTracksMutex.Lock();
         break;
      }
   }
   mTracksMutex.Unlock();
}

///changes the tasks associated with this Waveform to process the task from a different point in the track
///@param track the track to update
///@param seconds the point in the track from which the tasks associated with track should begin processing from.
void ODWaveTrackTaskQueue::DemandTrackUpdate(WaveTrack* track, double seconds)
{
   if(track)
   {
      mTracksMutex.Lock();
      for(unsigned int i=0;i<mTasks.size();i++)
      {
         mTasks[i]->DemandTrackUpdate(track,seconds);
      }

      mTracksMutex.Unlock();
   }
}


//Replaces all instances of a wavetracck with a NEW one (effectively transferes the task.)
void ODWaveTrackTaskQueue::ReplaceWaveTrack(WaveTrack* oldTrack, WaveTrack* newTrack)
{
   if(oldTrack)
   {
      mTasksMutex.Lock();
      for(unsigned int i=0;i<mTasks.size();i++)
         mTasks[i]->ReplaceWaveTrack(oldTrack,newTrack);
      mTasksMutex.Unlock();

      mTracksMutex.Lock();
      for(unsigned int i=0;i<mTracks.size();i++)
         if(mTracks[i]==oldTrack)
            mTracks[i]=newTrack;
      mTracksMutex.Unlock();
   }
}

//returns the wavetrack at position x.
WaveTrack* ODWaveTrackTaskQueue::GetWaveTrack(size_t x)
{
   WaveTrack* ret = NULL;
   mTracksMutex.Lock();
   if (x < mTracks.size())
      ret = mTracks[x];
   mTracksMutex.Unlock();
   return ret;
}

///returns the number of wavetracks in this queue.
int ODWaveTrackTaskQueue::GetNumWaveTracks()
{
   int ret = 0;
   mTracksMutex.Lock();
   ret=mTracks.size();
   mTracksMutex.Unlock();
   return ret;
}

///returns the number of ODTasks in this queue
int ODWaveTrackTaskQueue::GetNumTasks()
{
   int ret = 0;
   mTasksMutex.Lock();
   ret=mTasks.size();
   mTasksMutex.Unlock();
   return ret;
}

///returns a ODTask at position x
ODTask* ODWaveTrackTaskQueue::GetTask(size_t x)
{
   ODTask* ret = NULL;
   mTasksMutex.Lock();
   if (x < mTasks.size())
      ret = mTasks[x].get();
   mTasksMutex.Unlock();
   return ret;
}



//returns true if either tracks or tasks are empty
bool ODWaveTrackTaskQueue::IsEmpty()
{
   bool isEmpty;
   mTracksMutex.Lock();
   isEmpty = mTracks.size()<=0;
   mTracksMutex.Unlock();

   mTasksMutex.Lock();
   isEmpty = isEmpty || mTasks.size()<=0;
   mTasksMutex.Unlock();

   return isEmpty;
}

//returns true if the foremost task exists and is empty.
bool ODWaveTrackTaskQueue::IsFrontTaskComplete()
{
   mTasksMutex.Lock();
   if(mTasks.size())
   {
      //there is a chance the task got updated and now has more to do, (like when it is joined with a NEW track)
      //check.
      mTasks[0]->RecalculatePercentComplete();
      bool ret;
      ret = mTasks[0]->IsComplete();
      mTasksMutex.Unlock();

      return ret;
   }
   mTasksMutex.Unlock();
   return false;
}

///Removes and deletes the front task from the list.
void ODWaveTrackTaskQueue::RemoveFrontTask()
{
   mTasksMutex.Lock();
   if(mTasks.size())
   {
      //wait for the task to stop running.
      mTasks.erase(mTasks.begin());
   }
   mTasksMutex.Unlock();
}

///gets the front task for immediate execution
ODTask* ODWaveTrackTaskQueue::GetFrontTask()
{
   mTasksMutex.Lock();
   if(mTasks.size())
   {
      mTasksMutex.Unlock();
      return mTasks[0].get();
   }
   mTasksMutex.Unlock();
   return NULL;
}

///fills in the status bar message for a given track
void ODWaveTrackTaskQueue::FillTipForWaveTrack( WaveTrack * t, wxString &tip )
{
   if(ContainsWaveTrack(t) && GetNumTasks())
   {

    //  if(GetNumTasks()==1)
      mTipMsg.Printf(_("%s %2.0f%% complete.  Click to change task focal point."), GetFrontTask()->GetTip(), GetFrontTask()->PercentComplete()*100.0 );
     // else
       //  msg.Printf(_("%s %d additional tasks remaining."), GetFrontTask()->GetTip().c_str(), GetNumTasks());

      tip = mTipMsg.c_str();

   }
}
