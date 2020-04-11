/**********************************************************************

  Audacity: A Digital Audio Editor

  ODWaveTrackTaskQueue.h

  Created by Michael Chinen (mchinen)
  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

******************************************************************//**

\class ODWaveTrackTaskQueue
\brief watches over all to be done (not yet started and started but not finished)
tasks associated with a WaveTrack.

*//*******************************************************************/




#ifndef __AUDACITY_ODWAVETRACKTASKQUEUE__
#define __AUDACITY_ODWAVETRACKTASKQUEUE__

#include <vector>
#include "ODTaskThread.h"
#include "../Internat.h" // for TranslatableString
class Track;
class WaveTrack;
class ODTask;
/// A class representing a modular task to be used with the On-Demand structures.
class ODWaveTrackTaskQueue final
{
 public:

   // Constructor / Destructor

   /// Constructs an ODWaveTrackTaskQueue
   ODWaveTrackTaskQueue();

   virtual ~ODWaveTrackTaskQueue();



   ///Adds a track to the associated list.
   void AddWaveTrack( const std::shared_ptr< WaveTrack > &track);

   ///changes the tasks associated with this Waveform to process the task from a different point in the track
   void DemandTrackUpdate(WaveTrack* track, double seconds);

   ///replaces all instances of a WaveTrack within this task with another.
   void ReplaceWaveTrack(Track *oldTrack,
      const std::shared_ptr<Track> &newTrack);

   //if the wavetrack is in this queue, and is not the only wavetrack, clones the tasks and schedules it.
   void MakeWaveTrackIndependent( const std::shared_ptr< WaveTrack > &track);

   ///returns whether or not this queue's task list and another's can merge together, as when we make two mono tracks stereo.
   bool CanMergeWith(ODWaveTrackTaskQueue* otherQueue);
   void MergeWaveTrack( const std::shared_ptr< WaveTrack > &track);


   //returns true if the argument is in the WaveTrack list.
   bool ContainsWaveTrack(const WaveTrack* track);

   ///returns the number of wavetracks in this queue.
   int GetNumWaveTracks();

   ///Add a task to the queue.
   void AddTask(std::unique_ptr<ODTask> &&mtask);

   //returns true if either tracks or tasks are empty
   bool IsEmpty();

   //returns true if the foremost task exists and is empty.
   bool IsFrontTaskComplete();

   ///Removes and deletes the front task from the list.
   void RemoveFrontTask();

   ///Schedules the front task for immediate execution
   ODTask* GetFrontTask();

   ///returns the number of ODTasks in this queue
   int GetNumTasks();

   ///returns a ODTask at position x
   ODTask* GetTask(size_t x);

   ///fills in the status bar message for a given track
   void FillTipForWaveTrack( const WaveTrack * t, TranslatableString &tip );

 protected:

   // Remove expired weak pointers to tracks
   void Compress();

   //because we need to save this around for the tool tip.
   TranslatableString mTipMsg;


  ///the list of tracks associated with this queue.
  std::vector< std::weak_ptr< WaveTrack > > mTracks;
  ODLock mTracksMutex;

  ///the list of tasks associated with the tracks.  This class owns these tasks.
  std::vector<std::unique_ptr<ODTask>> mTasks;
  ODLock    mTasksMutex;

};

#endif

