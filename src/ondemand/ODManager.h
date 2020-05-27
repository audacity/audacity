/**********************************************************************

  Audacity: A Digital Audio Editor

  ODManager.h

  Created by Michael Chinen (mchinen) on 6/8/08
  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

******************************************************************//**

\class ODManager
\brief A singleton that manages currently running Tasks on an arbitrary
number of threads.

*//*******************************************************************/

#ifndef __AUDACITY_ODMANAGER__
#define __AUDACITY_ODMANAGER__

#include <vector>
#include "ODTaskThread.h"
#include <wx/event.h> // for DECLARE_EXPORTED_EVENT_TYPE

#ifdef __WXMAC__
// On Mac OS X, it's better not to use the wxThread class.
// We use our own implementation based on pthreads instead.
#include <pthread.h>
#include <time.h>
#endif //__WXMAC__

// This event is posted to the application
wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API,
                         EVT_ODTASK_UPDATE, wxCommandEvent);

///wxstring compare function for sorting case, which is needed to load correctly.
int CompareNoCaseFileName(const wxString& first, const wxString& second);
/// A singleton that manages currently running Tasks on an arbitrary
/// number of threads.
class TranslatableString;
class Track;
class WaveTrack;
class ODWaveTrackTaskQueue;
class ODManager final
{
 public:
   ///Gets the singleton instance - this is a function pointer that points to one of the below two instance calls.
   ///Note that it is not a member function pointer since it is a static function.
   ///the function pointer swapping is valid as long as the initial calls only happen from the main thread.
   static ODManager* (*Instance)();
   ///Gets the singleton instance
   static ODManager* InstanceFirstTime();
   ///Gets the singleton instance
   static ODManager* InstanceNormal();

   ///Kills the ODMananger Thread.
   static void Quit();

   ///changes the tasks associated with this Waveform to process the task from a different point in the track
   void DemandTrackUpdate(WaveTrack* track, double seconds);

   ///Reduces the count of current threads running.  Meant to be called when ODTaskThreads end in their own threads.  Thread-safe.
   void DecrementCurrentThreads();

   ///Adds a wavetrack, creates a queue member.
   void AddNewTask(std::unique_ptr<ODTask> &&mtask, bool lockMutex=true);

   ///Wakes the queue loop up by signalling its condition variable.
   void SignalTaskQueueLoop();

   ///if it shares a queue/task, creates a NEW queue/task for the track, and removes it from any previously existing tasks.
   void MakeWaveTrackIndependent( const std::shared_ptr< WaveTrack > &track);

   ///attach the track in question to another, already existing track's queues and tasks.  Remove the task/tracks.
   ///Returns success if it was possible..  Some ODTask conditions make it impossible until the Tasks finish.
   bool MakeWaveTrackDependent(
      const std::shared_ptr< WaveTrack > &dependentTrack,
      WaveTrack* masterTrack
   );

   ///if oldTrack is being watched,
   ///replace the wavetrack whose wavecache the gui watches for updates
   void ReplaceWaveTrack(Track *oldTrack,
      const std::shared_ptr< Track > &newTrack);

   ///Adds a task to the running queue.  Thread-safe.
   void AddTask(ODTask* task);

   void RemoveTaskIfInQueue(ODTask* task);

   ///sets a flag that is set if we have loaded some OD blockfiles from PCM.
   static void MarkLoadedODFlag();

   ///resets a flag that is set if we have loaded some OD blockfiles from PCM.
   static void UnmarkLoadedODFlag();

   ///returns a flag that is set if we have loaded some OD blockfiles from PCM.
   static bool HasLoadedODFlag();

   ///returns whether or not the singleton instance was created yet
   static bool IsInstanceCreated();

   ///fills in the status bar message for a given track
   void FillTipForWaveTrack( const WaveTrack * t, TranslatableString &tip );

   ///Gets the total percent complete for all tasks combined.
   float GetOverallPercentComplete();

   ///Get Total Number of Tasks.
   int GetTotalNumTasks();

   // RAII object for pausing and resuming..
   class Pauser
   {
      //Pause/unpause all OD Tasks.  Does not occur immediately.
      static void Pause(bool pause = true);
      static void Resume();
   public:
      Pauser() { Pause(); }
      ~Pauser() { Resume(); }
      Pauser(const Pauser&) PROHIBITED;
      Pauser &operator= (const Pauser&) PROHIBITED;
   };



  protected:
   //private constructor - Singleton.
   ODManager();
   //private constructor - DELETE with static method Quit()
   friend std::default_delete < ODManager > ;
   ~ODManager();
   ///Launches a thread for the manager and starts accepting Tasks.
   void Init();

   ///Start the main loop for the manager.
   void Start();

   ///Remove references in our array to Tasks that have been completed/Schedule NEW ones
   void UpdateQueues();

   //instance
   static std::unique_ptr<ODManager> pMan;

   //List of tracks and their active and inactive tasks.
   std::vector<std::unique_ptr<ODWaveTrackTaskQueue>> mQueues;
   ODLock mQueuesMutex;

   //List of current Task to do.
   std::vector<ODTask*> mTasks;
   //mutex for above variable
   ODLock mTasksMutex;

   //global pause switch for OD
   volatile bool mPause;
   ODLock mPauseLock;

   volatile int mNeedsDraw;

   ///Number of threads currently running.   Accessed thru multiple threads
   volatile int mCurrentThreads;
   //mutex for above variable
   ODLock mCurrentThreadsMutex;

   ///Maximum number of threads allowed out.
   int mMaxThreads;

   volatile bool mTerminate;
   ODLock mTerminateMutex;

   volatile bool mTerminated;
   ODLock mTerminatedMutex;

   //for the queue not empty comdition
   ODLock         mQueueNotEmptyCondLock;
   std::unique_ptr<ODCondition> mQueueNotEmptyCond;

#ifdef __WXMAC__

// On Mac OS X, it's better not to use the wxThread class.
// We use our own implementation based on pthreads instead.

class ODManagerHelperThread {
 public:
   typedef int ExitCode;
   ODManagerHelperThread() { mDestroy = false; mThread = NULL; }
  /* ExitCode*/ void Entry(){
         ODManager::Instance()->Start();
   }

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
      ODManagerHelperThread *th = (ODManagerHelperThread *)p;
      /* return (void *) */th->Entry();
      return NULL;
   }

   ///Specifies the priority the thread will run at.  Currently doesn't work.
   ///@param priority value from 0 (min priority) to 100 (max priority)
   void SetPriority(int priority)
   {
      mPriority=priority;
   }

   void Run() {
      pthread_create(&mThread, NULL, callback, this);
   }
 private:
   bool mDestroy;
   pthread_t mThread;
   int mPriority;
};
#else
   class ODManagerHelperThread final : public wxThread
   {
      public:
      ///Constructs an ODManagerHelperThread
      ///@param task the task to be launched as an
      ODManagerHelperThread(): wxThread(){}

      protected:
      ///Executes a part of the task
      void *Entry()
      {
         ODManager::Instance()->Start();
         return NULL;
      }

   };
#endif //__WXMAC__
};

#endif

