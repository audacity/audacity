/**********************************************************************

  Audacity: A Digital Audio Editor

  Profiler.cpp

  Created by Michael Chinen (mchinen) on 8/12/08
  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

******************************************************************//**

\class Profiler
\brief A simple profiler to measure the average time lengths that a
particular task/function takes.  Currently not thread-safe and not thread-smart,
but it will probably work fine if you use it on a high level.

\class TaskProfile
\brief a simple class to keep track of one task that may be called multiple times.

*//*******************************************************************/

#include "Audacity.h"
#include "Profiler.h"
#include <stdio.h>
#include <string.h>

///write to a profile at the end of the test.
Profiler::~Profiler()
{
   if(mTasks.size())
   {
      //print everything out.  append to a log.
      FILE* log = fopen("AudacityProfilerLog.txt", "a");
      time_t now;

      time(&now);
      fprintf(log,"Audacity Profiler Run, Ended at ");
      fprintf(log,"%s",ctime(&now));
      fprintf(log,"****************************************\n");
      //print out the tasks
      for(int i=0;i<(int)mTasks.size();i++)
      {
         if(mTasks[i]->mNumHits>0)
         {
            fprintf(log,"Task: %s\n(begins at line %d in %s)\n\n",mTasks[i]->mDescription,mTasks[i]->mLine,mTasks[i]->mFileName);
            fprintf(log,"Number of times run: %d\n",mTasks[i]->mNumHits);
            fprintf(log,"Total run time (seconds): %f\n", (double)mTasks[i]->mCumTime/CLOCKS_PER_SEC);
            fprintf(log,"Average run time (seconds): %f\n",mTasks[i]->ComputeAverageRunTime());

            if(i < ((int)mTasks.size()) -1)
               fprintf(log,"----------------------------\n");
         }
      }
      fprintf(log,"\n****************************************\n\n\n");

      fclose(log);
   }
}

///start the task timer.
void Profiler::Begin(char* fileName, int lineNum, char* taskDescription)
{
   mTasksMutex.Lock();
   GetOrCreateTaskProfile(fileName,lineNum)->Begin(fileName,lineNum,taskDescription);
   mTasksMutex.Unlock();
}

///end the task timer.
void Profiler::End(char* fileName, int lineNum, char* taskDescription)
{
   mTasksMutex.Lock();
   TaskProfile* tp;
   tp=GetTaskProfileByDescription(taskDescription);
   if(tp)
      tp->End(fileName,lineNum,taskDescription);
   mTasksMutex.Unlock();
}

///Gets the singleton instance
Profiler* Profiler::Instance()
{
   static Profiler pro;
   //this isn't 100% threadsafe but I think Okay for this purpose.

   return &pro;
}

///find a taskProfile for the given task, otherwise create
TaskProfile* Profiler::GetOrCreateTaskProfile(char* fileName, int lineNum)
{
   for(int i=0;i<(int)mTasks.size();i++)
   {
      if(strcmp(fileName,mTasks[i]->mFileName)==0 && lineNum == mTasks[i]->mLine)
         return mTasks[i].get();
   }

   auto tp = make_movable<TaskProfile>();
   mTasks.push_back(std::move(tp));
   return mTasks.back().get();
}

TaskProfile* Profiler::GetTaskProfileByDescription(char* description)
{
   for(int i=0;i<(int)mTasks.size();i++)
   {
      if(strcmp(description,mTasks[i]->mDescription)==0)
         return mTasks[i].get();
   }

   return NULL;

}


///Task Profile
TaskProfile::TaskProfile()
{
   mFileName = NULL;
   mCumTime=0;
   mNumHits=0;
}

TaskProfile::~TaskProfile()
{
   if(mFileName)
   {
      delete [] mFileName;
      delete [] mDescription;
   }
}

///start the task timer.
void TaskProfile::Begin(char* fileName, int lineNum, char* taskDescription)
{
   if(!mFileName)
   {
      mFileName = new char[strlen(fileName)+1];
      strcpy(mFileName,fileName);
      mDescription = new char[strlen(taskDescription)+1];
      strcpy(mDescription,taskDescription);
      mLine = lineNum;
   }

   mLastTime = clock();

}

///end the task timer.
void TaskProfile::End(char* WXUNUSED(fileName), int WXUNUSED(lineNum), char* WXUNUSED(taskDescription))
{
   mCumTime += clock() - mLastTime;
   mNumHits++;
}

double TaskProfile::ComputeAverageRunTime()
{
   if(mNumHits)
      return (double) ((double)mCumTime/CLOCKS_PER_SEC)/mNumHits;
   else
      return 0.0;
}
