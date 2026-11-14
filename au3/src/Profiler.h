/**********************************************************************

  Audacity: A Digital Audio Editor

  Profiler.h

  Created by Michael Chinen (mchinen) on 8/12/08
  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2 or later.  See License.txt.

******************************************************************//**

\class Profiler
\brief A simple profiler to measure the average time lengths that a
particular task/function takes.  Currently not thread-safe and not thread-smart,
but it will probably work fine if you use it on a high level.

\class TaskProfile
\brief a simple class to keep track of one task that may be called multiple times.

*//*******************************************************************/

#ifndef __AUDACITY_PROFILER__
#define __AUDACITY_PROFILER__
#include <mutex>
#include <vector>
#include <time.h>
#include "MemoryX.h"

#define BEGIN_TASK_PROFILING(TASK_DESCRIPTION) Profiler::Instance()->Begin(__FILE__, __LINE__, TASK_DESCRIPTION)
#define END_TASK_PROFILING(TASK_DESCRIPTION) Profiler::Instance()->End(__FILE__, __LINE__, TASK_DESCRIPTION)

class TaskProfile;
class Profiler
{
public:

    ///write to a profile at the end of the test.
    virtual ~Profiler();

    ///start the task timer.
    void Begin(const char* fileName, int lineNum, const char* taskDescription);
    ///end the task timer.
    void End(const char* fileName, int lineNum, const char* taskDescription);

    ///Gets the singleton instance
    static Profiler* Instance();

protected:
    ///private constructor - Singleton.
    Profiler() {}

    ///find a taskProfile for the given task, otherwise create
    TaskProfile* GetOrCreateTaskProfile(const char* fileName, int lineNum);
    TaskProfile* GetTaskProfileByDescription(const char* description);

    //List of current Task to do.
    std::vector<std::unique_ptr<TaskProfile> > mTasks;
    //mutex for above variable
    std::mutex mTasksMutex;
};

class TaskProfile
{
public:
    TaskProfile();
    virtual ~TaskProfile();

    ///start the task timer.
    void Begin(const char* fileName, int lineNum, const char* taskDescription);
    ///end the task timer.
    void End(const char* fileName, int lineNum, const char* taskDescription);

    double ComputeAverageRunTime();

    ArrayOf<char> mFileName;
    int mLine;
    ArrayOf<char> mDescription;
    int mNumHits;
    clock_t mCumTime;
    clock_t mLastTime;
};

#endif
