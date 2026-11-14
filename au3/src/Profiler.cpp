/**********************************************************************

  Audacity: A Digital Audio Editor

  Profiler.cpp

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

#include "Profiler.h"

#include <string.h>
#include <wx/crt.h>

///write to a profile at the end of the test.
Profiler::~Profiler()
{
    if (mTasks.size()) {
        //print everything out.  append to a log.
        FILE* log = fopen("AudacityProfilerLog.txt", "a");
        time_t now;

        time(&now);
        wxFprintf(log, "Audacity Profiler Run, Ended at ");
        wxFprintf(log, "%s", ctime(&now));
        wxFprintf(log, "****************************************\n");
        //print out the tasks
        for (int i=0; i < (int)mTasks.size(); i++) {
            if (mTasks[i]->mNumHits > 0) {
                wxFprintf(log, "Task: %s\n(begins at line %d in %s)\n\n",
                          mTasks[i]->mDescription.get(), mTasks[i]->mLine, mTasks[i]->mFileName.get());
                wxFprintf(log, "Number of times run: %d\n", mTasks[i]->mNumHits);
                wxFprintf(log, "Total run time (seconds): %f\n", (double)mTasks[i]->mCumTime / CLOCKS_PER_SEC);
                wxFprintf(log, "Average run time (seconds): %f\n", mTasks[i]->ComputeAverageRunTime());

                if (i < ((int)mTasks.size()) - 1) {
                    wxFprintf(log, "----------------------------\n");
                }
            }
        }
        wxFprintf(log, "\n****************************************\n\n\n");

        fclose(log);
    }
}

///start the task timer.
void Profiler::Begin(const char* fileName, int lineNum, const char* taskDescription)
{
    std::lock_guard<std::mutex> guard{ mTasksMutex };
    GetOrCreateTaskProfile(fileName, lineNum)->Begin(fileName, lineNum, taskDescription);
}

///end the task timer.
void Profiler::End(const char* fileName, int lineNum, const char* taskDescription)
{
    std::lock_guard<std::mutex> guard{ mTasksMutex };
    TaskProfile* tp;
    tp=GetTaskProfileByDescription(taskDescription);
    if (tp) {
        tp->End(fileName, lineNum, taskDescription);
    }
}

///Gets the singleton instance
Profiler* Profiler::Instance()
{
    static Profiler pro;
    //this isn't 100% threadsafe but I think Okay for this purpose.

    return &pro;
}

///find a taskProfile for the given task, otherwise create
TaskProfile* Profiler::GetOrCreateTaskProfile(const char* fileName, int lineNum)
{
    for (int i=0; i < (int)mTasks.size(); i++) {
        if (strcmp(fileName, mTasks[i]->mFileName.get()) == 0 && lineNum == mTasks[i]->mLine) {
            return mTasks[i].get();
        }
    }

    auto tp = std::make_unique<TaskProfile>();
    mTasks.push_back(std::move(tp));
    return mTasks.back().get();
}

TaskProfile* Profiler::GetTaskProfileByDescription(const char* description)
{
    for (int i=0; i < (int)mTasks.size(); i++) {
        if (strcmp(description, mTasks[i]->mDescription.get()) == 0) {
            return mTasks[i].get();
        }
    }

    return NULL;
}

///Task Profile
TaskProfile::TaskProfile()
{
    mCumTime=0;
    mNumHits=0;
}

TaskProfile::~TaskProfile()
{
}

///start the task timer.
void TaskProfile::Begin(const char* fileName, int lineNum, const char* taskDescription)
{
    if (!mFileName) {
        mFileName.reinit(strlen(fileName) + 1);
        strcpy(mFileName.get(), fileName);
        mDescription.reinit(strlen(taskDescription) + 1);
        strcpy(mDescription.get(), taskDescription);
        mLine = lineNum;
    }

    mLastTime = clock();
}

///end the task timer.
void TaskProfile::End(const char* WXUNUSED(fileName), int WXUNUSED(lineNum), const char* WXUNUSED(taskDescription))
{
    mCumTime += clock() - mLastTime;
    mNumHits++;
}

double TaskProfile::ComputeAverageRunTime()
{
    if (mNumHits) {
        return (double)((double)mCumTime / CLOCKS_PER_SEC) / mNumHits;
    } else {
        return 0.0;
    }
}
