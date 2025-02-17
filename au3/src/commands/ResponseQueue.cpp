/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   File License: wxWidgets

   Dan Horgan

******************************************************************//**

\file ResponseQueue.cpp
\brief Contains definitions for the ResponseQueue class

*//*******************************************************************/

#include "ResponseQueue.h"

#include <queue>
#include <string>
#include <wx/thread.h>

ResponseQueue::ResponseQueue()
    : mCondition(mMutex)
{ }

ResponseQueue::~ResponseQueue()
{ }

void ResponseQueue::AddResponse(Response response)
{
    wxMutexLocker locker(mMutex);
    mResponses.push(response);
    mCondition.Signal();
}

Response ResponseQueue::WaitAndGetResponse()
{
    wxMutexLocker locker(mMutex);
    if (mResponses.empty()) {
        mCondition.Wait();
    }
    wxASSERT(!mResponses.empty());
    Response msg = mResponses.front();
    mResponses.pop();
    return msg;
}
