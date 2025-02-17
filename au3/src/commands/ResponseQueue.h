/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxWidgets

   Dan Horgan

******************************************************************//**

\file ResponseQueue.h
\brief Contains declarations for Response and ResponseQueue classes

*//****************************************************************//**

\class Response
\brief Stores a command response string (and other response data if it becomes
necessary)

The string is internally stored as a std::string rather than wxString
because of thread-safety concerns.

*//****************************************************************//**

\class ResponseQueue
\brief Allow messages to be sent from the main thread to the script thread

Based roughly on wxMessageQueue<T> (which hasn't reached the stable wxwidgets
yet). Wraps a std::queue<Response> inside a wxMutex with a wxCondition to
force the script thread to wait until a response is available.

*//*******************************************************************/

#ifndef __RESPONSEQUEUE__
#define __RESPONSEQUEUE__

#include <queue>
#include <string>
#include <wx/thread.h> // member variable
#include <wx/string.h> // member variable

class wxMutex;
class wxCondition;
class wxMutexLocker;

class Response
{
private:
    std::string mMessage;
public:
    Response(const wxString& response)
        : mMessage(response.utf8_str())
    { }

    wxString GetMessage()
    {
        return wxString(mMessage.c_str(), wxConvUTF8);
    }
};

class ResponseQueue
{
private:
    std::queue<Response> mResponses;
    wxMutex mMutex;
    wxCondition mCondition;

public:
    ResponseQueue();
    ~ResponseQueue();

    void AddResponse(Response response);
    Response WaitAndGetResponse();
};

#endif /* End of include guard: __RESPONSEQUEUE__ */
