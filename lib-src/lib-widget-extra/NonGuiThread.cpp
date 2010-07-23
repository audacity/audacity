/*************************************************************************

  NonGuiThread.cpp

  James Crook
  (C) Audacity Developers, 2007

  wxWidgets license. See Licensing.txt

**********************************************************************//**

\class NonGuiThread
\brief NonGuiThread a thread class that allows non-GUI activities to
take place in the background without killing the GUI.

*//**********************************************************************/

#include <wx/wx.h>
#include <wx/apptrait.h>
#include "NonGuiThread.h"

bool NonGuiThread::IsLive=false;

NonGuiThread::NonGuiThread(tGenericFn pFn)
{
   mpFn = pFn;
   IsLive=true;
   mbExit = false;
}

NonGuiThread::~NonGuiThread()
{
   IsLive=false;
}

NonGuiThread::ExitCode NonGuiThread::Entry()
{
   // The while isn't needed here, but may be later if we break the function
   // up...
   while( !TestDestroy() && !mbExit  )
   {
      mbExit=true;
      (*mpFn)();
   }
   return (ExitCode)0;
}

// This one runs the function and only returns when function
// has run to completion.
void NonGuiThread::RunInThread(tGenericFn pFn)
{
   #ifdef WXMSW
   wxAppTraits *traits = wxTheApp ? wxTheApp->GetTraits() : NULL;
   wxASSERT( traits );//"no wxAppTraits in RunInThread()?"

   void *cookie = NULL;
   // disable all app windows while waiting for the child process to finish
   cookie = traits->BeforeChildWaitLoop();
   #endif

   NonGuiThread * mpThread = new NonGuiThread(pFn);
   mpThread->Create();
   mpThread->Resume();
   wxLogDebug(wxT("Into the thread..."));
   while( mpThread->IsLive )
   {
      wxMilliSleep( 100 );
      //traits->AlwaysYield();
      wxGetApp().Yield();
   }
   #ifdef WXMSW
   traits->AfterChildWaitLoop(cookie);
   #endif
}

// This function starts the thread and returns immediately.
NonGuiThread * NonGuiThread::StartChild( tGenericFn pFn )
{
   NonGuiThread * pThread = new NonGuiThread(pFn);
   //pThread->mpFn = pFn;
   pThread->Create();
   pThread->Run();
   return pThread;
}
