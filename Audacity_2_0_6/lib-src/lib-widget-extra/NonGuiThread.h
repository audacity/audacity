/*************************************************************************

  NonGuiThread.h

  James Crook
  (C) Audacity Developers, 2007

  wxWidgets license. See Licensing.txt

*************************************************************************/

#if !defined(AFX_NONGUITHREAD_H__E8F7FC2B_CB13_497B_A556_18551596AFD9__INCLUDED_)
#define AFX_NONGUITHREAD_H__E8F7FC2B_CB13_497B_A556_18551596AFD9__INCLUDED_

typedef void (*tGenericFn)(void);
//#include "AllCommands.h" // for tGenericFn
//#include "WidgetExtra.h"

class /*WIDGET_EXTRA_DLL*/ NonGuiThread : public wxThread
{
public:
   NonGuiThread(tGenericFn pFn);
   virtual ~NonGuiThread();
   NonGuiThread::ExitCode Entry();
   static void RunInThread(tGenericFn pFn);
   static NonGuiThread * StartChild( tGenericFn pFn );

public:
   bool mbExit;
   tGenericFn mpFn;
   static bool IsLive;
};

#endif // !defined(AFX_NONGUITHREAD_H__E8F7FC2B_CB13_497B_A556_18551596AFD9__INCLUDED_)
