/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   File License: wxWidgets

   Dan Horgan

******************************************************************//**

\file ScriptCommandRelay.h
\brief Contains declarations for ScriptCommandRelay

*//*******************************************************************/

#ifndef __SCRIPTCOMMANDRELAY__
#define __SCRIPTCOMMANDRELAY__

#include "../Audacity.h"

class CommandHandler;
class ResponseQueue;
class Response;
class ResponseQueueTarget;
class AudacityProject;
class Command;
class wxString;

typedef int (*tpExecScriptServerFunc)( wxString * pIn, wxString * pOut);
typedef int (*tpRegScriptServerFunc)(tpExecScriptServerFunc pFn);

extern "C" {
      AUDACITY_DLL_API int ExecCommand(wxString *pIn, wxString *pOut);
} // End 'extern C'

class ScriptCommandRelay
{
   private:
      // N.B. Static class members also have to be declared in the .cpp file
      static CommandHandler *sCmdHandler;
      static tpRegScriptServerFunc sScriptFn;
      static ResponseQueue sResponseQueue;

   public:

      static void SetRegScriptServerFunc(tpRegScriptServerFunc scriptFn);
      static void SetCommandHandler(CommandHandler &ch);

      static void Run();
      static void PostCommand(AudacityProject *project, Command *cmd);
      static void SendResponse(const wxString &response);
      static Response ReceiveResponse();
      static ResponseQueueTarget *GetResponseTarget();
};

#endif /* End of include guard: __SCRIPTCOMMANDRELAY__ */

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: TBD
