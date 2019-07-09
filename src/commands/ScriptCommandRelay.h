/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   File License: wxWidgets

   Dan Horgan

******************************************************************//**

\file ScriptCommandRelay.h
\brief Contains declarations for ScriptCommandRelay

*//*******************************************************************/

#ifndef __SCRIPT_COMMAND_RELAY__
#define __SCRIPT_COMMAND_RELAY__

#include "../Audacity.h"

#include "../MemoryX.h"

class wxWindow;
class CommandHandler;
class Response;
class OldStyleCommand;
using OldStyleCommandPointer = std::shared_ptr<OldStyleCommand>;
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

   public:

      static void SetRegScriptServerFunc(tpRegScriptServerFunc scriptFn);
      static void SetCommandHandler(CommandHandler &ch);

      static void Run();
      static void PostCommand(
         wxWindow *pWindow, const OldStyleCommandPointer &cmd);
      static Response ReceiveResponse();
};

#endif /* End of include guard: __SCRIPT_COMMAND_RELAY__ */
