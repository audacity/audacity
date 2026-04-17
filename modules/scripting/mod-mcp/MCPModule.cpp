/**********************************************************************

   Audacity - A Digital Audio Editor

   MCPModule.cpp

   Model Context Protocol module entry point. Counterpart to
   mod-script-pipe/ScripterCallback.cpp: we register with
   ScriptCommandRelay to obtain the in-process command dispatch hook,
   then delegate the event loop to MCPServer.

   Exposing Audacity's scripting surface to network clients is a
   security-sensitive feature. This module only binds to 127.0.0.1 but
   users should still be aware of it before loading the module.

**********************************************************************/

#include <wx/wx.h>

#include "MCPServer.h"
#include "ModuleConstants.h"
#include "commands/ScriptCommandRelay.h"

extern "C" {

static int DLL_API RegMCPServer(tpExecScriptServerFunc pFn)
{
   if (pFn)
      MCPServer::Instance().Serve(pFn);
   return 1;
}

DEFINE_VERSION_CHECK

extern "C" DLL_API int ModuleDispatch(ModuleDispatchTypes type)
{
   switch (type)
   {
   case ModuleInitialize:
      ScriptCommandRelay::StartScriptServer(RegMCPServer);
      break;
   case ModuleTerminate:
      MCPServer::Instance().Stop();
      break;
   default:
      break;
   }
   return 1;
}

} // extern "C"
