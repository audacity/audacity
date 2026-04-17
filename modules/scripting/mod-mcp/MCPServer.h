/**********************************************************************

   Audacity - A Digital Audio Editor

   MCPServer.h

   Declares the localhost HTTP server that exposes Audacity's scripting
   commands as Model Context Protocol tools. Mirrors the role that
   PipeServer plays in mod-script-pipe.

**********************************************************************/

#pragma once

#include <atomic>
#include <memory>
#include <mutex>

#include "commands/ScriptCommandRelay.h" // tpExecScriptServerFunc

namespace httplib { class Server; struct Request; struct Response; }
class wxString;

class MCPServer
{
public:
   static MCPServer& Instance();

   // Registered via ScriptCommandRelay::StartScriptServer. First call spins
   // up the HTTP server on the relay thread and blocks there; any re-entry
   // parks the thread so the relay's while-loop doesn't restart the server.
   void Serve(tpExecScriptServerFunc pFn);

   // Called from ModuleDispatch(ModuleTerminate).
   void Stop();

private:
   MCPServer() = default;
   ~MCPServer() = default;
   MCPServer(const MCPServer&) = delete;
   MCPServer& operator=(const MCPServer&) = delete;

   void HandleMCPRequest(const httplib::Request &req, httplib::Response &res);
   wxString DispatchAudacityCommand(const wxString &cmdString);

   std::mutex mFnMutex;
   tpExecScriptServerFunc mExecFn = nullptr;

   std::atomic<bool> mServing{false};

   struct Impl;
   std::unique_ptr<Impl> mImpl;
};
