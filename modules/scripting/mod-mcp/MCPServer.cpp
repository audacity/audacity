/**********************************************************************

   Audacity - A Digital Audio Editor

   MCPServer.cpp

   HTTP+JSON-RPC front-end that wraps Audacity's existing scripting
   dispatch. Parallels mod-script-pipe's PipeServer/ScripterCallback pair
   but speaks Model Context Protocol (2024-11-05).

**********************************************************************/

// httplib pulls in <windows.h> on Windows; keep its footprint small and
// stop it from poisoning std::min / std::max before wx headers arrive.
#ifdef _WIN32
#  ifndef NOMINMAX
#    define NOMINMAX
#  endif
#  ifndef WIN32_LEAN_AND_MEAN
#    define WIN32_LEAN_AND_MEAN
#  endif
#endif

#include <httplib.h>

#include "MCPServer.h"

#include <wx/string.h>

#include <rapidjson/document.h>
#include <rapidjson/stringbuffer.h>
#include <rapidjson/writer.h>

#include <chrono>
#include <cstdlib>
#include <sstream>
#include <string>
#include <thread>

namespace {

constexpr const char *kProtocolVersion = "2024-11-05";
constexpr const char *kServerName      = "audacity";
constexpr const char *kServerVersion   = "0.1.0";
constexpr const char *kDefaultHost     = "127.0.0.1";
constexpr int         kDefaultPort     = 8765;

// JSON-RPC 2.0 standard error codes.
constexpr int kParseError     = -32700;
constexpr int kInvalidRequest = -32600;
constexpr int kMethodNotFound = -32601;
constexpr int kInvalidParams  = -32602;

int ResolvePort()
{
   if (const char *env = std::getenv("AUDACITY_MCP_PORT"))
   {
      try
      {
         const int v = std::stoi(env);
         if (v > 0 && v < 65536)
            return v;
      }
      catch (...) { /* fall through to default */ }
   }
   return kDefaultPort;
}

// Audacity's CLI param parser doesn't support escape sequences inside
// quotes, so we have to pick whichever quote the value doesn't contain.
// If it contains both, the parser can't express it faithfully; we strip
// embedded double quotes as a last resort.
std::string QuoteParamValue(const std::string &v)
{
   const bool hasDq = v.find('"')  != std::string::npos;
   const bool hasSq = v.find('\'') != std::string::npos;
   if (!hasDq)
      return '"' + v + '"';
   if (!hasSq)
      return '\'' + v + '\'';
   std::string stripped;
   stripped.reserve(v.size());
   for (char c : v)
      if (c != '"')
         stripped.push_back(c);
   return '"' + stripped + '"';
}

std::string ScalarToString(const rapidjson::Value &v)
{
   if (v.IsString())
      return std::string(v.GetString(), v.GetStringLength());
   if (v.IsBool())
      return v.GetBool() ? "True" : "False";
   if (v.IsInt())    return std::to_string(v.GetInt());
   if (v.IsInt64())  return std::to_string(v.GetInt64());
   if (v.IsUint())   return std::to_string(v.GetUint());
   if (v.IsUint64()) return std::to_string(v.GetUint64());
   if (v.IsDouble())
   {
      std::ostringstream os;
      os.imbue(std::locale::classic());
      os << v.GetDouble();
      return os.str();
   }
   if (v.IsNull())
      return {};

   // Objects and arrays: pass through as compact JSON. Some Audacity
   // commands (e.g. Import2 FileNames) accept JSON-shaped values.
   rapidjson::StringBuffer sb;
   rapidjson::Writer<rapidjson::StringBuffer> w(sb);
   v.Accept(w);
   return std::string(sb.GetString(), sb.GetSize());
}

std::string BuildCommandString(const std::string &name,
                               const rapidjson::Value *params)
{
   std::string cmd = name;
   cmd += ':';
   if (params && params->IsObject())
   {
      for (auto it = params->MemberBegin(); it != params->MemberEnd(); ++it)
      {
         cmd += ' ';
         cmd.append(it->name.GetString(), it->name.GetStringLength());
         cmd += '=';
         cmd += QuoteParamValue(ScalarToString(it->value));
      }
   }
   return cmd;
}

std::string SerializeDoc(const rapidjson::Document &doc)
{
   rapidjson::StringBuffer sb;
   rapidjson::Writer<rapidjson::StringBuffer> w(sb);
   doc.Accept(w);
   return std::string(sb.GetString(), sb.GetSize());
}

std::string MakeError(const rapidjson::Value *id, int code,
                      const std::string &message)
{
   using namespace rapidjson;
   Document doc;
   doc.SetObject();
   auto &a = doc.GetAllocator();
   doc.AddMember("jsonrpc", "2.0", a);
   if (id && !id->IsNull())
   {
      Value idCopy;
      idCopy.CopyFrom(*id, a);
      doc.AddMember("id", idCopy, a);
   }
   else
      doc.AddMember("id", Value(), a);
   Value err(kObjectType);
   err.AddMember("code", code, a);
   err.AddMember("message", Value(message.c_str(), a), a);
   doc.AddMember("error", err, a);
   return SerializeDoc(doc);
}

// Build one tool's descriptor: {name, description, inputSchema}.
rapidjson::Value MakeTool(const char *name, const char *description,
                          rapidjson::Value schema,
                          rapidjson::Document::AllocatorType &a)
{
   using namespace rapidjson;
   Value t(kObjectType);
   t.AddMember("name", Value(name, a), a);
   t.AddMember("description", Value(description, a), a);
   t.AddMember("inputSchema", schema, a);
   return t;
}

rapidjson::Value ToolsList(rapidjson::Document::AllocatorType &a)
{
   using namespace rapidjson;
   Value tools(kArrayType);

   {
      Value schema(kObjectType);
      schema.AddMember("type", "object", a);
      Value props(kObjectType);
      Value typeProp(kObjectType);
      typeProp.AddMember("type", "string", a);
      typeProp.AddMember(
         "description",
         "Catalogue to return. Defaults to 'Menus' (fast: menu tree with "
         "command IDs, no parameter schemas). Use 'Commands' only when "
         "full parameter schemas are needed — it instantiates every "
         "registered effect (Audio Units, VSTs, LADSPA, …) to introspect "
         "them, which blocks Audacity's UI thread for many seconds on "
         "first call and can trigger plugin rescans.", a);
      Value enumArr(kArrayType);
      for (const char *e : {"Menus", "Commands", "Preferences",
                            "Boxes", "Tracks", "Clips", "Envelopes",
                            "Labels", "Selection"})
         enumArr.PushBack(Value(e, a), a);
      typeProp.AddMember("enum", enumArr, a);
      props.AddMember("type", typeProp, a);
      schema.AddMember("properties", props, a);

      tools.PushBack(
         MakeTool(
            "audacity_list_commands",
            "Return the catalogue of scripting / macro commands supported "
            "by this Audacity instance. The reply is the raw JSON response "
            "from Audacity's GetInfo command. Defaults to a cheap 'Menus' "
            "listing; pass type='Commands' for the expensive full schema.",
            std::move(schema), a),
         a);
   }

   {
      Value schema(kObjectType);
      schema.AddMember("type", "object", a);
      Value props(kObjectType);

      Value nameProp(kObjectType);
      nameProp.AddMember("type", "string", a);
      nameProp.AddMember(
         "description",
         "Command identifier, e.g. 'Select' or 'Export2'.", a);
      props.AddMember("name", nameProp, a);

      Value paramsProp(kObjectType);
      paramsProp.AddMember("type", "object", a);
      paramsProp.AddMember(
         "description",
         "Parameter map. Values are coerced to strings and passed as "
         "key=value pairs; nested objects or arrays are serialised as "
         "compact JSON before quoting.", a);
      paramsProp.AddMember("additionalProperties", true, a);
      props.AddMember("params", paramsProp, a);

      schema.AddMember("properties", props, a);
      Value required(kArrayType);
      required.PushBack("name", a);
      schema.AddMember("required", required, a);

      tools.PushBack(
         MakeTool(
            "audacity_run_command",
            "Execute an Audacity scripting command by name. Equivalent to "
            "sending 'Name: k1=\"v1\" k2=\"v2\"' through mod-script-pipe. "
            "Use audacity_list_commands to discover available commands "
            "and their parameters.",
            std::move(schema), a),
         a);
   }

   return tools;
}

} // namespace

struct MCPServer::Impl
{
   std::unique_ptr<httplib::Server> server;
};

MCPServer& MCPServer::Instance()
{
   static MCPServer inst;
   return inst;
}

wxString MCPServer::DispatchAudacityCommand(const wxString &cmdString)
{
   tpExecScriptServerFunc fn;
   {
      std::lock_guard<std::mutex> lock(mFnMutex);
      fn = mExecFn;
   }
   if (!fn)
      return {};

   wxString in  = cmdString;
   wxString out;
   fn(&in, &out);
   return out;
}

void MCPServer::Stop()
{
   if (!mImpl)
      return;
   if (mImpl->server)
      mImpl->server->stop();
   // We don't join: the server runs on ScriptCommandRelay's detached relay
   // thread, so it goes away with the process. Leave mServing true so that
   // the relay loop parks on re-entry rather than rebinding the port.
}

void MCPServer::Serve(tpExecScriptServerFunc pFn)
{
   {
      std::lock_guard<std::mutex> lock(mFnMutex);
      mExecFn = pFn;
   }

   if (mServing.exchange(true))
   {
      // Relay loop re-entered after the server stopped. Park here so the
      // while(true) in StartScriptServer doesn't busy-spin or rebind.
      for (;;)
         std::this_thread::sleep_for(std::chrono::hours(1));
   }

   mImpl = std::make_unique<Impl>();
   mImpl->server = std::make_unique<httplib::Server>();

   mImpl->server->Post("/mcp",
      [this](const httplib::Request &req, httplib::Response &res) {
         HandleMCPRequest(req, res);
      });

   mImpl->server->Get("/mcp",
      [](const httplib::Request &, httplib::Response &res) {
         res.status = 405;
         res.set_header("Allow", "POST");
         res.set_content(R"({"error":"Use POST /mcp"})", "application/json");
      });

   const int port = ResolvePort();
   // Blocks until stop(). If bind fails we simply fall out of Serve and
   // let the relay loop park us on the next iteration.
   mImpl->server->listen(kDefaultHost, port);
}

void MCPServer::HandleMCPRequest(const httplib::Request &req,
                                 httplib::Response &res)
{
   using namespace rapidjson;

   Document doc;
   doc.Parse(req.body.data(), req.body.size());
   if (doc.HasParseError())
   {
      res.status = 400;
      res.set_content(MakeError(nullptr, kParseError, "Invalid JSON"),
                      "application/json");
      return;
   }

   const Value *id = doc.HasMember("id") ? &doc["id"] : nullptr;

   if (!doc.IsObject() || !doc.HasMember("method") ||
       !doc["method"].IsString())
   {
      res.status = 400;
      res.set_content(MakeError(id, kInvalidRequest, "Missing method"),
                      "application/json");
      return;
   }

   const std::string method = doc["method"].GetString();
   const Value *params = doc.HasMember("params") ? &doc["params"] : nullptr;

   // Notifications (no id) expect no response body.
   if (!id || id->IsNull())
   {
      res.status = 204;
      return;
   }

   auto writeResult = [&](auto &&buildResult) {
      Document out;
      out.SetObject();
      auto &a = out.GetAllocator();
      out.AddMember("jsonrpc", "2.0", a);
      Value idCopy;
      idCopy.CopyFrom(*id, a);
      out.AddMember("id", idCopy, a);
      Value result(kObjectType);
      buildResult(result, a);
      out.AddMember("result", result, a);
      res.status = 200;
      res.set_content(SerializeDoc(out), "application/json");
   };

   if (method == "initialize")
   {
      writeResult([](Value &result, Document::AllocatorType &a) {
         result.AddMember("protocolVersion", Value(kProtocolVersion, a), a);
         Value caps(kObjectType);
         caps.AddMember("tools", Value(kObjectType), a);
         result.AddMember("capabilities", caps, a);
         Value info(kObjectType);
         info.AddMember("name",    Value(kServerName,    a), a);
         info.AddMember("version", Value(kServerVersion, a), a);
         result.AddMember("serverInfo", info, a);
      });
      return;
   }

   if (method == "ping")
   {
      writeResult([](Value &, Document::AllocatorType &) {});
      return;
   }

   if (method == "tools/list")
   {
      writeResult([](Value &result, Document::AllocatorType &a) {
         Value tools = ToolsList(a);
         result.AddMember("tools", tools, a);
      });
      return;
   }

   if (method == "tools/call")
   {
      if (!params || !params->IsObject() || !params->HasMember("name") ||
          !(*params)["name"].IsString())
      {
         res.status = 200;
         res.set_content(
            MakeError(id, kInvalidParams,
                      "tools/call requires {name, arguments?}"),
            "application/json");
         return;
      }

      const std::string toolName = (*params)["name"].GetString();
      const Value *args = params->HasMember("arguments")
                            ? &(*params)["arguments"] : nullptr;

      std::string audacityCmd;
      if (toolName == "audacity_list_commands")
      {
         // Default to 'Menus' because 'Commands' forces every registered
         // effect plugin to be instantiated for schema introspection,
         // which blocks the GUI thread for seconds and can trigger an
         // AU/VST rescan on macOS.
         std::string type = "Menus";
         if (args && args->IsObject() && args->HasMember("type") &&
             (*args)["type"].IsString())
            type = (*args)["type"].GetString();
         audacityCmd = "GetInfo: Type=" + type + " Format=JSON";
      }
      else if (toolName == "audacity_run_command")
      {
         if (!args || !args->IsObject() || !args->HasMember("name") ||
             !(*args)["name"].IsString())
         {
            res.status = 200;
            res.set_content(
               MakeError(id, kInvalidParams,
                         "audacity_run_command requires {name, params?}"),
               "application/json");
            return;
         }
         const std::string cmdName = (*args)["name"].GetString();
         const Value *cmdParams = args->HasMember("params")
                                    ? &(*args)["params"] : nullptr;
         audacityCmd = BuildCommandString(cmdName, cmdParams);
      }
      else
      {
         res.status = 200;
         res.set_content(
            MakeError(id, kMethodNotFound, "Unknown tool: " + toolName),
            "application/json");
         return;
      }

      const wxString response =
         DispatchAudacityCommand(wxString::FromUTF8(audacityCmd.c_str()));
      const std::string responseUtf8(response.utf8_str());

      writeResult([&](Value &result, Document::AllocatorType &a) {
         Value content(kArrayType);
         Value entry(kObjectType);
         entry.AddMember("type", "text", a);
         entry.AddMember("text", Value(responseUtf8.c_str(), a), a);
         content.PushBack(entry, a);
         result.AddMember("content", content, a);
         result.AddMember("isError", false, a);
      });
      return;
   }

   res.status = 200;
   res.set_content(
      MakeError(id, kMethodNotFound, "Unknown method: " + method),
      "application/json");
}
