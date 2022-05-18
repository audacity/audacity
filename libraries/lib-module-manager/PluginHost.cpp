/**********************************************************************

  Audacity: A Digital Audio Editor

  @file PluginHost.cpp

  @author Vitaly Sverchinsky

  Part of lib-module-manager library

**********************************************************************/

#include "PluginHost.h"

#include <wx/app.h>
#include <wx/log.h>
#include <wx/module.h>
#include <wx/process.h>

#include "BasicUI.h"
#include "PathList.h"
#include "FileNames.h"
#include "ModuleManager.h"
#include "IPCClient.h"
#include "PlatformCompatibility.h"
#include "PluginManager.h"

namespace
{
   //Attempts to instantiate plugin module and put plugin descriptors into result
   void Discover(detail::PluginValidationResult& result, const wxString& providerId, const wxString& pluginPath)
   {
      try
      {
         if(auto provider = ModuleManager::Get().CreateProviderInstance(providerId, wxEmptyString))
         {
            TranslatableString errorMessage{};

            provider->DiscoverPluginsAtPath(pluginPath, errorMessage, [&result](PluginProvider *provider, ComponentInterface *ident)
            {
               //Workaround: use DefaultRegistrationCallback to create all descriptors for us
               //and then put a copy into result
               auto id = PluginManager::DefaultRegistrationCallback(provider, ident);
               if(const auto desc = PluginManager::Get().GetPlugin(id))
                  result.Add(PluginDescriptor { *desc });
               return id;
            });
            if(!errorMessage.empty())
               result.SetError(errorMessage.Debug());
         }
         else
            result.SetError("provider not found");
      }
      catch(...)
      {
         result.SetError("unknown error");
      }
   }
}

PluginHost::PluginHost()
{
   FileNames::InitializePathList();

   wxFileName configFileName{ FileNames::Configuration() };
   auto pConfig = std::make_unique<FileConfig>(
      AppName, wxEmptyString, configFileName.GetFullPath(),
      wxEmptyString, wxCONFIG_USE_LOCAL_FILE);
   pConfig->Init();
   InitPreferences(std::move(pConfig));

   auto& moduleManager = ModuleManager::Get();
   moduleManager.Initialize();
   moduleManager.DiscoverProviders();

   mClient = std::make_unique<IPCClient>(*this);
}

void PluginHost::OnConnect(IPCChannel& channel) noexcept
{
   std::lock_guard lck(mSync);
   mChannel = &channel;
}

void PluginHost::OnDisconnect() noexcept
{
   Stop();
}

void PluginHost::OnConnectionError() noexcept
{
   Stop();
}

void PluginHost::OnDataAvailable(const void* data, size_t size) noexcept
{
   try
   {
      mInputMessageReader.ConsumeBytes(data, size);
      if(mInputMessageReader.CanPop())
      {
         {
            std::lock_guard lck(mSync);
            assert(!mRequest);
            mRequest = mInputMessageReader.Pop();
         }
         mRequestCondition.notify_one();
      }
   }
   catch(...)
   {
      Stop();
   }
}

bool PluginHost::Serve()
{
   std::unique_lock lck(mSync);
   mRequestCondition.wait(lck, [this]{ return !mRunning || mRequest.has_value(); });

   if(!mRunning)
      return false;

   if(mRequest)
   {
      std::optional<wxString> request;
      mRequest.swap(request);

      lck.unlock();
      
      wxString providerId;
      wxString pluginPath;
      detail::PluginValidationResult result;
      if(detail::ParseRequestString(*request, providerId, pluginPath))
         Discover(result, providerId, pluginPath);
      else
         result.SetError("malformed request string");

      XMLStringWriter xmlWriter;
      result.WriteXML(xmlWriter);

      lck.lock();
      if(mChannel)
         detail::PutMessage(*mChannel, xmlWriter);
   }

   return true;
}


void PluginHost::Stop() noexcept
{
   try
   {
      {
         std::lock_guard lck(mSync);//may throw
         mRunning = false;
         mChannel = nullptr;
      }
   }
   catch(...)
   {
      //If something went wrong with mutex locking we'll try to
      //awake main thread if it's blocked on condition variable.
      //Attempt to relock the mutex there should throw as well(?)...
      //which, in turn, will result in std::terminate being called
   }
   mRequestCondition.notify_one();
}

bool PluginHost::Start()
{
   const auto cmd = wxString::Format("\"%s\" %s", PlatformCompatibility::GetExecutablePath(), PluginHost::HostArgument);

   auto process = std::make_unique<wxProcess>();
   process->Detach();
   if(wxExecute(cmd, wxEXEC_ASYNC, process.get()) != 0)
   {
      //process will delete itself upon termination
      process.release();
      return true;
   }
   return false;
}

bool PluginHost::IsHostProcess()
{
   return wxTheApp && wxTheApp->argc >= 2 && wxStrcmp(wxTheApp->argv[1], HostArgument) == 0;
}

class PluginHostModule final :
   public wxModule
{
public:
   DECLARE_DYNAMIC_CLASS(PluginHostModule)

   bool OnInit() override
   {
      if(PluginHost::IsHostProcess())
      {
         //log messages will appear in a separate window
         //redirect to log file later
         wxLog::EnableLogging(false);

         //Handle requests...
         PluginHost host;
         while(host.Serve()) { }
         //...and terminate app
         return false;
      }
      //do noting if current process isn't a host process
      return true;
   }
   
   void OnExit() override
   {

   }
};
IMPLEMENT_DYNAMIC_CLASS(PluginHostModule, wxModule);
