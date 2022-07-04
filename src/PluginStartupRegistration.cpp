/**********************************************************************

  Audacity: A Digital Audio Editor

  @file PluginStartupRegistration.cpp

  @author Vitaly Sverchinsky

**********************************************************************/

#include "PluginStartupRegistration.h"

#include <thread>

#include <wx/log.h>
#include <wx/app.h>

#include "PluginManager.h"
#include "PluginDescriptor.h"
#include "widgets/ProgressDialog.h"
#include "widgets/wxWidgetsWindowPlacement.h"


PluginStartupRegistration::PluginStartupRegistration(const std::map<wxString, std::vector<wxString>>& pluginsToProcess)
{
   for(auto& p : pluginsToProcess)
      mPluginsToProcess.push_back(p);
   mValidator = std::make_unique<AsyncPluginValidator>(*this);
}

void PluginStartupRegistration::OnInternalError(const wxString& error)
{
   StopWithError(error);
}

void PluginStartupRegistration::OnPluginFound(const PluginDescriptor& desc)
{
   auto& pluginManager = PluginManager::Get();
   //Multiple providers can report same module paths
   if(desc.GetPluginType() == PluginTypeStub)
      //do not register until all associated providers have tried to load the module
      mFailedPluginsCache.push_back(desc);
   else
   {
      mValidProviderFound = true;
      PluginManager::Get().RegisterPlugin(PluginDescriptor { desc });
   }
}

void PluginStartupRegistration::OnValidationFinished()
{
   ++mCurrentPluginProviderIndex;
   if(mValidProviderFound ||
      mPluginsToProcess[mCurrentPluginIndex].second.size() == mCurrentPluginProviderIndex)
   {
      //we've tried all providers associated with same module path...
      if(!mValidProviderFound && !mFailedPluginsCache.empty())
      {
         //...but none of them succeeded
         mFailedPluginsPaths.push_back(mFailedPluginsCache[0].GetPath());

         for(auto& desc : mFailedPluginsCache)
            PluginManager::Get().RegisterPlugin(std::move(desc));
      }
      ++mCurrentPluginIndex;
      mCurrentPluginProviderIndex = 0;
      mValidProviderFound = false;
      mFailedPluginsCache.clear();
   }
   ProcessNext();
}

const std::vector<wxString>& PluginStartupRegistration::GetFailedPluginsPaths() const noexcept
{
   return mFailedPluginsPaths;
}

void PluginStartupRegistration::Run()
{
   auto dialog = BasicUI::MakeProgress(XO("Searching for plugins"), XO(""));
   ProcessNext();
   while(mValidator)
   {
      const auto message = TranslatableString { mPluginsToProcess[mCurrentPluginIndex].first, { } };
      //Update UI
      if(dialog->Poll(mCurrentPluginIndex, mPluginsToProcess.size(), message) != BasicUI::ProgressResult::Success)
         Stop();
      //AsyncPluginValidator uses event loop for internal message
      //delivery, but ProgressDialog::Poll implementation does not call
      //wxApp::Yield each time, which may result in too long CPU stalls
      if(wxTheApp->HasPendingEvents())
         wxTheApp->Yield();
      else
         //Seems like we do have no events yet, save CPU cycles then
         std::this_thread::sleep_for(std::chrono::milliseconds { 10 });
   }
}

void PluginStartupRegistration::Stop()
{
   mValidator.reset();
   PluginManager::Get().Save();
}

void PluginStartupRegistration::StopWithError(const wxString& msg)
{
   //TODO: show error dialog?
   wxLogError("Plugin registration error: %s", msg);
   Stop();
}

void PluginStartupRegistration::ProcessNext()
{
   if(mCurrentPluginIndex == mPluginsToProcess.size())
   {
      Stop();
      return;
   }

   try
   {
      mValidator->Validate(
         mPluginsToProcess[mCurrentPluginIndex].second[mCurrentPluginProviderIndex],
         mPluginsToProcess[mCurrentPluginIndex].first
      );
   }
   catch(std::exception& e)
   {
      StopWithError(e.what());
   }
   catch(...)
   {
      StopWithError("unknown error");
   }
}
