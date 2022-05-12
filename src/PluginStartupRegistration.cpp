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


PluginStartupRegistration::PluginStartupRegistration(std::vector<std::pair<wxString, wxString>> pluginsToProcess)
   : mPluginsToProcess(std::move(pluginsToProcess))
{
   mValidator = std::make_unique<AsyncPluginValidator>(*this);
}

void PluginStartupRegistration::OnInternalError(const wxString& error)
{
   wxLogError(error);
   Stop();
}

void PluginStartupRegistration::OnPluginFound(const PluginDescriptor& desc)
{
   PluginManager::Get().RegisterPlugin(PluginDescriptor { desc });
   if(!desc.IsValid())
      mFailedPlugins.push_back(desc.GetPath());
}

void PluginStartupRegistration::OnValidationFinished()
{
   ++mCurrentPluginIndex;
   ProcessNext();
}

const std::vector<wxString>& PluginStartupRegistration::GetFailedPlugins() const noexcept
{
   return mFailedPlugins;
}

void PluginStartupRegistration::Run()
{
   auto dialog = BasicUI::MakeProgress(XO("Searching for plugins"), XO(""));
   ProcessNext();
   while(mValidator)
   {
      const auto message = TranslatableString { mPluginsToProcess[mCurrentPluginIndex].second, { } };
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

void PluginStartupRegistration::ProcessNext()
{
   if(mCurrentPluginIndex == mPluginsToProcess.size())
      Stop();
   else
      mValidator->Validate(
         mPluginsToProcess[mCurrentPluginIndex].first,
         mPluginsToProcess[mCurrentPluginIndex].second);
}
