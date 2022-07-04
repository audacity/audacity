/**********************************************************************

  Audacity: A Digital Audio Editor

  @file PluginStartupRegistration.h

  @author Vitaly Sverchinsky

**********************************************************************/

#pragma once

#include <vector>
#include <map>
#include <memory>
#include <wx/string.h>
#include "AsyncPluginValidator.h"

namespace BasicUI
{
   class ProgressDialog;
}

///Helper class that passes plugins provided in constructor
///to plugin validator, then "good" plugins are registered in
///PluginManager. 
class PluginStartupRegistration final :
   public AsyncPluginValidator::Delegate
{
   std::unique_ptr<AsyncPluginValidator> mValidator;
   std::vector<std::pair<wxString, std::vector<wxString>>> mPluginsToProcess;
   size_t mCurrentPluginIndex{0};
   size_t mCurrentPluginProviderIndex{0};
   bool mValidProviderFound{false};
   std::vector<wxString> mFailedPluginsPaths;
   std::vector<PluginDescriptor> mFailedPluginsCache;
public:

   PluginStartupRegistration(const std::map<wxString, std::vector<wxString>>& pluginsToProcess);

   ///Starts validation, showing dialog that blocks execution until
   ///process is complete or canceled
   void Run();

   ///Returns list of paths of plugins that didn't pass validation for some reason
   const std::vector<wxString>& GetFailedPluginsPaths() const noexcept;

   void OnInternalError(const wxString& error) override;
   void OnPluginFound(const PluginDescriptor& desc) override;
   void OnValidationFinished() override;

private:
   
   void Stop();
   void StopWithError(const wxString& msg);
   void ProcessNext();
};
