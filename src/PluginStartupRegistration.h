/**********************************************************************

  Audacity: A Digital Audio Editor

  @file PluginStartupRegistration.h

  @author Vitaly Sverchinsky

**********************************************************************/

#pragma once

#include <vector>
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
   const std::vector<std::pair<wxString, wxString>> mPluginsToProcess;
   size_t mCurrentPluginIndex{0};
   std::vector<wxString> mFailedPlugins;
public:

   PluginStartupRegistration(std::vector<std::pair<wxString, wxString>> pluginsToProcess);

   ///Starts validation, showing dialog that blocks execution until
   ///process is complete or canceled
   void Run();

   ///Returns list of plugins that didn't pass validation for some reason
   const std::vector<wxString>& GetFailedPlugins() const noexcept;

   void OnInternalError(const wxString& error) override;
   void OnPluginFound(const PluginDescriptor& desc) override;
   void OnValidationFinished() override;

private:
   
   void Stop();
   void StopWithError(const wxString& msg);
   void ProcessNext();
};
