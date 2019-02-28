/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadCommands.h

  Dominic Mazzoni
  James Crook

**********************************************************************/

#include "audacity/ModuleInterface.h"
#include "audacity/EffectInterface.h"
#include "audacity/PluginInterface.h"

#include "AudacityCommand.h"
#include "../MemoryX.h"

///////////////////////////////////////////////////////////////////////////////
//
// BuiltinCommandsModule
//
///////////////////////////////////////////////////////////////////////////////

class BuiltinCommandsModule final : public ModuleInterface
{
public:
   BuiltinCommandsModule(ModuleManagerInterface *moduleManager, const wxString *path);
   virtual ~BuiltinCommandsModule();

   // ComponentInterface implementation

   wxString GetPath() override;
   ComponentInterfaceSymbol GetSymbol() override;
   VendorSymbol GetVendor() override;
   wxString GetVersion() override;
   wxString GetDescription() override;

   // ModuleInterface implementation

   bool Initialize() override;
   void Terminate() override;

   wxArrayString GetFileExtensions() override { return {}; }
   wxString InstallPath() override { return {}; }

   bool AutoRegisterPlugins(PluginManagerInterface & pm) override;
   wxArrayString FindPluginPaths(PluginManagerInterface & pm) override;
   unsigned DiscoverPluginsAtPath(
      const wxString & path, wxString &errMsg,
      const RegistrationCallback &callback)
         override;

   bool IsPluginValid(const wxString & path, bool bFast) override;

   ComponentInterface *CreateInstance(const wxString & path) override;
   void DeleteInstance(ComponentInterface *instance) override;

private:
   // BuiltinEffectModule implementation

   std::unique_ptr<AudacityCommand> Instantiate(const wxString & path);

private:
   ModuleManagerInterface *mModMan;
   wxString mPath;

   wxArrayString mNames;
};
