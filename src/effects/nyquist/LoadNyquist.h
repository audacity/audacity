/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadNyquist.h

  Dominic Mazzoni

**********************************************************************/

#include "ModuleInterface.h"
#include "EffectInterface.h"
#include "PluginInterface.h"

///////////////////////////////////////////////////////////////////////////////
//
// NyquistEffectsModule
//
///////////////////////////////////////////////////////////////////////////////

class NyquistEffectsModule final : public ModuleInterface
{
public:
   NyquistEffectsModule();
   virtual ~NyquistEffectsModule();

   // ComponentInterface implementation

   PluginPath GetPath() override;
   ComponentInterfaceSymbol GetSymbol() override;
   VendorSymbol GetVendor() override;
   wxString GetVersion() override;
   TranslatableString GetDescription() override;

   // ModuleInterface implementation

   bool Initialize() override;
   void Terminate() override;
   EffectFamilySymbol GetOptionalFamilySymbol() override;


   const FileExtensions &GetFileExtensions() override;
   FilePath InstallPath() override;

   bool AutoRegisterPlugins(PluginManagerInterface & pm) override;
   PluginPaths FindPluginPaths(PluginManagerInterface & pm) override;
   unsigned DiscoverPluginsAtPath(
      const PluginPath & path, TranslatableString &errMsg,
      const RegistrationCallback &callback)
         override;

   bool IsPluginValid(const PluginPath & path, bool bFast) override;

   std::unique_ptr<ComponentInterface>
      CreateInstance(const PluginPath & path) override;
};
