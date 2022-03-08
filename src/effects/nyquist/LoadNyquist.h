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

   PluginPath GetPath() const override;
   ComponentInterfaceSymbol GetSymbol() const override;
   VendorSymbol GetVendor() const override;
   wxString GetVersion() const override;
   TranslatableString GetDescription() const override;

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
