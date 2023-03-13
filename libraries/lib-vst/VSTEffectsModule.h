/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTEffectsModule.h

  Dominic Mazzoni

  Paul Licameli split from VSTEffect.h

**********************************************************************/
#if USE_VST

#ifndef __AUDACITY_VST_EFFECTS_MODULE__
#define __AUDACITY_VST_EFFECTS_MODULE__

#include "PluginProvider.h"

class VST_API VSTEffectsModule final : public PluginProvider
{
public:
   VSTEffectsModule();
   virtual ~VSTEffectsModule();

   // ComponentInterface implementation

   PluginPath GetPath() const override;
   ComponentInterfaceSymbol GetSymbol() const override;
   VendorSymbol GetVendor() const override;
   wxString GetVersion() const override;
   TranslatableString GetDescription() const override;

   // PluginProvider implementation

   bool Initialize() override;
   void Terminate() override;
   EffectFamilySymbol GetOptionalFamilySymbol() override;

   const FileExtensions &GetFileExtensions() override;
   FilePath InstallPath() override;

   void AutoRegisterPlugins(PluginManagerInterface & pm) override;
   PluginPaths FindModulePaths(PluginManagerInterface & pm) override;
   unsigned DiscoverPluginsAtPath(
      const PluginPath & path, TranslatableString &errMsg,
      const RegistrationCallback &callback)
         override;
   
   bool CheckPluginExist(const PluginPath& path) const override;

   std::unique_ptr<ComponentInterface>
      LoadPlugin(const PluginPath & path) override;
};

#endif

#endif // USE_VST
