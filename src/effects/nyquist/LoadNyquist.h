/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadNyquist.h

  Dominic Mazzoni

**********************************************************************/

#include "audacity/ModuleInterface.h"
#include "audacity/EffectInterface.h"
#include "audacity/PluginInterface.h"

///////////////////////////////////////////////////////////////////////////////
//
// NyquistEffectsModule
//
///////////////////////////////////////////////////////////////////////////////

class NyquistEffectsModule final : public ModuleInterface
{
public:
   NyquistEffectsModule(ModuleManagerInterface *moduleManager, const wxString *path);
   virtual ~NyquistEffectsModule();

   // ComponentInterface implementation

   wxString GetPath() override;
   ComponentInterfaceSymbol GetSymbol() override;
   ComponentInterfaceSymbol GetVendor() override;
   wxString GetVersion() override;
   wxString GetDescription() override;

   // ModuleInterface implementation

   bool Initialize() override;
   void Terminate() override;

   wxArrayString FileExtensions() override;
   wxString InstallPath() override;

   bool AutoRegisterPlugins(PluginManagerInterface & pm) override;
   wxArrayString FindPluginPaths(PluginManagerInterface & pm) override;
   unsigned DiscoverPluginsAtPath(
      const wxString & path, wxString &errMsg,
      const RegistrationCallback &callback)
         override;

   bool IsPluginValid(const wxString & path, bool bFast) override;

   ComponentInterface *CreateInstance(const wxString & path) override;
   void DeleteInstance(ComponentInterface *instance) override;

   // NyquistEffectModule implementation

private:
   ModuleManagerInterface *mModMan;
   wxString mPath;
};
