/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadVamp.h

  Chris Cannam

**********************************************************************/

#include "../../Audacity.h"

#if defined(USE_VAMP)

#include "audacity/ModuleInterface.h"
#include "audacity/EffectInterface.h"
#include "audacity/PluginInterface.h"

///////////////////////////////////////////////////////////////////////////////
//
// VampEffectsModule
//
///////////////////////////////////////////////////////////////////////////////

class VampEffectsModule final : public ModuleInterface
{
public:
   VampEffectsModule(ModuleManagerInterface *moduleManager, const wxString *path);
   virtual ~VampEffectsModule();

   // ComponentInterface implementation

   wxString GetPath() override;
   ComponentInterfaceSymbol GetSymbol() override;
   ComponentInterfaceSymbol GetVendor() override;
   wxString GetVersion() override;
   wxString GetDescription() override;

   // ModuleInterface implementation

   bool Initialize() override;
   void Terminate() override;

   wxArrayString FileExtensions() override { return {}; }
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
   // VampEffectModule implementation

   std::unique_ptr<Vamp::Plugin> FindPlugin(const wxString & wpath,
                            int & output,
                            bool & hasParameters);

private:
   ModuleManagerInterface *mModMan;
   wxString mPath;
};

#endif
