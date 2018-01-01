/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadEffects.h

  Dominic Mazzoni

**********************************************************************/

#include "audacity/ModuleInterface.h"
#include "audacity/EffectInterface.h"
#include "audacity/PluginInterface.h"

#include "Effect.h"
#include "../MemoryX.h"

///////////////////////////////////////////////////////////////////////////////
//
// BuiltinEffectsModule
//
///////////////////////////////////////////////////////////////////////////////

class BuiltinEffectsModule final : public ModuleInterface
{
public:
   BuiltinEffectsModule(ModuleManagerInterface *moduleManager, const wxString *path);
   virtual ~BuiltinEffectsModule();

   // IdentInterface implementation

   wxString GetPath() override;
   wxString GetSymbol() override;
   wxString GetName() override;
   wxString GetVendor() override;
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

   IdentInterface *CreateInstance(const wxString & path) override;
   void DeleteInstance(IdentInterface *instance) override;

private:
   // BuiltinEffectModule implementation

   std::unique_ptr<Effect> Instantiate(const wxString & path);

private:
   ModuleManagerInterface *mModMan;
   wxString mPath;

   wxArrayString mNames;
};
