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

   // IdentInterface implementatino

   wxString GetPath() override;
   wxString GetSymbol() override;
   wxString GetName() override;
   wxString GetVendor() override;
   wxString GetVersion() override;
   wxString GetDescription() override;

   // ModuleInterface implementation

   bool Initialize() override;
   void Terminate() override;

   bool AutoRegisterPlugins(PluginManagerInterface & pm) override;
   wxArrayString FindPlugins(PluginManagerInterface & pm) override;
   bool RegisterPlugin(PluginManagerInterface & pm, const wxString & path) override;

   bool IsPluginValid(const wxString & path) override;

   IdentInterface *CreateInstance(const wxString & path) override;
   void DeleteInstance(IdentInterface *instance) override;

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