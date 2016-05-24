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

   // NyquistEffectModule implementation

private:
   ModuleManagerInterface *mModMan;
   wxString mPath;
};
