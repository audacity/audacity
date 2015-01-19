/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadVamp.h

  Chris Cannam

**********************************************************************/

#include "audacity/ModuleInterface.h"
#include "audacity/EffectInterface.h"
#include "audacity/PluginInterface.h"

///////////////////////////////////////////////////////////////////////////////
//
// VampEffectsModule
//
///////////////////////////////////////////////////////////////////////////////

class VampEffectsModule : public ModuleInterface
{
public:
   VampEffectsModule(ModuleManagerInterface *moduleManager, const wxString *path);
   virtual ~VampEffectsModule();

   // IdentInterface implementatino

   virtual wxString GetPath();
   virtual wxString GetSymbol();
   virtual wxString GetName();
   virtual wxString GetVendor();
   virtual wxString GetVersion();
   virtual wxString GetDescription();

   // ModuleInterface implementation

   virtual bool Initialize();
   virtual void Terminate();

   virtual bool AutoRegisterPlugins(PluginManagerInterface & pm);
   virtual wxArrayString FindPlugins(PluginManagerInterface & pm);
   virtual bool RegisterPlugin(PluginManagerInterface & pm, const wxString & path);

   virtual bool IsPluginValid(const wxString & path);

   virtual IdentInterface *CreateInstance(const wxString & path);
   virtual void DeleteInstance(IdentInterface *instance);

   // VampEffectModule implementation

private:
   ModuleManagerInterface *mModMan;
   wxString mPath;
};
