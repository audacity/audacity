/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadLV2.h

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

*********************************************************************/

#ifndef LV2EFFECTSMODULE_H
#define LV2EFFECTSMODULE_H

#include "../../MemoryX.h"
#include <wx/hashmap.h>

#include <lilv/lilv.h>

#include "lv2/lv2plug.in/ns/lv2core/lv2.h"
#include "lv2/lv2plug.in/ns/ext/data-access/data-access.h"
#include "lv2/lv2plug.in/ns/ext/instance-access/instance-access.h"
#include "lv2/lv2plug.in/ns/ext/port-groups/port-groups.h"
#include "lv2/lv2plug.in/ns/ext/port-props/port-props.h"
#include "lv2/lv2plug.in/ns/ext/presets/presets.h"
#include "lv2/lv2plug.in/ns/ext/uri-map/uri-map.h"
#include "lv2/lv2plug.in/ns/extensions/units/units.h"

#include "audacity/ModuleInterface.h"
#include "audacity/EffectInterface.h"
#include "audacity/PluginInterface.h"

#undef URI
#define URI(n, u)

// Define the list of URIs that we will use
#undef URILIST
#define URILIST \
   URI( gBool,          LV2_ATOM__Bool                ) \
   URI( gDouble,        LV2_ATOM__Double              ) \
   URI( gFloat,         LV2_ATOM__Float               ) \
   URI( gInt,           LV2_ATOM__Int                 ) \
   URI( gLong,          LV2_ATOM__Long                ) \
   URI( gAudio,         LV2_CORE__AudioPort           ) \
   URI( gControl,       LV2_CORE__ControlPort         ) \
   URI( gInput,         LV2_CORE__InputPort           ) \
   URI( gInstrument,    LV2_CORE__InstrumentPlugin    ) \
   URI( gOutput,        LV2_CORE__OutputPort          ) \
   URI( gOptional,      LV2_CORE__connectionOptional  ) \
   URI( gEnumeration,   LV2_CORE__enumeration         ) \
   URI( gInteger,       LV2_CORE__integer             ) \
   URI( gName,          LV2_CORE__name                ) \
   URI( gLatency,       LV2_CORE__reportsLatency      ) \
   URI( gSampleRate,    LV2_CORE__sampleRate          ) \
   URI( gToggled,       LV2_CORE__toggled             ) \
   URI( gGroup,         LV2_PORT_GROUPS__group        ) \
   URI( gSubGroupOf,    LV2_PORT_GROUPS__subGroupOf   ) \
   URI( gLogarithmic,   LV2_PORT_PROPS__logarithmic   ) \
   URI( gTrigger,       LV2_PORT_PROPS__trigger       ) \
   URI( gPreset,        LV2_PRESETS__Preset           ) \
   URI( gUnit,          LV2_UNITS__unit               ) \
   URI( gUnitSymbol,    LV2_UNITS__symbol             ) \
   URI( gLabel,         LILV_NS_RDFS "label"          )

///////////////////////////////////////////////////////////////////////////////
//
// LV2EffectsModule
//
///////////////////////////////////////////////////////////////////////////////

class LV2EffectsModule final : public ModuleInterface
{
public:
   LV2EffectsModule(ModuleManagerInterface *moduleManager, const wxString *path);
   virtual ~LV2EffectsModule();

   // ComponentInterface implementation

   PluginPath GetPath() override;
   ComponentInterfaceSymbol GetSymbol() override;
   VendorSymbol GetVendor() override;
   wxString GetVersion() override;
   wxString GetDescription() override;

   // ModuleInterface implementation

   bool Initialize() override;
   void Terminate() override;

   FileExtensions GetFileExtensions() override { return {}; }
   FilePath InstallPath() override { return {}; }

   bool AutoRegisterPlugins(PluginManagerInterface & pm) override;
   PluginPaths FindPluginPaths(PluginManagerInterface & pm) override;
   unsigned DiscoverPluginsAtPath(
      const PluginPath & path, wxString &errMsg,
      const RegistrationCallback &callback)
         override;

   bool IsPluginValid(const PluginPath & path, bool bFast) override;

   ComponentInterface *CreateInstance(const PluginPath & path) override;
   void DeleteInstance(ComponentInterface *instance) override;

   // LV2EffectModule implementation

private:
   const LilvPlugin *GetPlugin(const PluginPath & path);

private:
   ModuleManagerInterface *mModMan;
   PluginPath mPath;
};

extern LilvWorld *gWorld;

#endif
