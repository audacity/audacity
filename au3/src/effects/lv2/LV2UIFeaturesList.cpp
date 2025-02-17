/**********************************************************************

  Audacity: A Digital Audio Editor

  @file LV2UIFeaturesList.cpp

  Paul Licameli split from LV2Effect.cpp

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2 or later.  See License.txt.

**********************************************************************/

#include "LV2UIFeaturesList.h"
#include "LV2InstanceFeaturesList.h"
#include "lv2/instance-access/instance-access.h"
#include <wx/window.h>

LV2UIFeaturesList::LV2UIFeaturesList(
    const LV2WrapperFeaturesList& baseFeatures, UIHandler* pHandler,
    const LilvNode* node, LilvInstance* pInstance, wxWindow* pParent)
    : ExtendedLV2FeaturesList{WithBase, baseFeatures}
    , mpHandler{pHandler}
    , mOk{InitializeFeatures(node, pInstance, pParent)}
{
}

bool LV2UIFeaturesList::InitializeFeatures(const LilvNode* node,
                                           LilvInstance* pInstance, wxWindow* pParent)
{
    AddFeature(LV2_UI__resize, &mUIResizeFeature);
    mExtensionDataFeature = { pInstance
                              ? lilv_instance_get_descriptor(pInstance)->extension_data
                              : nullptr
    };
    AddFeature(LV2_DATA_ACCESS_URI, &mExtensionDataFeature);
    AddFeature(LV2_EXTERNAL_UI__Host, &mExternalUIHost);
    AddFeature(LV2_EXTERNAL_UI_DEPRECATED_URI, &mExternalUIHost);
    AddFeature(LV2_INSTANCE_ACCESS_URI,
               pInstance ? lilv_instance_get_handle(pInstance) : nullptr);
    AddFeature(LV2_UI__parent, pParent ? pParent->GetHandle() : nullptr);
    return ValidateFeatures(node);
}

int LV2UIFeaturesList::ui_resize(LV2UI_Feature_Handle handle,
                                 int width, int height)
{
    return static_cast<UIHandler*>(handle)->ui_resize(width, height);
}

void LV2UIFeaturesList::ui_closed(LV2UI_Controller controller)
{
    return static_cast<UIHandler*>(controller)->ui_closed();
}

uint32_t LV2UIFeaturesList::suil_port_index(
    SuilController controller, const char* port_symbol)
{
    return static_cast<UIHandler*>(controller)->suil_port_index(port_symbol);
}

void LV2UIFeaturesList::suil_port_write(SuilController controller,
                                        uint32_t port_index, uint32_t buffer_size, uint32_t protocol,
                                        const void* buffer)
{
    return static_cast<UIHandler*>(controller)
           ->suil_port_write(port_index, buffer_size, protocol, buffer);
}

LV2UIFeaturesList::UIHandler::~UIHandler() = default;

static LV2InstanceFeaturesList::ValidatePlugin::Scope scope{
    [](const LilvPlugin& plug, LV2InstanceFeaturesList& instanceFeatures){
        return LV2UIFeaturesList{ LV2WrapperFeaturesList{ instanceFeatures },
                                  nullptr, lilv_plugin_get_uri(&plug)
        }.mOk;
    }
};
