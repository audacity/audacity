/**********************************************************************

  Audacity: A Digital Audio Editor

  @file LV2UIFeaturesList.cpp

  Paul Licameli split from LV2Effect.cpp

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2 or later.  See License.txt.

**********************************************************************/

#include "LV2UIFeaturesList.h"
#include "lv2/instance-access/instance-access.h"

LV2UIFeaturesList::LV2UIFeaturesList(
   const LV2FeaturesListBase &baseFeatures, UIHandler &handler
)  : ExtendedLV2FeaturesList{ baseFeatures }
   , mHandler{ handler }
{
}

bool LV2UIFeaturesList::InitializeFeatures()
{
   // To be set up later when making a dialog:
   mExtensionDataFeature = {};

   AddFeature(LV2_UI__resize, &mUIResizeFeature);
   AddFeature(LV2_DATA_ACCESS_URI, &mExtensionDataFeature);
   AddFeature(LV2_EXTERNAL_UI__Host, &mExternalUIHost);
   AddFeature(LV2_EXTERNAL_UI_DEPRECATED_URI, &mExternalUIHost);
   // Two features must be filled in later
   mInstanceAccessFeature = mFeatures.size();
   AddFeature(LV2_INSTANCE_ACCESS_URI, nullptr);
   mParentFeature = mFeatures.size();
   AddFeature(LV2_UI__parent, nullptr);
   return ValidateFeatures(lilv_plugin_get_uri(&mPlug));
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
   SuilController controller, const char *port_symbol)
{
   return static_cast<UIHandler *>(controller)->suil_port_index(port_symbol);
}

void LV2UIFeaturesList::suil_port_write(SuilController controller,
   uint32_t port_index, uint32_t buffer_size, uint32_t protocol,
      const void *buffer)
{
   return static_cast<UIHandler *>(controller)
      ->suil_port_write(port_index, buffer_size, protocol, buffer);
}

LV2UIFeaturesList::UIHandler::~UIHandler() = default;
