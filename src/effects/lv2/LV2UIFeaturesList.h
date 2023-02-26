/**********************************************************************

  Audacity: A Digital Audio Editor

  @file LV2UIFeaturesList.h

  Paul Licameli split from LV2Effect.h

  Audacity(R) is copyright (c) 1999-2013 Audacity Team.
  License: GPL v2 or later.  See License.txt.

*********************************************************************/

#ifndef __AUDACITY_LV2_UI_FEATURES_LIST__
#define __AUDACITY_LV2_UI_FEATURES_LIST__

#if USE_LV2

#include "LV2FeaturesList.h"
#include "lv2_external_ui.h"
#include <suil/suil.h>
#include "lv2/data-access/data-access.h"

struct LV2WrapperFeaturesList;
class wxWindow;

struct LV2UIFeaturesList final : ExtendedLV2FeaturesList {
   //! Abstraction of host services that a plug-ins native UI needs
   struct UIHandler {
      virtual ~UIHandler();
      virtual int ui_resize(int width, int height) = 0;
      virtual void ui_closed() = 0;
      virtual void suil_port_write(uint32_t port_index, uint32_t buffer_size,
         uint32_t protocol, const void *buffer) = 0;
      virtual uint32_t suil_port_index(const char *port_symbol) = 0;
   };

   LV2UIFeaturesList(
      const LV2WrapperFeaturesList &baseFeatures, UIHandler *pHandler,
      const LilvNode *node,
      LilvInstance *pInstance = nullptr, wxWindow *pParent = nullptr);

private:
   //! @return success
   bool InitializeFeatures(const LilvNode *node,
      LilvInstance *pInstance, wxWindow *pParent);

public:
   // publicize
   using ExtendedLV2FeaturesList::mFeatures;

   UIHandler *const mpHandler;

   /*! @name static functions that dispatch to a UIHandler
    @{
    */
   static int ui_resize(LV2UI_Feature_Handle handle, int width, int height);
   static void ui_closed(LV2UI_Controller controller);
   static uint32_t suil_port_index(
      SuilController controller, const char *port_symbol);
   static void suil_port_write(SuilController controller,
      uint32_t port_index, uint32_t buffer_size, uint32_t protocol,
      const void *buffer);
   //! @}

   // These objects contain C-style virtual function tables that we fill in
   const LV2UI_Resize mUIResizeFeature{
      mpHandler, LV2UIFeaturesList::ui_resize };
   // Not const, filled in when making a dialog
   LV2_Extension_Data_Feature mExtensionDataFeature{};

   const LilvNodePtr mHumanId{ lilv_plugin_get_name(&mPlug) };
   const LV2_External_UI_Host mExternalUIHost{
      // The void* bound to the argument of ui_closed will be the same
      // given to suil_instance_new
      LV2UIFeaturesList::ui_closed, lilv_node_as_string(mHumanId.get()) };

   const bool mOk;
};

#endif
#endif
