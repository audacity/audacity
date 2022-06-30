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

#include "lv2_external_ui.h"
#include <suil/suil.h>

struct LV2UIFeaturesList final {
   //! Abstraction of host services that a plug-ins native UI needs
   struct UIHandler {
      virtual ~UIHandler();
      virtual int ui_resize(int width, int height) = 0;
      virtual void ui_closed() = 0;
      virtual void suil_port_write(uint32_t port_index, uint32_t buffer_size,
         uint32_t protocol, const void *buffer) = 0;
      virtual uint32_t suil_port_index(const char *port_symbol) = 0;
   };

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
};

#endif
#endif
