/*
 * Audacity: A Digital Audio Editor
 */

#include "lv2idleuifactory.h"
#include "externalidleui.h"
#include "x11idleui.h"

std::unique_ptr<au::effects::ILv2IdleUi> au::effects::tryCreateLv2IdleUi(SuilInstance& suilInstance, bool isExternalUi)
{
    if (isExternalUi) {
        return std::make_unique<ExternalIdleUi>(suilInstance);
    } else {
        auto uiShowInterface { static_cast<const LV2UI_Show_Interface*>(
                                   suil_instance_extension_data(&suilInstance, LV2_UI__showInterface)) };
        auto uiIdleInterface{ static_cast<const LV2UI_Idle_Interface*>(
                                  suil_instance_extension_data(&suilInstance, LV2_UI__idleInterface)) };
        if (!uiIdleInterface || !uiIdleInterface->idle) {
            return nullptr;
        }

        return std::make_unique<X11IdleUi>(uiShowInterface, *uiIdleInterface, suil_instance_get_handle(&suilInstance));
    }
}
