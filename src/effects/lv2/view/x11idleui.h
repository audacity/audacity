/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ilv2idleui.h"
#include <lv2/ui/ui.h>
#include <suil/suil.h>

namespace au::effects {
class X11IdleUi : public ILv2IdleUi
{
public:
    X11IdleUi(const LV2UI_Show_Interface*, const LV2UI_Idle_Interface&, const SuilHandle);
    ~X11IdleUi() override = default;

    void showExternalUi() override;
    void hideExternalUi() override;
    bool updateExternalUi() override;

private:
    const LV2UI_Show_Interface* m_uiShowInterface;
    const LV2UI_Idle_Interface* m_uiIdleInterface;
    SuilHandle const m_suilHandle;
};
}
