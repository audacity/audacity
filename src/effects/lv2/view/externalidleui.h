/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ilv2idleui.h"
#include "libraries/lib-lv2/lv2_external_ui.h"

#include <suil/suil.h>

namespace au::effects {
class ExternalIdleUi : public ILv2IdleUi
{
public:
    ExternalIdleUi(SuilInstance& suilInstance);
    ~ExternalIdleUi() override;

    void showExternalUi() override;
    void hideExternalUi() override;
    bool updateExternalUi() override;

private:
    LV2_External_UI_Widget* const m_externalUiWidget;
};
}
