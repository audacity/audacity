/*
 * Audacity: A Digital Audio Editor
 */
#include "externalidleui.h"
#include <cassert>

namespace au::effects {
ExternalIdleUi::ExternalIdleUi(SuilInstance& suilInstance)
    : m_externalUiWidget(static_cast<LV2_External_UI_Widget*>(suil_instance_get_widget(&suilInstance)))
{
    assert(m_externalUiWidget != nullptr);
    showFloatingUi();
}

ExternalIdleUi::~ExternalIdleUi()
{
    hideFloatingUi();
}

void ExternalIdleUi::showFloatingUi()
{
    LV2_EXTERNAL_UI_SHOW(m_externalUiWidget);
}

void ExternalIdleUi::hideFloatingUi()
{
    LV2_EXTERNAL_UI_HIDE(m_externalUiWidget);
}

bool ExternalIdleUi::idleFloatingUi()
{
    LV2_EXTERNAL_UI_RUN(m_externalUiWidget);
    return true;
}
} // namespace au::effects
