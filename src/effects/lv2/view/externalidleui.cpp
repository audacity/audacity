/*
 * Audacity: A Digital Audio Editor
 */
#include "externalidleui.h"
#include "log.h"

namespace au::effects {
ExternalIdleUi::ExternalIdleUi(SuilInstance& suilInstance)
    : m_externalUiWidget(static_cast<LV2_External_UI_Widget*>(suil_instance_get_widget(&suilInstance)))
{
    DO_ASSERT_X(m_externalUiWidget != nullptr, "ExternalIdleUi: m_externalUiWidget is null");
    showExternalUi();
}

ExternalIdleUi::~ExternalIdleUi()
{
    hideExternalUi();
}

void ExternalIdleUi::showExternalUi()
{
    LV2_EXTERNAL_UI_SHOW(m_externalUiWidget);
}

void ExternalIdleUi::hideExternalUi()
{
    LV2_EXTERNAL_UI_HIDE(m_externalUiWidget);
}

bool ExternalIdleUi::updateExternalUi()
{
    LV2_EXTERNAL_UI_RUN(m_externalUiWidget);
    return true;
}
} // namespace au::effects
