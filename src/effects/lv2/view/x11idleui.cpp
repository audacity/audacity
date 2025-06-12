/*
 * Audacity: A Digital Audio Editor
 */
#include "x11idleui.h"

namespace au::effects {
X11IdleUi::X11IdleUi(const LV2UI_Show_Interface* show, const LV2UI_Idle_Interface& idle, const SuilHandle handle)
    : m_uiShowInterface{show}, m_uiIdleInterface{&idle}, m_suilHandle{handle}
{
    showExternalUi();
}

void X11IdleUi::showExternalUi()
{
    if (m_uiShowInterface && m_uiShowInterface->show) {
        m_uiShowInterface->show(m_suilHandle);
    }
}

void X11IdleUi::hideExternalUi()
{
    if (m_uiShowInterface && m_uiShowInterface->hide) {
        m_uiShowInterface->hide(m_suilHandle);
        m_uiShowInterface = nullptr;
    }
}

bool X11IdleUi::updateExternalUi()
{
    if (m_uiIdleInterface && m_uiIdleInterface->idle && m_uiIdleInterface->idle(m_suilHandle)) {
        m_uiIdleInterface = nullptr;
        hideExternalUi();
        return false;
    }
    return true;
}
}
