/*
 * Audacity: A Digital Audio Editor
 */
#include "x11idleui.h"

namespace au::effects {
X11IdleUi::X11IdleUi(const LV2UI_Show_Interface* show, const LV2UI_Idle_Interface& idle, const SuilHandle handle)
    : m_uiShowInterface{show}, m_uiIdleInterface{&idle}, m_suilHandle{handle}
{
    showFloatingUi();
}

void X11IdleUi::showFloatingUi()
{
    if (m_uiShowInterface && m_uiShowInterface->show) {
        m_uiShowInterface->show(m_suilHandle);
    }
}

void X11IdleUi::hideFloatingUi()
{
    if (m_uiShowInterface && m_uiShowInterface->hide) {
        m_uiShowInterface->hide(m_suilHandle);
    }
}

bool X11IdleUi::idleFloatingUi()
{
    if (m_uiIdleInterface && m_uiIdleInterface->idle && m_uiIdleInterface->idle(m_suilHandle)) {
        m_uiIdleInterface = nullptr;
        if (m_uiShowInterface && m_uiShowInterface->hide) {
            m_uiShowInterface->hide(m_suilHandle);
        }
        m_uiShowInterface = nullptr;
        return false;
    }
    return true;
}
}
