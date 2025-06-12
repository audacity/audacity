/*
 * Audacity: A Digital Audio Editor
 */

#include "lv2uihandler.h"
#include "lv2viewmodel.h"

namespace au::effects {
Lv2UiHandler::Lv2UiHandler(Lv2ViewModel& viewModel)
    : m_viewModel(viewModel)
{}

int Lv2UiHandler::ui_resize(int width, int height)
{
    return m_viewModel.onResizeUi(width, height);
}

void Lv2UiHandler::ui_closed()
{
    m_viewModel.onUiClosed();
}

void Lv2UiHandler::suil_port_write(uint32_t port_index, uint32_t buffer_size, uint32_t protocol, const void* buffer)
{
    m_viewModel.onSuilPortWrite(port_index, buffer_size, protocol, buffer);
}

uint32_t Lv2UiHandler::suil_port_index(const char* port_symbol)
{
    return m_viewModel.suilPortIndex(port_symbol);
}
}
