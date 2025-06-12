/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "libraries/lib-lv2/LV2UIFeaturesList.h"

namespace au::effects {
class Lv2ViewModel;

class Lv2UiHandler : public LV2UIFeaturesList::UIHandler
{
public:
    Lv2UiHandler(Lv2ViewModel& viewModel);
    ~Lv2UiHandler() override = default;

private:
    int ui_resize(int width, int height) override;
    void ui_closed() override;
    void suil_port_write(uint32_t port_index, uint32_t buffer_size, uint32_t protocol, const void* buffer) override;
    uint32_t suil_port_index(const char* port_symbol) override;

    Lv2ViewModel& m_viewModel;
};
}
