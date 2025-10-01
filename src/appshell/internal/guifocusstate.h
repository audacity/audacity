/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "iguifocusstate.h"

namespace au::appshell {
class GuiFocusState : public IGuiFocusState
{
public:
    ~GuiFocusState() override = default;

    void init();

    bool isFocused() const override;
    muse::async::Channel<bool> isFocusedChanged() const override;

private:
    muse::async::Channel<bool> m_isFocusedChanged;
};
}
