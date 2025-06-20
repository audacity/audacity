/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

namespace au::effects {
class ILv2IdleUi
{
public:
    virtual ~ILv2IdleUi() = default;

    virtual void showFloatingUi() = 0;
    virtual void hideFloatingUi() = 0;
    virtual bool idleFloatingUi() = 0;
};
}
