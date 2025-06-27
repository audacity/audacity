/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

namespace au::effects {
class ILv2IdleUi
{
public:
    virtual ~ILv2IdleUi() = default;

    virtual void showExternalUi() = 0;
    virtual void hideExternalUi() = 0;
    virtual bool updateExternalUi() = 0;
};
}
