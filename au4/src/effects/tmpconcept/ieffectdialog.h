/*
* Audacity: A Digital Audio Editor
*/
#pragma once

namespace au::effects {
class IEffectDialog
{
public:
    virtual ~IEffectDialog() = default;
    virtual bool show() = 0;
};
}
