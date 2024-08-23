/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <map>

#include "../ieffectsviewregister.h"

namespace au::effects {
class EffectsViewRegister : public IEffectsViewRegister
{
public:
    EffectsViewRegister() = default;

    void regUrl(const muse::String& effectName, const muse::String& viewUrl) override;
    const muse::String& viewUrl(const muse::String& effectName) const override;

private:

    std::map<muse::String, muse::String> m_data;
};
}
