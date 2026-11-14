/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <map>

#include "../ibuiltineffectsviewregister.h"

namespace au::effects {
class BuiltinEffectsViewRegister : public IBuiltinEffectsViewRegister
{
public:
    BuiltinEffectsViewRegister() = default;

    void setDefaultUrl(const muse::String& viewUrl) override;
    void regUrl(const muse::String& effectName, const muse::String& viewUrl) override;

    const muse::String& viewUrl(const muse::String& effectName) const override;

private:

    muse::String m_defaultUrl;
    std::map<muse::String, muse::String> m_data;
};
}
