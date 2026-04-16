/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../ieffectloadersregister.h"

namespace au::effects {
class EffectLoadersRegister : public IEffectLoadersRegister
{
public:
    void registerLoader(IEffectLoaderPtr loader) override;
    IEffectLoaderPtr loader(EffectFamily family) const override;

private:
    std::vector<IEffectLoaderPtr> m_loaders;
};
} // namespace au::effects
