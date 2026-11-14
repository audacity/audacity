/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "au3-nyquist-effects/NyquistBase.h"

class WaveChannel;

namespace au::effects {
class NyquistPromptEffect : public NyquistBase
{
public:
    NyquistPromptEffect();
    ~NyquistPromptEffect() override = default;

    static inline NyquistPromptEffect
    * FetchParameters(NyquistPromptEffect& e, EffectSettings&)
    {
        return &e;
    }

    ComponentInterfaceSymbol GetSymbol() const override;
    EffectFamilySymbol GetFamily() const override;
    EffectGroup GetGroup() const override { return EffectGroup::None; }

    wxString& commandText() { return mInputCmd; }

    static const ComponentInterfaceSymbol Symbol;

    bool m_foo = false;
};
}
