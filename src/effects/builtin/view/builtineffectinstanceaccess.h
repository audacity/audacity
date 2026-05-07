/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "au3-components/EffectInterface.h"
#include "effects/effects_base/ieffectinstancesregister.h"
#include "effects/effects_base/ieffectsprovider.h"
#include "framework/global/log.h"
#include "framework/global/modularity/ioc.h"

#include <functional>
#include <memory>

namespace au::effects {
class BuiltinEffectInstanceAccess
{
protected:
    muse::GlobalInject<IEffectInstancesRegister> instancesRegister;
    muse::GlobalInject<IEffectsProvider> effectsProvider;

    const EffectInstanceId m_accessInstanceId;

    explicit BuiltinEffectInstanceAccess(EffectInstanceId instanceId);
    ~BuiltinEffectInstanceAccess() = default;

    std::shared_ptr<effects::EffectInstance> instance() const;
    const EffectSettings& settings() const;
    EffectSettingsAccessPtr settingsAccess() const;

    template<typename T>
    void modifySettings(const std::function<void(EffectSettings& settings)>& modifier)
    {
        const EffectSettingsAccessPtr access = this->settingsAccess();
        IF_ASSERT_FAILED(access) {
            return;
        }

        auto modified = false;
        access->ModifySettings([&](EffectSettings& settings) {
            EffectSettings copy = EffectSettings::Make<T>();

            const T* pSrc = settings.cast<T>();
            T* pDst = copy.cast<T>();
            IF_ASSERT_FAILED(pSrc && pDst) {
                return nullptr;
            }
            *pDst = *pSrc;
            copy.extra = settings.extra;

            modifier(copy);
            if (*pDst != *pSrc || !(copy.extra == settings.extra)) {
                settings = std::move(copy);
                modified = true;
            }
            return nullptr;
        });

        if (modified) {
            instancesRegister()->notifyAboutSettingsChanged(m_accessInstanceId);
        }
    }

    template<typename T>
    const T& settings() const
    {
        const T* st = settings().cast<T>();
        IF_ASSERT_FAILED(st) {
            static T null;
            return null;
        }
        return *st;
    }

    template<typename EffectType>
    const EffectType& effect() const
    {
        const EffectId effectId = instancesRegister()->effectIdByInstanceId(m_accessInstanceId);
        const Effect* e = effectsProvider()->effect(effectId);
        return *dynamic_cast<const EffectType*>(e);
    }

    template<typename EffectType>
    EffectType& effect()
    {
        return const_cast<EffectType&>(static_cast<const BuiltinEffectInstanceAccess*>(this)->effect<EffectType>());
    }
};
}
