/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "effects/effects_base/ieffectsprovider.h"

namespace au::effects {
class EffectsProviderMock : public IEffectsProvider
{
public:
    MOCK_METHOD(void, initOnce, (const muse::modularity::ContextPtr&, muse::audioplugins::IRegisterAudioPluginsScenario&),
                (override));

    MOCK_METHOD(EffectMetaList, effectMetaList, (), (const, override));
    MOCK_METHOD(EffectMeta, meta, (const EffectId&), (const, override));
    MOCK_METHOD(std::string, effectName, (const std::string&), (const, override));
    MOCK_METHOD(std::string, effectName, (const RealtimeEffectState&), (const, override));
    MOCK_METHOD(std::string, effectPath, (const std::string&), (const, override));
    MOCK_METHOD(bool, paramsAreInputAgnostic, (const EffectId&), (const, override));

    MOCK_METHOD(bool, hasEffectFamily, (EffectFamily), (const, override));

    MOCK_METHOD(muse::async::Notification, effectMetaListChanged, (), (const, override));

    MOCK_METHOD(bool, validateEffect, (const muse::modularity::ContextPtr&, const EffectId&), (override));
    MOCK_METHOD(muse::async::Promise<bool>, validateEffectAsync, (const EffectId&), (override));

    MOCK_METHOD(bool, loadEffect, (const EffectId&), (const, override));
    MOCK_METHOD(Effect*, effect, (const EffectId&), (const, override));
    MOCK_METHOD(void, setEffectActivated, (const EffectId&, bool), (override));

    MOCK_METHOD(NewPluginsRegistered, rescanPlugins,
                (const muse::modularity::ContextPtr&, muse::audioplugins::IRegisterAudioPluginsScenario&), (override));
    MOCK_METHOD(void, forgetPlugins, (const EffectFilter&), (override));

    MOCK_METHOD(void, save, (), (override));
};
}
