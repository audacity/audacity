/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"
#include "effects/ieffectsconfiguration.h"

#include "effects/ieffectsprovider.h"

namespace au::effects {
class EffectsProvider : public IEffectsProvider
{
    muse::Inject<IEffectsConfiguration> configuration;

public:
    void reloadEffects() override;

    ManifestList manifestList() const override;
    muse::async::Notification manifestListChanged() const override;

    Manifest manifest(const muse::String& id) const override;

private:
    mutable ManifestList m_manifests;
    muse::async::Notification m_manifestListChanged;
};
}
