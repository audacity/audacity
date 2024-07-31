/*
* Audacity: A Digital Audio Editor
*/
#include "effectsprovider.h"

#include "log.h"

using namespace au::effects;

void EffectsProvider::reloadEffects()
{
    //! todo: get list of effects

    m_manifestListChanged.notify();
}

ManifestList EffectsProvider::manifestList() const
{
    return m_manifests;
}

muse::async::Notification EffectsProvider::manifestListChanged() const
{
    return m_manifestListChanged;
}

Manifest EffectsProvider::manifest(const muse::String& id) const
{
    for (const Manifest& manifest : m_manifests) {
        if (manifest.id == id) {
            return manifest;
        }
    }

    LOGE() << "not found manifest: " << id;
    return Manifest();
}
