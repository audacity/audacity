/*
* Audacity: A Digital Audio Editor
*/
#pragma once

// from muse
#include "vst/ivstmodulesrepository.h"

namespace au::effects {
class MuseVstModulesRepository : public muse::vst::IVstModulesRepository
{
public:
    MuseVstModulesRepository() = default;

    void init();
    void deinit();

    bool exists(const muse::audio::AudioResourceId& resourceId) const override;
    muse::vst::PluginModulePtr pluginModule(const muse::audio::AudioResourceId& resourceId) const override;
    void addPluginModule(const muse::audio::AudioResourceId& resourceId) override;
    void removePluginModule(const muse::audio::AudioResourceId& resourceId) override;
    muse::audio::AudioResourceMetaList instrumentModulesMeta() const override;
    muse::audio::AudioResourceMetaList fxModulesMeta() const override;
    void refresh() override;

private:

    muse::vst::PluginContext m_pluginContext;
};
}
