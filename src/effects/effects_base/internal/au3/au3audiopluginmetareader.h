#pragma once

#include "audioplugins/iaudiopluginmetareader.h"
#include "global/iapplication.h"

class PluginProvider;

namespace au::effects {
class Au3AudioPluginMetaReader : public muse::audioplugins::IAudioPluginMetaReader
{
public:
    Au3AudioPluginMetaReader(PluginProvider&);
    ~Au3AudioPluginMetaReader() override;

    void init();
    void deinit();

private:
    virtual void doInit() {}
    muse::RetVal<muse::audio::AudioResourceMetaList> readMeta(const muse::io::path_t& pluginPath) const override;

    PluginProvider& m_pluginProvider;
    bool m_initialized = false;
    bool m_terminated = false;
};
}
