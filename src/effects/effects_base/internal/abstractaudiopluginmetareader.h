#pragma once

#include "audioplugins/iaudiopluginmetareader.h"
#include "global/iapplication.h"

class PluginProvider;

namespace au::effects {
class AbstractAudioPluginMetaReader : public muse::audioplugins::IAudioPluginMetaReader
{
public:
    AbstractAudioPluginMetaReader(PluginProvider&);
    ~AbstractAudioPluginMetaReader() override;

    void init(const muse::IApplication::RunMode& mode);
    void deinit();

private:
    virtual void doInit(const muse::IApplication::RunMode& mode);
    muse::RetVal<muse::audio::AudioResourceMetaList> readMeta(const muse::io::path_t& pluginPath) const override;

    PluginProvider& m_pluginProvider;
    bool m_initialized = false;
    bool m_terminated = false;
};
}
