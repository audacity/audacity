/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <map>

// from muse
#include "vst/internal/vstinstancesregister.h"

namespace au::effects {
class MuseVstInstancesRegister : public muse::vst::IVstInstancesRegister
{
public:
    MuseVstInstancesRegister() = default;

    // make
    muse::vst::IVstPluginInstancePtr makeAndRegisterInstrPlugin(const muse::audio::AudioResourceId& resourceId,
                                                                const muse::audio::TrackId trackId) override;

    muse::vst::IVstPluginInstancePtr makeAndRegisterFxPlugin(const muse::audio::AudioResourceId& resourceId,
                                                             const muse::audio::TrackId trackId,
                                                             const muse::audio::AudioFxChainOrder chainOrder) override;

    muse::vst::IVstPluginInstancePtr makeAndRegisterMasterFxPlugin(const muse::audio::AudioResourceId& resourceId,
                                                                   const muse::audio::AudioFxChainOrder chainOrder) override;

    // register
    void registerInstrPlugin(const muse::audio::TrackId trackId, muse::vst::IVstPluginInstancePtr instance) override;

    void registerFxPlugin(const muse::audio::TrackId trackId, const muse::audio::AudioFxChainOrder chainOrder,
                          muse::vst::IVstPluginInstancePtr instance) override;

    void registerMasterFxPlugin(const muse::audio::AudioFxChainOrder chainOrder, muse::vst::IVstPluginInstancePtr instance) override;

    // get
    muse::vst::IVstPluginInstancePtr instanceById(const muse::vst::VstPluginInstanceId id) const override;
    muse::vst::IVstPluginInstancePtr instrumentPlugin(const muse::audio::AudioResourceId& resourceId,
                                                      const muse::audio::TrackId trackId) const override;
    muse::vst::IVstPluginInstancePtr fxPlugin(const muse::audio::AudioResourceId& resourceId, const muse::audio::TrackId trackId,
                                              const muse::audio::AudioFxChainOrder chainOrder) const override;
    muse::vst::IVstPluginInstancePtr masterFxPlugin(const muse::audio::AudioResourceId& resourceId,
                                                    const muse::audio::AudioFxChainOrder chainOrder) const override;

    // unregister
    void unregisterById(const muse::vst::VstPluginInstanceId id) override;
    void unregisterInstrPlugin(const muse::audio::AudioResourceId& resourceId, const muse::audio::TrackId trackId) override;
    void unregisterFxPlugin(const muse::audio::AudioResourceId& resourceId, const muse::audio::TrackId trackId,
                            const muse::audio::AudioFxChainOrder chainOrder) override;
    void unregisterMasterFxPlugin(const muse::audio::AudioResourceId& resourceId, const muse::audio::AudioFxChainOrder chainOrder) override;

    void unregisterAllInstrPlugin() override;
    void unregisterAllFx() override;

private:
    mutable std::mutex m_mutex;

    enum class Type {
        Undefined = 0,
        Instrument,
        Effect
    };

    struct Key {
        Type type = Type::Undefined;
        muse::audio::AudioResourceId resourceId;
        muse::audio::TrackId trackId = -1;
        muse::audio::AudioFxChainOrder chainOrder = 0;

        inline bool operator ==(const Key& k) const
        {
            return type == k.type && trackId == k.trackId && resourceId == k.resourceId && chainOrder == k.chainOrder;
        }

        inline bool operator <(const Key& k) const
        {
            if (type != k.type) {
                return type < k.type;
            }

            if (trackId != k.trackId) {
                return trackId < k.trackId;
            }

            if (resourceId != k.resourceId) {
                return resourceId < k.resourceId;
            }

            return chainOrder < k.chainOrder;
        }
    };

    std::map<Key, muse::vst::IVstPluginInstancePtr> m_instances;
};
}
