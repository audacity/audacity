/*
* Audacity: A Digital Audio Editor
*/
#include "musevstpluginsregister.h"

#include "musevstplugininstance.h"

#include "log.h"

using namespace au::effects;
using namespace muse::vst;

IVstPluginInstancePtr MuseVstInstancesRegister::makeAndRegisterInstrPlugin(const muse::audio::AudioResourceId&,
                                                                           const muse::audio::TrackId)
{
    NOT_IMPLEMENTED;
    return nullptr;
}

IVstPluginInstancePtr MuseVstInstancesRegister::makeAndRegisterFxPlugin(const muse::audio::AudioResourceId&,
                                                                        const muse::audio::TrackId,
                                                                        const muse::audio::AudioFxChainOrder)
{
    NOT_IMPLEMENTED;
    return nullptr;
}

IVstPluginInstancePtr MuseVstInstancesRegister::makeAndRegisterMasterFxPlugin(const muse::audio::AudioResourceId&,
                                                                              const muse::audio::AudioFxChainOrder)
{
    NOT_IMPLEMENTED;
    return nullptr;
}

void MuseVstInstancesRegister::registerInstrPlugin(const muse::audio::TrackId trackId, IVstPluginInstancePtr instance)
{
    IF_ASSERT_FAILED(instance) {
        return;
    }

    std::lock_guard lock(m_mutex);

    m_instances.insert_or_assign({ Type::Instrument, instance->resourceId(), trackId, 0 }, instance);
}

void MuseVstInstancesRegister::registerFxPlugin(const muse::audio::TrackId trackId,
                                                const muse::audio::AudioFxChainOrder chainOrder,
                                                IVstPluginInstancePtr instance)
{
    IF_ASSERT_FAILED(instance) {
        return;
    }

    std::lock_guard lock(m_mutex);

    m_instances.insert_or_assign({ Type::Effect, instance->resourceId(), trackId, chainOrder }, instance);
}

void MuseVstInstancesRegister::registerMasterFxPlugin(const muse::audio::AudioFxChainOrder chainOrder, IVstPluginInstancePtr instance)
{
    registerFxPlugin(-1, chainOrder, instance);
}

IVstPluginInstancePtr MuseVstInstancesRegister::instanceById(const VstPluginInstanceId id) const
{
    std::lock_guard lock(m_mutex);
    for (const auto& p : m_instances) {
        if (p.second->id() == id) {
            return p.second;
        }
    }
    return nullptr;
}

IVstPluginInstancePtr MuseVstInstancesRegister::instrumentPlugin(const muse::audio::AudioResourceId& resourceId,
                                                                 const muse::audio::TrackId trackId) const
{
    std::lock_guard lock(m_mutex);

    auto it = m_instances.find({ Type::Instrument, resourceId, trackId, 0 });
    if (it != m_instances.end()) {
        return it->second;
    }

    LOGE() << "Unable to find instrument plugin, trackId: " << trackId
           << " , resourceId: " << resourceId;

    return nullptr;
}

IVstPluginInstancePtr MuseVstInstancesRegister::fxPlugin(const muse::audio::AudioResourceId& resourceId,
                                                         const muse::audio::TrackId trackId,
                                                         const muse::audio::AudioFxChainOrder chainOrder) const
{
    std::lock_guard lock(m_mutex);

    auto it = m_instances.find({ Type::Effect, resourceId, trackId, chainOrder });
    if (it != m_instances.end()) {
        return it->second;
    }

    LOGE() << "Unable to find fx plugin, trackId: " << trackId
           << ", resourceId: " << resourceId
           << ", chainOrder: " << chainOrder;

    return nullptr;
}

IVstPluginInstancePtr MuseVstInstancesRegister::masterFxPlugin(const muse::audio::AudioResourceId& resourceId,
                                                               const muse::audio::AudioFxChainOrder chainOrder) const
{
    std::lock_guard lock(m_mutex);

    auto it = m_instances.find({ Type::Effect, resourceId, -1, chainOrder });
    if (it != m_instances.end()) {
        return it->second;
    }

    LOGE() << "Unable to find master fx plugin"
           << ", resourceId: " << resourceId
           << ", chainOrder: " << chainOrder;

    return nullptr;
}

void MuseVstInstancesRegister::unregisterById(const VstPluginInstanceId id)
{
    std::lock_guard lock(m_mutex);

    muse::remove_if(m_instances, [id](const auto& p) {
        return p.second->id() == id;
    });
}

void MuseVstInstancesRegister::unregisterInstrPlugin(const muse::audio::AudioResourceId& resourceId, const muse::audio::TrackId trackId)
{
    std::lock_guard lock(m_mutex);

    m_instances.erase({ Type::Instrument, resourceId, trackId, 0 });
}

void MuseVstInstancesRegister::unregisterFxPlugin(const muse::audio::AudioResourceId& resourceId,
                                                  const muse::audio::TrackId trackId,
                                                  const muse::audio::AudioFxChainOrder chainOrder)
{
    std::lock_guard lock(m_mutex);

    m_instances.erase({ Type::Effect, resourceId, trackId, chainOrder });
}

void MuseVstInstancesRegister::unregisterMasterFxPlugin(const muse::audio::AudioResourceId& resourceId,
                                                        const muse::audio::AudioFxChainOrder chainOrder)
{
    unregisterFxPlugin(resourceId, -1, chainOrder);
}

void MuseVstInstancesRegister::unregisterAllInstrPlugin()
{
    std::lock_guard lock(m_mutex);

    muse::remove_if(m_instances, [](const auto& p) {
        return p.first.type == Type::Instrument;
    });
}

void MuseVstInstancesRegister::unregisterAllFx()
{
    std::lock_guard lock(m_mutex);

    muse::remove_if(m_instances, [](const auto& p) {
        return p.first.type == Type::Effect;
    });
}
