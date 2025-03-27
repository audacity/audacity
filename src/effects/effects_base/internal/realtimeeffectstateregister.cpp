#include "realtimeeffectstateregister.h"

#include "libraries/lib-realtime-effects/RealtimeEffectState.h"

namespace au::effects {
RealtimeEffectStateId RealtimeEffectStateRegister::registerState(const RealtimeEffectStatePtr& state)
{
    const auto id = state->GetID();
    m_stateMap[id] = state;
    return id;
}

void RealtimeEffectStateRegister::unregisterState(RealtimeEffectStateId id)
{
    m_stateMap.erase(id);
}

RealtimeEffectStatePtr RealtimeEffectStateRegister::stateById(RealtimeEffectStateId id) const
{
    const auto it = m_stateMap.find(id);
    return it != m_stateMap.end() ? it->second.lock() : nullptr;
}

void RealtimeEffectStateRegister::clear()
{
    m_stateMap.clear();
}
}
