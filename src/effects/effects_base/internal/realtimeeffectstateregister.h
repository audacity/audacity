#pragma once

#include "irealtimeeffectstateregister.h"
#include <unordered_map>

namespace au::effects {
class RealtimeEffectStateRegister : public IRealtimeEffectStateRegister
{
public:
    RealtimeEffectStateId registerState(const RealtimeEffectStatePtr& state) override;
    void unregisterState(RealtimeEffectStateId id) override;
    RealtimeEffectStatePtr stateById(RealtimeEffectStateId id) const override;
    void clear() override;

private:
    std::unordered_map<RealtimeEffectStateId, std::weak_ptr<RealtimeEffectState> > m_stateMap;
};
}
