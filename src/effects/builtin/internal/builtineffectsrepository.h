/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <map>

#include "modularity/ioc.h"
#include "effects/effects_base/ieffectsviewregister.h"

#include "../ibuiltineffectsrepository.h"

#include "libraries/lib-components/ComponentInterfaceSymbol.h"

namespace au::effects {
class BuiltinEffectsRepository : public IBuiltinEffectsRepository
{
    muse::Inject<IEffectsViewRegister> effectsViewRegister;

public:
    BuiltinEffectsRepository() = default;

    void preInit();
    void init();

    muse::async::Notification effectMetaListUpdated() const override;
    EffectMetaList effectMetaList() const override;

    EffectMeta effectMeta(const ComponentInterfaceSymbol& symbol) const;

private:
    void updateEffectMetaList();

    muse::async::Notification m_effectMetaListUpdated;
    std::map<ComponentInterfaceSymbol, EffectMeta> m_metas;
};
}
