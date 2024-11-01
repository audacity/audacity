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

    void init();

    EffectMetaList effectMetaList() const override;

    EffectMeta effectMeta(const ComponentInterfaceSymbol& symbol) const;

private:
    std::map<ComponentInterfaceSymbol, EffectMeta> m_metas;
};
}
