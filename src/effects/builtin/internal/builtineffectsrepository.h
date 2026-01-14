/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <map>

#include "modularity/ioc.h"

#include "../ieffectsviewregister.h"
#include "../ibuiltineffectsrepository.h"

#include "au3-components/ComponentInterfaceSymbol.h"

namespace au::effects {
class BuiltinEffectsRepository : public IBuiltinEffectsRepository, public muse::Injectable
{
    muse::Inject<IEffectsViewRegister> effectsViewRegister{ this };

public:
    BuiltinEffectsRepository(const muse::modularity::ContextPtr& ctx) : muse::Injectable(ctx) {}

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
