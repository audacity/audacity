/*
* Audacity: A Digital Audio Editor
*/
#include "builtineffectsrepository.h"

#include "framework/global/log.h"
#include "effects/effects_base/effectstypes.h"

#include <QtQml>
#include <algorithm>

using namespace au::effects;

void BuiltinEffectsRepository::registerMeta(const EffectMeta& meta)
{
    IF_ASSERT_FAILED(meta.isValid()) {
        LOGW() << "Trying to register invalid meta with id: " << meta.id.toStdString();
    }
    IF_ASSERT_FAILED(std::none_of(m_metas.begin(), m_metas.end(), [&meta](const EffectMeta& m) { return m.id == meta.id; })) {
        LOGW() << "Registering meta with duplicate id: " << meta.id.toStdString();
    }
    m_metas.push_back(meta);
    m_effectMetaListUpdated.notify();
}

muse::async::Notification BuiltinEffectsRepository::effectMetaListUpdated() const
{
    return m_effectMetaListUpdated;
}

EffectMetaList BuiltinEffectsRepository::effectMetaList() const
{
    return m_metas;
}
