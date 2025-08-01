/*
 * Audacity: A Digital Audio Editor
 */
#include "abstracteffectsettingmodel.h"

namespace au::effects {
AbstractEffectSettingModel::AbstractEffectSettingModel(QObject* parent)
    : AbstractEffectModel{parent} {}

QString AbstractEffectSettingModel::paramId() const
{
    return m_paramId;
}

void AbstractEffectSettingModel::setParamId(const QString& newParamId)
{
    if (m_paramId == newParamId) {
        return;
    }
    m_paramId = newParamId;
    emit paramIdChanged();
}

void AbstractEffectSettingModel::doReload()
{
    emit valueChanged();
    emit labelChanged();
    emit minChanged();
    emit maxChanged();
    emit stepChanged();
}

void AbstractEffectSettingModel::doUpdateSettings()
{
    doReload();
}

const Effect* AbstractEffectSettingModel::effect() const
{
    return effectsProvider()->effect(effectId());
}
} // namespace au::effects
