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
}

void AbstractEffectSettingModel::doUpdateSettings()
{
    doReload();
}
} // namespace au::effects
