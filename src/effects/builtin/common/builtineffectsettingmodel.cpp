/*
 * Audacity: A Digital Audio Editor
 */
#include "builtineffectsettingmodel.h"

namespace au::effects {
BuiltinEffectSettingModel::BuiltinEffectSettingModel(QObject* parent)
    : BuiltinEffectModel{parent} {}

QString BuiltinEffectSettingModel::paramId() const
{
    return m_paramId;
}

void BuiltinEffectSettingModel::setParamId(const QString& newParamId)
{
    if (m_paramId == newParamId) {
        return;
    }
    m_paramId = newParamId;
    emit paramIdChanged();
}

void BuiltinEffectSettingModel::doReload()
{
    emit valueChanged();
}

void BuiltinEffectSettingModel::doUpdateSettings()
{
    doReload();
}
} // namespace au::effects
