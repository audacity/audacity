/*
 * Audacity: A Digital Audio Editor
 */
#include "effectviewerdialogmodel.h"
#include "log.h"

namespace au::effects {
EffectViewerDialogModel::EffectViewerDialogModel(QObject* parent)
    : QObject(parent)
{
}

QString EffectViewerDialogModel::title() const
{
    return m_title;
}

int EffectViewerDialogModel::instanceId() const
{
    return m_instanceId;
}

void EffectViewerDialogModel::setInstanceId(int newInstanceId)
{
    if (m_instanceId == newInstanceId) {
        return;
    }
    m_instanceId = newInstanceId;
    emit instanceIdChanged();
    const auto id = instancesRegister()->effectIdByInstanceId(m_instanceId).toStdString();
    m_title = QString::fromStdString(effectsProvider()->effectName(id));
    emit titleChanged();
}
} // namespace au::effects
