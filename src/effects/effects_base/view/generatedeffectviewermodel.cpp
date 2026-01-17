/*
 * Audacity: A Digital Audio Editor
 */
#include "generatedeffectviewermodel.h"
#include "effectparameterslistmodel.h"

#include "au3-effects/EffectPlugin.h"

#include "framework/global/log.h"

using namespace au::effects;
using namespace muse;

GeneratedEffectViewerModel::GeneratedEffectViewerModel(QObject* parent, EffectInstanceId instanceId)
    : AbstractEffectViewModel(parent, instanceId)
    , m_parametersModel(new EffectParametersListModel(this, instanceId))
    , m_effectName(computeEffectName(instanceId, effectsProvider().get(), instancesRegister().get()))
    , m_title(computeTitle(m_effectName))
{
    // Connect to parameters model to forward hasParametersChanged signal
    connect(m_parametersModel, &EffectParametersListModel::hasParametersChanged,
            this, &GeneratedEffectViewerModel::hasParametersChanged);
}

QString GeneratedEffectViewerModel::computeEffectName(EffectInstanceId instanceId, IEffectsProvider* provider,
                                                      IEffectInstancesRegister* instancesRegister)
{
    if (instanceId < 0) {
        return QString();
    }

    EffectId effectId = instancesRegister->effectIdByInstanceId(instanceId);
    EffectMeta meta = provider->meta(effectId);

    if (meta.isValid()) {
        return meta.title.toQString();
    }
    return QString("Unknown Effect");
}

QString GeneratedEffectViewerModel::computeTitle(const QString& effectName)
{
    if (effectName.isEmpty()) {
        return QObject::tr("Auto-Generated UI");
    }
    return QObject::tr("%1 Fallback UI").arg(effectName);
}

QString GeneratedEffectViewerModel::noParametersMessage() const
{
    return QObject::tr("No parameters available for this effect");
}

bool GeneratedEffectViewerModel::hasParameters() const
{
    return m_parametersModel->hasParameters();
}

void GeneratedEffectViewerModel::doInit()
{
    LOGI() << "instanceId=" << instanceId();

    // Initialize the parameters model
    m_parametersModel->load();
}

void GeneratedEffectViewerModel::doStartPreview()
{
    LOGI() << "instanceId=" << instanceId();

    EffectSettingsAccessPtr settingsAccess = instancesRegister()->settingsAccessById(instanceId());
    if (!settingsAccess) {
        LOGE() << "No settings access for instance " << instanceId();
        return;
    }

    settingsAccess->ModifySettings([this](EffectSettings& settings) {
        executionScenario()->previewEffect(instanceId(), settings);
        return nullptr;
    });
}
