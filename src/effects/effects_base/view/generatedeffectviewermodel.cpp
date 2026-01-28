/*
 * Audacity: A Digital Audio Editor
 */
#include "generatedeffectviewermodel.h"
#include "effectparameterslistmodel.h"

#include "au3-effects/EffectPlugin.h"
#include "playback/iaudiooutput.h"
#include "trackedit/itrackeditproject.h"

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

GeneratedEffectViewerModel::~GeneratedEffectViewerModel()
{
    // End parameter editing session
    EffectInstance* instance = instancesRegister()->instanceById(instanceId()).get();
    if (!instance) {
        return;
    }

    IParameterExtractorService* extractor = getParameterExtractor();
    if (extractor) {
        extractor->endParameterEditing(instance);
    }
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

IParameterExtractorService* GeneratedEffectViewerModel::getParameterExtractor() const
{
    const EffectId effectId = instancesRegister()->effectIdByInstanceId(instanceId());
    const EffectMeta meta = effectsProvider()->meta(effectId);
    const EffectFamily family = meta.family;

    return parameterExtractorRegistry()
           ? parameterExtractorRegistry()->extractorForFamily(family)
           : nullptr;
}

QString GeneratedEffectViewerModel::noParametersMessage() const
{
    return QObject::tr("No parameters available for this effect");
}

bool GeneratedEffectViewerModel::hasParameters() const
{
    return m_parametersModel->hasParameters();
}

double GeneratedEffectViewerModel::sampleRate() const
{
    return playback()->audioOutput()->sampleRate();
}

double GeneratedEffectViewerModel::tempo() const
{
    auto project = globalContext()->currentTrackeditProject();
    if (!project) {
        return 0.0;
    }
    return project->timeSignature().tempo;
}

int GeneratedEffectViewerModel::upperTimeSignature() const
{
    auto project = globalContext()->currentTrackeditProject();
    if (!project) {
        return 0;
    }
    return project->timeSignature().upper;
}

int GeneratedEffectViewerModel::lowerTimeSignature() const
{
    auto project = globalContext()->currentTrackeditProject();
    if (!project) {
        return 0;
    }
    return project->timeSignature().lower;
}

void GeneratedEffectViewerModel::doInit()
{
    LOGI() << "instanceId=" << instanceId();

    // Begin parameter editing session
    EffectInstance* instance = instancesRegister()->instanceById(instanceId()).get();
    if (instance) {
        IParameterExtractorService* extractor = getParameterExtractor();
        if (extractor) {
            EffectSettingsAccessPtr settingsAccess = instancesRegister()->settingsAccessById(instanceId());
            extractor->beginParameterEditing(instance, settingsAccess);
        }
    }

    // Subscribe to settings changes (e.g., when a preset is loaded)
    // This ensures the UI updates when settings change externally
    instancesRegister()->settingsChanged(instanceId()).onNotify(this, [this]() {
        m_parametersModel->reloadParameters();
    });

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

void GeneratedEffectViewerModel::doStopPreview()
{
    effectsProvider()->stopPreview();
}
