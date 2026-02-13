/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "abstracteffectviewmodel.h"
#include "ieffectsprovider.h"
#include "../iparameterextractorregistry.h"
#include "context/iglobalcontext.h"

namespace au::effects {
class EffectParametersListModel;

//! ViewModel for auto-generated effect UI (fallback when no custom UI is available)
class GeneratedEffectViewerModel : public AbstractEffectViewModel
{
    Q_OBJECT

    Q_PROPERTY(EffectParametersListModel * parametersModel READ parametersModel CONSTANT FINAL)
    Q_PROPERTY(QString effectName READ effectName CONSTANT FINAL)
    Q_PROPERTY(QString title READ title CONSTANT FINAL)
    Q_PROPERTY(QString noParametersMessage READ noParametersMessage CONSTANT FINAL)
    Q_PROPERTY(bool hasParameters READ hasParameters NOTIFY hasParametersChanged FINAL)
    Q_PROPERTY(double sampleRate READ sampleRate NOTIFY sampleRateChanged FINAL)
    Q_PROPERTY(double tempo READ tempo NOTIFY timeSignatureChanged FINAL)
    Q_PROPERTY(int upperTimeSignature READ upperTimeSignature NOTIFY timeSignatureChanged FINAL)
    Q_PROPERTY(int lowerTimeSignature READ lowerTimeSignature NOTIFY timeSignatureChanged FINAL)

    muse::Inject<IEffectsProvider> effectsProvider{ this };
    muse::Inject<IParameterExtractorRegistry> parameterExtractorRegistry{ this };
    muse::Inject<au::context::IGlobalContext> globalContext{ this };

public:
    explicit GeneratedEffectViewerModel(QObject* parent, EffectInstanceId instanceId);
    ~GeneratedEffectViewerModel() override;

    EffectParametersListModel* parametersModel() const { return m_parametersModel; }
    QString effectName() const { return m_effectName; }
    QString title() const { return m_title; }
    QString noParametersMessage() const;
    bool hasParameters() const;

    double sampleRate() const;
    double tempo() const;
    int upperTimeSignature() const;
    int lowerTimeSignature() const;

signals:
    void hasParametersChanged();
    void sampleRateChanged();
    void timeSignatureChanged();

protected:
    void doInit() override;
    void doStartPreview() override;
    void doStopPreview() override;

private:
    static QString computeEffectName(EffectInstanceId instanceId, IEffectsProvider* provider, IEffectInstancesRegister* instancesRegister);
    static QString computeTitle(const QString& effectName);

    IParameterExtractorService* getParameterExtractor() const;

    EffectParametersListModel* const m_parametersModel;
    const QString m_effectName;
    const QString m_title;
};

class GeneratedEffectViewerModelFactory : public EffectViewModelFactory<GeneratedEffectViewerModel>
{
};
}
