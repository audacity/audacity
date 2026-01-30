/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>

#include "framework/global/iinteractive.h"
#include "framework/global/io/ifilesystem.h"
#include "framework/global/modularity/ioc.h"

#include "effects/effects_base/view/abstracteffectviewmodel.h"
#include "effects/effects_base/ieffectsprovider.h"
#include "effects/effects_base/ieffectinstancesregister.h"
#include "effects/effects_base/iparameterextractorregistry.h"

namespace au::effects {
class NyquistParameterExtractorService;

//! ViewModel for the Nyquist Prompt effect
//! Provides a multi-line text editor for entering Nyquist code directly
class NyquistPromptViewModel : public AbstractEffectViewModel
{
    Q_OBJECT

    Q_PROPERTY(QString commandText READ commandText WRITE setCommandText NOTIFY commandTextChanged FINAL)
    Q_PROPERTY(QString title READ title CONSTANT FINAL)

protected:
    muse::Inject<IEffectsProvider> effectsProvider{ this };
    muse::Inject<IParameterExtractorRegistry> parameterExtractorRegistry{ this };
    muse::GlobalInject<muse::io::IFileSystem> fileSystem;
    muse::Inject<muse::IInteractive> interactive{ this };

public:
    NyquistPromptViewModel(QObject* parent, int instanceId);
    ~NyquistPromptViewModel() override = default;

    QString commandText() const;
    void setCommandText(const QString& text);

    QString title() const;

    Q_INVOKABLE void loadScript();
    Q_INVOKABLE void saveScript();
    Q_INVOKABLE void debugEffect();

signals:
    void commandTextChanged();

protected:
    void doInit() override;
    void doStartPreview() override;
    void doStopPreview() override;

private:
    NyquistParameterExtractorService* getExtractor() const;

    QString m_commandText;
    QString m_lastFilePath;
};

class NyquistPromptViewModelFactory : public EffectViewModelFactory<NyquistPromptViewModel>
{
};
}
