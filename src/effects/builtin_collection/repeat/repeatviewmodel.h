/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/builtin/qml/Audacity/BuiltinEffects/builtineffectmodel.h"

#include "context/iglobalcontext.h"
#include "playback/iplaybackconfiguration.h"

namespace au::effects {
class RepeatViewModel : public BuiltinEffectModel
{
    Q_OBJECT

    Q_PROPERTY(int repeatCount READ repeatCount WRITE setRepeatCount NOTIFY repeatCountChanged FINAL)
    Q_PROPERTY(int repeatCountMin READ repeatCountMin CONSTANT FINAL)
    Q_PROPERTY(int repeatCountMax READ repeatCountMax CONSTANT FINAL)

    Q_PROPERTY(double selectionDuration READ selectionDuration NOTIFY reloaded FINAL)
    Q_PROPERTY(double newSelectionDuration READ newSelectionDuration NOTIFY repeatCountChanged FINAL)
    Q_PROPERTY(double sampleRate READ sampleRate NOTIFY reloaded FINAL)
    Q_PROPERTY(double tempo READ tempo NOTIFY reloaded FINAL)
    Q_PROPERTY(int upperTimeSignature READ upperTimeSignature NOTIFY reloaded FINAL)
    Q_PROPERTY(int lowerTimeSignature READ lowerTimeSignature NOTIFY reloaded FINAL)

    //! Same format as the time display of the playback toolbar
    Q_PROPERTY(int timeFormat READ timeFormat NOTIFY timeFormatChanged FINAL)

    muse::GlobalInject<playback::IPlaybackConfiguration> playbackConfiguration;
    muse::ContextInject<context::IGlobalContext> globalContext{ this };

public:
    RepeatViewModel(QObject* parent, int instanceId);
    ~RepeatViewModel() override = default;

    int repeatCount() const;
    void setRepeatCount(int newRepeatCount);
    int repeatCountMin() const;
    int repeatCountMax() const;

    double selectionDuration() const;
    double newSelectionDuration() const;
    double sampleRate() const;
    double tempo() const;
    int upperTimeSignature() const;
    int lowerTimeSignature() const;
    int timeFormat() const;

signals:
    void repeatCountChanged();
    void reloaded();
    void timeFormatChanged();

private:
    void doReload() override;
};

class RepeatViewModelFactory : public EffectViewModelFactory<RepeatViewModel>
{
};
}
