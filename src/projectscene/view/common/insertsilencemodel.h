/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "modularity/ioc.h"
#include "iinteractive.h"
#include "iprojectsceneconfiguration.h"
#include "playback/iplayback.h"
#include "trackedit/itrackeditinteraction.h"
#include "trackedit/iselectioncontroller.h"

namespace au::projectscene {
class InsertSilenceModel : public QObject
{
    Q_OBJECT

    muse::Inject<playback::IPlayback> playback;
    muse::Inject<trackedit::ITrackeditInteraction> trackeditInteraction;
    muse::Inject<trackedit::ISelectionController> selectionController;
    muse::Inject<IProjectSceneConfiguration> configuration;
    muse::Inject<muse::IInteractive> interactive;

    Q_PROPERTY(double duration READ duration WRITE setDuration NOTIFY durationChanged)
    Q_PROPERTY(QString durationFormat READ durationFormat WRITE setDurationFormat NOTIFY durationFormatChanged)
    Q_PROPERTY(int sampleRate READ sampleRate NOTIFY sampleRateChanged)

public:
    Q_INVOKABLE void init();

    int sampleRate() const;
    double duration() const;
    void setDuration(double duration);
    QString durationFormat() const;
    void setDurationFormat(const QString& newDurationFormat);

    Q_INVOKABLE void apply();

signals:
    void durationChanged();
    void durationFormatChanged();
    void sampleRateChanged();

private:
    double m_duration = 0.0;
    QString m_durationFormat = "";
};
}
