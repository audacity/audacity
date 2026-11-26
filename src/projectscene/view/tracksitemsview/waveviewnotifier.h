/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"

#include "context/iglobalcontext.h"
#include "iprojectsceneconfiguration.h"

namespace au::projectscene {
//! Tells the wave views of a context what they all listen to. Subscribing each view to these channels
//! itself costs a search through all the other views, on every creation and destruction of a view.
class WaveViewNotifier : public QObject, public muse::async::Asyncable, public muse::Contextable
{
    Q_OBJECT

    muse::GlobalInject<IProjectSceneConfiguration> configuration;

    muse::ContextInject<context::IGlobalContext> globalContext{ this };

public:
    static WaveViewNotifier* forContext(const muse::modularity::ContextPtr& ctx);

signals:
    void waveformSettingsChanged();
    void clipStyleChanged();
    void isRecordingChanged();

private:
    explicit WaveViewNotifier(const muse::modularity::ContextPtr& ctx);
};
}
