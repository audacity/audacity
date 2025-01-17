/*
* Audacity: A Digital Audio Editor
*/
#include "playbackmodule.h"

#include <QQmlEngine>
#include <QtQml>

#include "modularity/ioc.h"

#include "ui/iuiactionsregister.h"

#include "internal/playbackconfiguration.h"
#include "internal/playbackcontroller.h"
#include "internal/playbackuiactions.h"

#include "internal/au3/au3playback.h"
#include "internal/au3/au3trackplaybackcontrol.h"

#include "view/common/playbackstatemodel.h"

#include "view/toolbars/components/timecodemodeselector.h"
#include "view/toolbars/components/timecodemodel.h"
#include "view/toolbars/components/bpmmodel.h"

using namespace au::playback;
using namespace muse;
using namespace muse::modularity;
using namespace muse::ui;
using namespace muse::actions;

static void playback_init_qrc()
{
    Q_INIT_RESOURCE(playback);
}

std::string PlaybackModule::moduleName() const
{
    return "playback";
}

void PlaybackModule::registerExports()
{
    m_configuration = std::make_shared<PlaybackConfiguration>();
    m_controller = std::make_shared<PlaybackController>();
    m_uiActions = std::make_shared<PlaybackUiActions>(m_controller);
    m_playback = std::make_shared<Au3Playback>();

    ioc()->registerExport<PlaybackConfiguration>(moduleName(), m_configuration);
    ioc()->registerExport<IPlaybackController>(moduleName(), m_controller);
    ioc()->registerExport<playback::IPlayback>(moduleName(), m_playback);

    ioc()->registerExport<ITrackPlaybackControl>(moduleName(), new Au3TrackPlaybackControl());
}

void PlaybackModule::resolveImports()
{
    auto ar = ioc()->resolve<IUiActionsRegister>(moduleName());
    if (ar) {
        ar->reg(m_uiActions);
    }
}

void PlaybackModule::registerResources()
{
    playback_init_qrc();
}

void PlaybackModule::registerUiTypes()
{
    qmlRegisterUncreatableType<TimecodeModeSelector>("Audacity.Playback", 1, 0, "TimecodeModeSelector",
                                                     "TimecodeModeSelector is a simple enum");
    qmlRegisterType<TimecodeModel>("Audacity.Playback", 1, 0, "TimecodeModel");
    qmlRegisterType<PlaybackStateModel>("Audacity.Playback", 1, 0, "PlaybackStateModel");
    qmlRegisterType<BPMModel>("Audacity.Playback", 1, 0, "BPMModel");
}

void PlaybackModule::onInit(const IApplication::RunMode& mode)
{
    if (mode == IApplication::RunMode::AudioPluginRegistration) {
        return;
    }

    m_controller->init();

    if (mode != IApplication::RunMode::GuiApp) {
        return;
    }

    m_uiActions->init();
}

void PlaybackModule::onDeinit()
{
    m_controller->deinit();
}
