/*
* Audacity: A Digital Audio Editor
*/

#include <QQmlEngine>
#include <QtQml>

#include "framework/interactive/iinteractiveuriregister.h"

#include "internal/playbackconfiguration.h"
#include "internal/playbackcontroller.h"
#include "internal/playbackmetercontroller.h"
#include "internal/playbackuiactions.h"
#include "internal/au3/au3playback.h"
#include "internal/au3/au3trackplaybackcontrol.h"

#include "view/common/playbackstatemodel.h"
#include "view/common/playbackmetermodel.h"
#include "view/common/metermodel.h"
#include "view/panels/playbackmeterpanelmodel.h"

#include "playbackmodule.h"

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
    m_controller = std::make_shared<PlaybackController>(iocContext());
    m_uiActions = std::make_shared<PlaybackUiActions>(iocContext(), m_controller);
    m_playback = std::make_shared<Au3Playback>(iocContext());

    ioc()->registerExport<PlaybackConfiguration>(moduleName(), m_configuration);
    ioc()->registerExport<IPlaybackController>(moduleName(), m_controller);
    ioc()->registerExport<playback::IPlayback>(moduleName(), m_playback);
    ioc()->registerExport<ITrackPlaybackControl>(moduleName(), std::make_shared<Au3TrackPlaybackControl>(iocContext()));
    ioc()->registerExport<IPlaybackMeterController>(moduleName(), std::make_shared<PlaybackMeterController>());
}

void PlaybackModule::resolveImports()
{
    auto ar = ioc()->resolve<IUiActionsRegister>(moduleName());
    if (ar) {
        ar->reg(m_uiActions);
    }
    auto ir = ioc()->resolve<muse::interactive::IInteractiveUriRegister>(moduleName());
    if (ir) {
        ir->registerQmlUri(muse::Uri("audacity://playback/loop_region_in_out"), "Audacity/Playback/dialogs/LoopRegionInOut.qml");
    }
}

void PlaybackModule::registerResources()
{
    playback_init_qrc();
}

void PlaybackModule::registerUiTypes()
{
    qmlRegisterType<PlaybackStateModel>("Audacity.Playback", 1, 0, "PlaybackStateModel");
    qmlRegisterType<PlaybackMeterPanelModel>("Audacity.Playback", 1, 0, "PlaybackMeterPanelModel");
    qmlRegisterType<PlaybackMeterModel>("Audacity.Playback", 1, 0, "PlaybackMeterModel");
    qmlRegisterType<MeterModel>("Audacity.Playback", 1, 0, "MeterModel");
    qmlRegisterUncreatableType<TracksBehaviors>("Audacity.Playback", 1, 0, "SoloBehavior", "Not creatable from QML");
    qmlRegisterUncreatableType<PlaybackQualityPrefs>("Audacity.Playback", 1, 0, "PlaybackQuality", "Not creatable from QML");
    qmlRegisterUncreatableType<DitherTypePrefs>("Audacity.Playback", 1, 0, "DitherType", "Not creatable from QML");
    qmlRegisterUncreatableType<PlaybackMeterType>("Audacity.Playback", 1, 0, "PlaybackMeterType", "Not creatable from QML");
    qmlRegisterUncreatableType<PlaybackMeterStyle>("Audacity.Playback", 1, 0, "PlaybackMeterStyle", "Not creatable from QML");
    qmlRegisterUncreatableType<PlaybackMeterPosition>("Audacity.Playback", 1, 0, "PlaybackMeterPosition", "Not creatable from QML");
    qmlRegisterUncreatableType<PlaybackMeterDbRange>("Audacity.Playback", 1, 0, "PlaybackMeterDbRange", "Not creatable from QML");
}

void PlaybackModule::onInit(const IApplication::RunMode& mode)
{
    if (mode == IApplication::RunMode::AudioPluginRegistration) {
        return;
    }

    if (mode != IApplication::RunMode::GuiApp) {
        return;
    }

    m_configuration->init();
    m_uiActions->init();
    m_controller->init();
}

void PlaybackModule::onDeinit()
{
    m_controller->deinit();
}
