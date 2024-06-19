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

#include "view/common/playbackstatemodel.h"
#include "view/toolbars/playbacktoolbarmodel.h"
#include "view/toolbars/playbacktoolbarcustomisemodel.h"
#include "view/toolbars/playbacktoolbarcustomiseitem.h"

#include "view/toolbars/timecode/timecodemodel.h"

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

    ioc()->registerExport<PlaybackConfiguration>(moduleName(), m_configuration);
    ioc()->registerExport<IPlaybackController>(moduleName(), m_controller);
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
    // toolbars
    qmlRegisterType<PlaybackToolBarModel>("Audacity.Playback", 1, 0, "PlaybackToolBarModel");
    qmlRegisterType<PlaybackToolBarCustomiseModel>("Audacity.Playback", 1, 0, "PlaybackToolBarCustomiseModel");
    qmlRegisterUncreatableType<PlaybackToolBarCustomiseItem>("Audacity.Playback", 1, 0, "PlaybackToolBarCustomiseItem",
                                                             "Cannot create");

    qmlRegisterType<TimecodeModel>("Audacity.Playback", 1, 0, "TimecodeModel");
    qmlRegisterType<PlaybackStateModel>("Audacity.Playback", 1, 0, "PlaybackStateModel");
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
