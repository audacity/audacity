/*
* Audacity: A Digital Audio Editor
*/
#include "playbackmodule.h"

#include <QQmlEngine>
#include <QtQml>

#include "modularity/ioc.h"

#include "ui/iuiactionsregister.h"

#include "internal/playbackcontroller.h"
#include "internal/playbackuiactions.h"

#include "view/toolbars/playbacktoolbarmodel.h"
#include "view/toolbars/playbacktoolbarabstractitem.h"
#include "view/toolbars/playbacktoolbarcustomisemodel.h"
#include "view/toolbars/playbacktoolbarcustomiseitem.h"

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
    m_playbackController = std::make_shared<PlaybackController>();
    m_playbackUiActions = std::make_shared<PlaybackUiActions>(m_playbackController);

    ioc()->registerExport<IPlaybackController>(moduleName(), m_playbackController);
}

void PlaybackModule::resolveImports()
{
    auto ar = ioc()->resolve<IUiActionsRegister>(moduleName());
    if (ar) {
        ar->reg(m_playbackUiActions);
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
    qmlRegisterUncreatableType<PlaybackToolBarAbstractItem>("Muse.Ui", 1, 0, "PlaybackToolBarItem", "Cannot create an PlaybackToolBarItem");
    qmlRegisterType<PlaybackToolBarCustomiseModel>("Audacity.Playback", 1, 0, "PlaybackToolBarCustomiseModel");
    qmlRegisterUncreatableType<PlaybackToolBarCustomiseItem>("Audacity.Playback", 1, 0, "PlaybackToolBarCustomiseItem",
                                                             "Cannot create");
}

void PlaybackModule::onInit(const IApplication::RunMode& mode)
{
    if (mode == IApplication::RunMode::AudioPluginRegistration) {
        return;
    }

    m_playbackController->init();

    if (mode != IApplication::RunMode::GuiApp) {
        return;
    }

    m_playbackUiActions->init();
}
