/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * A Digital Audio Editor
 *
 * Copyright (C) 2024 Audacity BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#include "applicationactioncontroller.h"

#include "framework/ui/navigationcommands.h"

#include <algorithm>

#include <QApplication>
#include <QCloseEvent>
#include <QFileOpenEvent>
#include <QWindow>
#include <QMimeData>

#include "framework/global/async/async.h"
#include "framework/global/defer.h"
#include "framework/global/translation.h"
#include "framework/rcommand/actiontocommand.h"

#include "project/types/projecttypes.h"

#include "../appshellcommands.h"

using namespace au::appshell;
using namespace muse;
using namespace muse::actions;
using namespace muse::rcommand;

static const QString TRACK_VIEW_SECTION_NAME("TrackViewSection");
static const QString TIMELINE_SECTION_NAME("TimelineSection");
static const QString VERTICAL_RULER_CONTROL_NAME("VerticalRuler");

static const std::string INSTALLER_PATH_PARAM("installer_path");

static CommandQuery quitConv(const Command& command, const ActionData& args)
{
    CommandQuery query(command);
    if (args.count() > 1) {
        query.addParam(INSTALLER_PATH_PARAM, Val(args.arg<std::string>(1)));
    }
    return query;
}

void ApplicationActionController::preInit()
{
    qApp->installEventFilter(this);

#ifdef Q_OS_MAC
    // Re-open window when user clicks the dock icon while all windows are closed
    connect(qApp, &QGuiApplication::applicationStateChanged, this, [this](Qt::ApplicationState state) {
        if (state != Qt::ApplicationActive) {
            return;
        }
        QWindow* window = mainWindow() ? mainWindow()->qWindow() : nullptr;
        if (window && !window->isVisible()) {
            window->show();
            window->requestActivate();
        }
    });
#endif
}

void ApplicationActionController::init()
{
    auto cd = commandDispatcher();
    cd->onRequest(this, GLOBAL_QUIT_COMMAND, [this](const Params& params) {
        const muse::io::path_t installerPath = params.at(INSTALLER_PATH_PARAM).toString();
        return quit(installerPath) ? make_ok() : make_ret(Ret::Code::Cancel);
    });
    cd->onRequest(this, GLOBAL_RESTART_COMMAND, [this]() { return restart(); });
    cd->onRequest(this, APP_TOGGLE_FULLSCREEN_COMMAND, [this]() { return toggleFullScreen(); });
    cd->onRequest(this, APP_ABOUT_COMMAND, [this]() { return openAboutDialog(); });
    cd->onRequest(this, APP_ABOUT_QT_COMMAND, [this]() { return openAboutQtDialog(); });
    cd->onRequest(this, APP_ONLINE_HANDBOOK_COMMAND, [this]() { return openOnlineHandbookPage(); });
    cd->onRequest(this, APP_ASK_HELP_COMMAND, [this]() { return openAskForHelpPage(); });
    cd->onRequest(this, APP_PREFERENCES_COMMAND, [this]() { return openPreferencesDialog(); });
    cd->onRequest(this, APP_REVERT_FACTORY_COMMAND, [this]() { return revertToFactorySettings(); });
    cd->onRequest(this, APP_AUDIO_SETTINGS_COMMAND, [this]() { return openAudioSettingsDialog(); });
    cd->onRequest(this, APP_SHORTCUTS_PREFERENCES_COMMAND, [this]() { return openShortcutsPreferencesDialog(); });
    cd->onRequest(this, APP_EDITING_PREFERENCES_COMMAND, [this]() { return openEditingPreferencesDialog(); });
    cd->onRequest(this, APP_SPECTROGRAM_PREFERENCES_COMMAND, [this]() { return openSpectrogramPreferencesDialog(); });

    cd->onRequest(this, GLOBAL_COPY_COMMAND, [this]() { return doGlobalCopy(); });
    cd->onRequest(this, GLOBAL_CUT_COMMAND, [this]() { return doGlobalCut(); });
    cd->onRequest(this, GLOBAL_PASTE_COMMAND, [this]() { return doGlobalPaste(); });
    cd->onRequest(this, GLOBAL_UNDO_COMMAND, [this]() { return doGlobalUndo(); });
    cd->onRequest(this, GLOBAL_REDO_COMMAND, [this]() { return doGlobalRedo(); });
    cd->onRequest(this, GLOBAL_DELETE_COMMAND, [this]() { return doGlobalDelete(); });
    cd->onRequest(this, GLOBAL_CANCEL_COMMAND, [this]() { return doGlobalCancel(); });
    cd->onRequest(this, GLOBAL_TRIGGER_COMMAND, [this]() { return doGlobalTrigger(); });
    cd->onRequest(this, GLOBAL_ENTER_COMMAND, [this]() { return doGlobalEnter(); });
    cd->onRequest(this, GLOBAL_SHIFT_ENTER_COMMAND, [this]() { return doGlobalShiftEnter(); });
    cd->onRequest(this, GLOBAL_CONTEXT_MENU_COMMAND, [this]() { return doGlobalContextMenu(); });

    static const std::vector<ActionToCommand> actionToCommand = {
        { "quit", GLOBAL_QUIT_COMMAND, quitConv },
        { "restart", GLOBAL_RESTART_COMMAND, {} },
        { "fullscreen", APP_TOGGLE_FULLSCREEN_COMMAND, {} },
        { "about-audacity", APP_ABOUT_COMMAND, {} },
        { "about-qt", APP_ABOUT_QT_COMMAND, {} },
        { "online-handbook", APP_ONLINE_HANDBOOK_COMMAND, {} },
        { "ask-help", APP_ASK_HELP_COMMAND, {} },
        { "preference-dialog", APP_PREFERENCES_COMMAND, {} },
        { "revert-factory", APP_REVERT_FACTORY_COMMAND, {} },
        { "audio-settings", APP_AUDIO_SETTINGS_COMMAND, {} },
        { "shortcuts-preferences", APP_SHORTCUTS_PREFERENCES_COMMAND, {} },
        { "editing-preferences", APP_EDITING_PREFERENCES_COMMAND, {} },
        { "spectrogram-preferences", APP_SPECTROGRAM_PREFERENCES_COMMAND, {} },
        { "action://copy", GLOBAL_COPY_COMMAND, {} },
        { "action://cut", GLOBAL_CUT_COMMAND, {} },
        { "action://paste", GLOBAL_PASTE_COMMAND, {} },
        { "action://undo", GLOBAL_UNDO_COMMAND, {} },
        { "action://redo", GLOBAL_REDO_COMMAND, {} },
        { "action://delete", GLOBAL_DELETE_COMMAND, {} },
        { "action://cancel", GLOBAL_CANCEL_COMMAND, {} },
        { "action://trigger", GLOBAL_TRIGGER_COMMAND, {} },
        { "action://enter", GLOBAL_ENTER_COMMAND, {} },
        { "action://shift-enter", GLOBAL_SHIFT_ENTER_COMMAND, {} },
        { "action://context-menu", GLOBAL_CONTEXT_MENU_COMMAND, {} },
    };
    registerActionToCommand(this, actionToCommand, commandDispatcher(), dispatcher());
}

const std::vector<muse::actions::ActionCode>& ApplicationActionController::prohibitedActionsWhileRecording() const
{
    static const std::vector<ActionCode> PROHIBITED_WHILE_RECORDING {
        "quit",
        "restart",
    };

    return PROHIBITED_WHILE_RECORDING;
}

void ApplicationActionController::onDragEnterEvent(QDragEnterEvent* event)
{
    onDragMoveEvent(event);
}

void ApplicationActionController::onDragMoveEvent(QDragMoveEvent* event)
{
    const QMimeData* mime = event->mimeData();
    const QList<QUrl> urls = mime->urls();
    if (!urls.isEmpty()) {
        const QUrl& url = urls.front();
        if (url.isLocalFile() && extensionInstaller()->isFileSupported(url.toLocalFile())) {
            event->acceptProposedAction();
            return;
        }
    }

    if (isProjectOpened()) {
        event->ignore();
        return;
    }

    for (const QUrl& url : urls) {
        if (projectFilesController()->isUrlSupported(url)) {
            event->acceptProposedAction();
            return;
        }
    }

    event->ignore();
}

void ApplicationActionController::onDropEvent(QDropEvent* event)
{
    const QMimeData* mime = event->mimeData();
    const QList<QUrl> urls = mime->urls();
    if (urls.isEmpty()) {
        return;
    }

    const QUrl& url = urls.front();
    if (url.isLocalFile() && extensionInstaller()->isFileSupported(url.toLocalFile())) {
        event->accept();
        const muse::io::path_t filePath = url.toLocalFile();
        muse::async::Async::call(this, [this, filePath]() {
            extensionInstaller()->installExtension(filePath);
        });
        return;
    }

    if (isProjectOpened()) {
        event->ignore();
        return;
    }

    QList<QUrl> projectUrls;
    QStringList mediaFiles;

    for (const QUrl& url : urls) {
        if (!projectFilesController()->isUrlSupported(url)) {
            continue;
        }

        if (au::project::isAudacityFile(muse::io::path_t(url))) {
            projectUrls << url;
        } else {
            mediaFiles << url.toLocalFile();
        }
    }

    if (projectUrls.isEmpty() && mediaFiles.isEmpty()) {
        event->ignore();
        return;
    }

    event->accept();

    if (!projectUrls.isEmpty()) {
        muse::async::Async::call(this, [this, projectUrls]() {
            for (const QUrl& url : projectUrls) {
                dispatcher()->dispatch("file-open", ActionData::make_arg1<QUrl>(url));
            }
        });
    }

    if (!mediaFiles.isEmpty()) {
        muse::async::Async::call(this, [this, mediaFiles]() {
            dispatcher()->dispatch("project-import-startup-media",
                                   ActionData::make_arg2<QStringList, bool>(mediaFiles, false));
        });
    }
}

bool ApplicationActionController::canReceiveAction(const ActionCode& code) const
{
    if (recordController()->isRecording()) {
        return !muse::contains(prohibitedActionsWhileRecording(), code);
    }
    return true;
}

bool ApplicationActionController::eventFilter(QObject* watched, QEvent* event)
{
    if (event->type() == QEvent::Close && watched == mainWindow()->qWindow()) {
        if (multiwindowsProvider()->windowCount() > 1) {
            if (!projectFilesController()->closeOpenedProject()) {
                event->ignore();
                return true;
            }
            auto provider = multiwindowsProvider();
            auto ctx = iocContext();
            QMetaObject::invokeMethod(qApp, [provider, ctx]() {
                // during the call the window and the context will be destroyed
                // do not capture or use anything that is context-dependent here
                // i.e. dont use Async::call(this instead of invokeMethod
                provider->quitWindow(ctx);
            }, Qt::QueuedConnection);
            event->accept();
            return true;
        }
#ifdef Q_OS_MAC
        // On macos closing the last window does not exit the app
        if (!projectFilesController()->closeOpenedProject()) {
            event->ignore();
            return true;
        }
        // Instead we hide the window, it will be shown when needed
        mainWindow()->qWindow()->setVisible(false);
        event->accept();
        return true;
#else
        const bool accepted = quit();
        event->setAccepted(accepted);
        return true;
#endif
    }

    if (event->type() == QEvent::Quit) {
        const bool accepted = quit();
        event->setAccepted(accepted);
        return true;
    }

    //! on macOS custom URL opened from browser are also passed as QEvent::FileOpen
    if (event->type() == QEvent::FileOpen && watched == qApp) {
        handleFileOpenEvent(static_cast<const QFileOpenEvent*>(event));
        return true;
    }

    return QObject::eventFilter(watched, event);
}

void ApplicationActionController::processPendingEvents()
{
    for (const std::unique_ptr<QEvent>& event : applicationEventController()->takePendingEvents()) {
        if (event->type() == QEvent::FileOpen) {
            handleFileOpenEvent(static_cast<const QFileOpenEvent*>(event.get()));
        }
    }
}

void ApplicationActionController::handleFileOpenEvent(const QFileOpenEvent* event)
{
    const QUrl url = event->url();

    // TODO: isUrlSupported - is misleading, as it does not handle audio.com urls
    if (projectFilesController()->isUrlSupported(url)) {
        if (startupScenario()->startupCompleted()) {
            // On macos the main window may be hidden, show and raise it
            // before loading the project
            if (auto mw = mainWindow()) {
                if (QWindow* window = mw->qWindow(); window && !window->isVisible()) {
                    window->setVisible(true);
                }
                mw->requestShowOnFront();
            }
            dispatcher()->dispatch("file-open", ActionData::make_arg1<QUrl>(url));
        } else {
            startupScenario()->setStartupProjectFile(project::ProjectFile { url });
        }

        return;
    }

    const QString urlStr = url.toString(QUrl::FullyEncoded);
    if (startupScenario()->startupCompleted()) {
        if (auto mw = mainWindow()) {
            if (QWindow* window = mw->qWindow(); window && !window->isVisible()) {
                window->setVisible(true);
            }
            mw->requestShowOnFront();
        }
        dispatcher()->dispatch("open-url", ActionData::make_arg1<QString>(urlStr));
    } else {
        startupScenario()->setStartupUrl(urlStr);
    }
}

bool ApplicationActionController::quit(const muse::io::path_t& installerPath)
{
    if (m_quiting) {
        return false;
    }

    m_quiting = true;
    DEFER {
        m_quiting = false;
    };

    auto allContexts = application()->contexts();

    // Close the current window first, then others
    auto thisCtx = iocContext();
    std::stable_partition(allContexts.begin(), allContexts.end(),
                          [&thisCtx](const auto& ctx) { return ctx == thisCtx; });

    for (const auto& ctx : allContexts) {
        auto pfc = muse::modularity::ioc(ctx)->resolve<project::IProjectFilesController>("appshell");
        if (pfc && !pfc->closeOpenedProject()) {
            return false;
        }
    }

    if (!installerPath.empty()) {
        //! NOTE: All windows are quitting to complete the update, apply it
        //! in-place, falling back to handing the package to the user.
        bool applied = false;
        if (appUpdateService()->canAutoInstall()) {
            const muse::RetVal<muse::io::path_t> prepared = appUpdateService()->prepareUpdate(installerPath);
            if (prepared.ret) {
                applied = bool(appUpdateService()->finalizeUpdate(prepared.val));
            }
        }

        if (!applied) {
#if defined(Q_OS_LINUX)
            platformInteractive()->revealInFileBrowser(installerPath);
#else
            platformInteractive()->openUrl(QUrl::fromLocalFile(installerPath.toQString()));
#endif
        }
    }

    QCoreApplication::exit();
    return true;
}

muse::Ret ApplicationActionController::restart()
{
    if (!projectFilesController()->closeOpenedProject(false)) {
        return make_ret(Ret::Code::Cancel);
    }

    if (multiwindowsProvider()->windowCount() == 1) {
        application()->restart();
    } else {
        multiwindowsProvider()->quitAllAndRestartLast();

        QCoreApplication::exit();
    }

    return make_ok();
}

muse::Ret ApplicationActionController::toggleFullScreen()
{
    mainWindow()->toggleFullScreen();
    return make_ok();
}

muse::Ret ApplicationActionController::openAboutDialog()
{
    interactive()->open("audacity://about/audacity");
    return make_ok();
}

muse::Ret ApplicationActionController::openAboutQtDialog()
{
    QApplication::aboutQt();
    return make_ok();
}

muse::Ret ApplicationActionController::openOnlineHandbookPage()
{
    std::string handbookUrl = configuration()->handbookUrl();
    platformInteractive()->openUrl(handbookUrl);
    return make_ok();
}

muse::Ret ApplicationActionController::openAskForHelpPage()
{
    std::string askForHelpUrl = configuration()->askForHelpUrl();
    platformInteractive()->openUrl(askForHelpUrl);
    return make_ok();
}

muse::Ret ApplicationActionController::openPreferencesDialog()
{
    //! TODO AU4
    // if (multiwindowsProvider()->isPreferencesAlreadyOpened()) {
    //     multiwindowsProvider()->activateWindowWithOpenedPreferences();
    //     return;
    // }

    interactive()->open("audacity://preferences");
    return make_ok();
}

muse::Ret ApplicationActionController::openAudioSettingsDialog()
{
    muse::UriQuery preferencesUri("audacity://preferences");
    preferencesUri.addParam("currentPageId", muse::Val("audio-settings"));

    interactive()->open(preferencesUri);
    return make_ok();
}

muse::Ret ApplicationActionController::openShortcutsPreferencesDialog()
{
    muse::UriQuery preferencesUri("audacity://preferences");
    preferencesUri.addParam("currentPageId", muse::Val("shortcuts"));

    interactive()->open(preferencesUri);
    return make_ok();
}

muse::Ret ApplicationActionController::openEditingPreferencesDialog()
{
    muse::UriQuery preferencesUri("audacity://preferences");
    preferencesUri.addParam("currentPageId", muse::Val("editing"));

    interactive()->open(preferencesUri);
    return make_ok();
}

muse::Ret ApplicationActionController::openSpectrogramPreferencesDialog()
{
    muse::UriQuery preferencesUri("audacity://preferences");
    preferencesUri.addParam("currentPageId", muse::Val("spectrogram"));

    interactive()->open(preferencesUri);
    return make_ok();
}

muse::Ret ApplicationActionController::revertToFactorySettings()
{
    std::string title = muse::trc("appshell", "Are you sure you want to revert to factory settings?");
    std::string question = muse::trc("appshell",
                                     "This action will reset all your app preferences and custom UI configurations. "
                                     "It also deletes your custom workspaces and shortcuts. "
                                     "You will also need to scan all third party plugins again.\n\n"
                                     "This action will not delete any of your projects.");

    muse::IInteractive::ButtonData cancelBtn = interactive()->buttonData(muse::IInteractive::Button::Cancel);
    cancelBtn.accent = true;

    int revertBtn = int(muse::IInteractive::Button::Apply);
    auto promise = interactive()->warning(title, question,
                                          { cancelBtn,
                                            muse::IInteractive::ButtonData(revertBtn, muse::trc("appshell", "Revert")) },
                                          cancelBtn.btn, { muse::IInteractive::Option::WithIcon },
                                          muse::trc("appshell", "Revert to factory settings"));

    promise.onResolve(this, [this](const muse::IInteractive::Result& res) {
        if (res.isButton(muse::IInteractive::Button::Cancel)) {
            return;
        }

        static constexpr bool KEEP_DEFAULT_SETTINGS = false;
        static constexpr bool NOTIFY_ABOUT_CHANGES = false;
        static constexpr bool NOTIFY_OTHER_INSTANCES = false;
        configuration()->revertToFactorySettings(KEEP_DEFAULT_SETTINGS, NOTIFY_ABOUT_CHANGES, NOTIFY_OTHER_INSTANCES);

        std::string title = muse::trc("appshell", "Would you like to restart Audacity now?");
        std::string question = muse::trc("appshell", "Audacity needs to be restarted for these changes to take effect.");

        int restartBtn = int(muse::IInteractive::Button::Apply);
        auto promise = interactive()->question(title, question,
                                               { interactive()->buttonData(muse::IInteractive::Button::Cancel),
                                                 muse::IInteractive::ButtonData(restartBtn,
                                                                                muse::trc("appshell", "Restart"), true) },
                                               restartBtn, {},
                                               muse::trc("appshell", "Restart Audacity"));

        promise.onResolve(this, [this](const muse::IInteractive::Result& res) {
            if (!res.isButton(muse::IInteractive::Button::Cancel)) {
                restart();
            }
        });
    });

    return make_ok();
}

bool ApplicationActionController::isProjectOpened() const
{
    bool hasProject = globalContext()->currentProject() != nullptr;
    bool isOpened = uiContextResolver()->matchWithCurrent(context::UiCtxProjectOpened);
    return hasProject && isOpened;
}

bool ApplicationActionController::isProjectOpenedAndFocused() const
{
    bool isOpened = isProjectOpened();
    bool isFocused = uiContextResolver()->matchWithCurrent(context::UiCtxProjectFocused);
    return isOpened && isFocused;
}

muse::Ret ApplicationActionController::doGlobalCopy()
{
    if (!isProjectOpenedAndFocused()) {
        return make_ret(Ret::Code::NotSupported);
    }

    dispatcher()->dispatch("action://trackedit/copy");
    return make_ok();
}

muse::Ret ApplicationActionController::doGlobalCut()
{
    if (!isProjectOpenedAndFocused()) {
        return make_ret(Ret::Code::NotSupported);
    }

    dispatcher()->dispatch("action://trackedit/cut");
    return make_ok();
}

muse::Ret ApplicationActionController::doGlobalPaste()
{
    if (!isProjectOpened()) {
        return make_ret(Ret::Code::NotSupported);
    }

    dispatcher()->dispatch("action://trackedit/paste-default");
    return make_ok();
}

muse::Ret ApplicationActionController::doGlobalUndo()
{
    if (!isProjectOpened()) {
        return make_ret(Ret::Code::NotSupported);
    }

    dispatcher()->dispatch("action://trackedit/undo");
    return make_ok();
}

muse::Ret ApplicationActionController::doGlobalRedo()
{
    if (!isProjectOpened()) {
        return make_ret(Ret::Code::NotSupported);
    }

    dispatcher()->dispatch("action://trackedit/redo");
    return make_ok();
}

muse::Ret ApplicationActionController::doGlobalDelete()
{
    if (!isProjectOpenedAndFocused()) {
        return make_ret(Ret::Code::NotSupported);
    }

    dispatcher()->dispatch("action://trackedit/delete");
    return make_ok();
}

muse::Ret ApplicationActionController::doGlobalCancel()
{
    if (isProjectOpenedAndFocused()) {
        dispatcher()->dispatch("action://trackedit/cancel");
        return make_ok();
    }

    commandDispatcher()->dispatch(muse::ui::ESCAPE_COMMAND);
    return make_ok();
}

muse::Ret ApplicationActionController::doGlobalTrigger()
{
    if (isProjectOpened()) {
        dispatcher()->dispatch("action://playback/toggle-play-stop");
        return make_ok();
    }

    commandDispatcher()->dispatch(muse::ui::TRIGGER_CONTROL_COMMAND);
    return make_ok();
}

muse::Ret ApplicationActionController::doGlobalEnter()
{
    const muse::ui::INavigationSection* activeSection = navigationController()->activeSection();
    if (activeSection && activeSection->name() != TRACK_VIEW_SECTION_NAME) {
        commandDispatcher()->dispatch(muse::ui::TRIGGER_CONTROL_COMMAND);
        return make_ok();
    }

    if (isProjectOpenedAndFocused()) {
        dispatcher()->dispatch("track-view-replace-selection");
        return make_ok();
    }

    commandDispatcher()->dispatch(muse::ui::TRIGGER_CONTROL_COMMAND);
    return make_ok();
}

muse::Ret ApplicationActionController::doGlobalShiftEnter()
{
    const muse::ui::INavigationSection* activeSection = navigationController()->activeSection();
    if (activeSection && activeSection->name() != TRACK_VIEW_SECTION_NAME) {
        commandDispatcher()->dispatch(muse::ui::TRIGGER_CONTROL_COMMAND);
        return make_ok();
    }

    if (isProjectOpenedAndFocused()) {
        dispatcher()->dispatch("track-view-range-selection");
        return make_ok();
    }

    commandDispatcher()->dispatch(muse::ui::TRIGGER_CONTROL_COMMAND);
    return make_ok();
}

muse::Ret ApplicationActionController::doGlobalContextMenu()
{
    const muse::ui::INavigationSection* activeSection = navigationController()->activeSection();
    if (!activeSection) {
        return make_ret(Ret::Code::NotSupported);
    }

    if (activeSection->name() == TRACK_VIEW_SECTION_NAME) {
        const muse::ui::INavigationControl* activeControl = navigationController()->activeControl();
        if (activeControl && activeControl->name() == VERTICAL_RULER_CONTROL_NAME) {
            dispatcher()->dispatch("track-view-ruler-context-menu");
            return make_ok();
        }

        dispatcher()->dispatch("track-view-item-context-menu");
    } else if (activeSection->name() == TIMELINE_SECTION_NAME) {
        dispatcher()->dispatch("timeline-context-menu");
    }

    return make_ok();
}
