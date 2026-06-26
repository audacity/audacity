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

#include "project/types/projecttypes.h"

using namespace au::appshell;
using namespace muse::actions;

static const QString TRACK_VIEW_SECTION_NAME("TrackViewSection");

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
    dispatcher()->reg(this, "quit", [this](const muse::actions::ActionData& args) {
        muse::io::path_t installerPath = args.count() > 1 ? args.arg<muse::io::path_t>(1) : "";
        quit(installerPath);
    });

    dispatcher()->reg(this, "restart", [this]() {
        restart();
    });

    dispatcher()->reg(this, "fullscreen", this, &ApplicationActionController::toggleFullScreen);

    dispatcher()->reg(this, "about-audacity", this, &ApplicationActionController::openAboutDialog);
    dispatcher()->reg(this, "about-qt", this, &ApplicationActionController::openAboutQtDialog);
    dispatcher()->reg(this, "online-handbook", this, &ApplicationActionController::openOnlineHandbookPage);
    dispatcher()->reg(this, "ask-help", this, &ApplicationActionController::openAskForHelpPage);
    dispatcher()->reg(this, "preference-dialog", this, &ApplicationActionController::openPreferencesDialog);

    dispatcher()->reg(this, "revert-factory", this, &ApplicationActionController::revertToFactorySettings);

    dispatcher()->reg(this, "audio-settings", this, &ApplicationActionController::openAudioSettingsDialog);
    dispatcher()->reg(this, "shortcuts-preferences", this, &ApplicationActionController::openShortcutsPreferencesDialog);
    dispatcher()->reg(this, "editing-preferences", this, &ApplicationActionController::openEditingPreferencesDialog);
    dispatcher()->reg(this, "spectrogram-preferences", this, &ApplicationActionController::openSpectrogramPreferencesDialog);

    // Global actions
    dispatcher()->reg(this, "action://copy", this, &ApplicationActionController::doGlobalCopy);
    dispatcher()->reg(this, "action://cut", this, &ApplicationActionController::doGlobalCut);
    dispatcher()->reg(this, "action://paste", this, &ApplicationActionController::doGlobalPaste);
    dispatcher()->reg(this, "action://undo", this, &ApplicationActionController::doGlobalUndo);
    dispatcher()->reg(this, "action://redo", this, &ApplicationActionController::doGlobalRedo);
    dispatcher()->reg(this, "action://delete", this, &ApplicationActionController::doGlobalDelete);
    dispatcher()->reg(this, "action://cancel", this, &ApplicationActionController::doGlobalCancel);
    dispatcher()->reg(this, "action://trigger", this, &ApplicationActionController::doGlobalTrigger);
    dispatcher()->reg(this, "action://enter", this, &ApplicationActionController::doGlobalEnter);
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
    if (isProjectOpened()) {
        event->ignore();
        return;
    }

    const QMimeData* mime = event->mimeData();
    const QList<QUrl> urls = mime->urls();
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
    if (isProjectOpened()) {
        event->ignore();
        return;
    }

    const QMimeData* mime = event->mimeData();
    const QList<QUrl> urls = mime->urls();
    if (urls.isEmpty()) {
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
        const QFileOpenEvent* openEvent = static_cast<const QFileOpenEvent*>(event);
        const QUrl url = openEvent->url();

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

            return true;
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
        return true;
    }

    return QObject::eventFilter(watched, event);
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
#if defined(Q_OS_LINUX)
        platformInteractive()->revealInFileBrowser(installerPath);
#else
        platformInteractive()->openUrl(QUrl::fromLocalFile(installerPath.toQString()));
#endif
    }

    QCoreApplication::exit();
    return true;
}

void ApplicationActionController::restart()
{
    if (projectFilesController()->closeOpenedProject(false)) {
        if (multiwindowsProvider()->windowCount() == 1) {
            application()->restart();
        } else {
            multiwindowsProvider()->quitAllAndRestartLast();

            QCoreApplication::exit();
        }
    }
}

void ApplicationActionController::toggleFullScreen()
{
    mainWindow()->toggleFullScreen();
}

void ApplicationActionController::openAboutDialog()
{
    interactive()->open("audacity://about/audacity");
}

void ApplicationActionController::openAboutQtDialog()
{
    QApplication::aboutQt();
}

void ApplicationActionController::openOnlineHandbookPage()
{
    std::string handbookUrl = configuration()->handbookUrl();
    platformInteractive()->openUrl(handbookUrl);
}

void ApplicationActionController::openAskForHelpPage()
{
    std::string askForHelpUrl = configuration()->askForHelpUrl();
    platformInteractive()->openUrl(askForHelpUrl);
}

void ApplicationActionController::openPreferencesDialog()
{
    //! TODO AU4
    // if (multiwindowsProvider()->isPreferencesAlreadyOpened()) {
    //     multiwindowsProvider()->activateWindowWithOpenedPreferences();
    //     return;
    // }

    interactive()->open("audacity://preferences");
}

void ApplicationActionController::openAudioSettingsDialog()
{
    muse::UriQuery preferencesUri("audacity://preferences");
    preferencesUri.addParam("currentPageId", muse::Val("audio-settings"));

    interactive()->open(preferencesUri);
}

void ApplicationActionController::openShortcutsPreferencesDialog()
{
    muse::UriQuery preferencesUri("audacity://preferences");
    preferencesUri.addParam("currentPageId", muse::Val("shortcuts"));

    interactive()->open(preferencesUri);
}

void ApplicationActionController::openEditingPreferencesDialog()
{
    muse::UriQuery preferencesUri("audacity://preferences");
    preferencesUri.addParam("currentPageId", muse::Val("editing"));

    interactive()->open(preferencesUri);
}

void ApplicationActionController::openSpectrogramPreferencesDialog()
{
    muse::UriQuery preferencesUri("audacity://preferences");
    preferencesUri.addParam("currentPageId", muse::Val("spectrogram"));

    interactive()->open(preferencesUri);
}

void ApplicationActionController::revertToFactorySettings()
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

void ApplicationActionController::doGlobalCopy()
{
    if (isProjectOpenedAndFocused()) {
        dispatcher()->dispatch("action://trackedit/copy");
    } else {
        // resolve other actions
    }
}

void ApplicationActionController::doGlobalCut()
{
    if (isProjectOpenedAndFocused()) {
        dispatcher()->dispatch("action://trackedit/cut");
    } else {
        // resolve other actions
    }
}

void ApplicationActionController::doGlobalPaste()
{
    if (isProjectOpenedAndFocused()) {
        dispatcher()->dispatch("action://trackedit/paste-default");
    } else {
        // resolve other actions
    }
}

void ApplicationActionController::doGlobalUndo()
{
    if (isProjectOpened()) {
        dispatcher()->dispatch("action://trackedit/undo");
    } else {
        // resolve other actions
    }
}

void ApplicationActionController::doGlobalRedo()
{
    if (isProjectOpened()) {
        dispatcher()->dispatch("action://trackedit/redo");
    } else {
        // resolve other actions
    }
}

void ApplicationActionController::doGlobalDelete()
{
    if (isProjectOpenedAndFocused()) {
        dispatcher()->dispatch("action://trackedit/delete");
    } else {
        // resolve other actions
    }
}

void ApplicationActionController::doGlobalCancel()
{
    if (isProjectOpenedAndFocused()) {
        dispatcher()->dispatch("action://trackedit/cancel");
    }

    dispatcher()->dispatch("nav-escape");
    commandDispatcher()->dispatch(muse::ui::ESCAPE_COMMAND);
}

void ApplicationActionController::doGlobalTrigger()
{
    if (isProjectOpened()) {
        dispatcher()->dispatch("action://playback/togglePlayStop");
    } else {
        commandDispatcher()->dispatch(muse::ui::TRIGGER_CONTROL_COMMAND);
    }
}

void ApplicationActionController::doGlobalEnter()
{
    const muse::ui::INavigationSection* activeSection = navigationController()->activeSection();
    if (activeSection && activeSection->name() != TRACK_VIEW_SECTION_NAME) {
        commandDispatcher()->dispatch(muse::ui::TRIGGER_CONTROL_COMMAND);
        return;
    }

    if (isProjectOpenedAndFocused()) {
        dispatcher()->dispatch("track-view-replace-selection");
        return;
    }

    commandDispatcher()->dispatch(muse::ui::TRIGGER_CONTROL_COMMAND);
}
