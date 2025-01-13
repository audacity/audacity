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

#include <QApplication>
#include <QCloseEvent>
#include <QFileOpenEvent>
#include <QWindow>
#include <QMimeData>

#include "async/async.h"
#include "audio/soundfonttypes.h"

#include "defer.h"
#include "translation.h"
#include "log.h"

using namespace au::appshell;
using namespace muse::actions;

void ApplicationActionController::preInit()
{
    qApp->installEventFilter(this);
}

void ApplicationActionController::init()
{
    dispatcher()->reg(this, "quit", [this] {
        quit();
    });

    dispatcher()->reg(this, "restart", [this]() {
        restart();
    });

    dispatcher()->reg(this, "fullscreen", this, &ApplicationActionController::toggleFullScreen);

    dispatcher()->reg(this, "about-musescore", this, &ApplicationActionController::openAboutDialog);
    dispatcher()->reg(this, "about-qt", this, &ApplicationActionController::openAboutQtDialog);
    dispatcher()->reg(this, "online-handbook", this, &ApplicationActionController::openOnlineHandbookPage);
    dispatcher()->reg(this, "ask-help", this, &ApplicationActionController::openAskForHelpPage);
    dispatcher()->reg(this, "preference-dialog", this, &ApplicationActionController::openPreferencesDialog);

    dispatcher()->reg(this, "revert-factory", this, &ApplicationActionController::revertToFactorySettings);
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

void ApplicationActionController::onDragMoveEvent(QDragMoveEvent*)
{
    //! TODO AU4
    // const QMimeData* mime = event->mimeData();
    // QList<QUrl> urls = mime->urls();
    // if (urls.count() > 0) {
    //     const QUrl& url = urls.front();
    //     if (projectFilesController()->isUrlSupported(url)
    //         || (url.isLocalFile() && audio::synth::isSoundFont(io::path_t(url)))) {
    //         event->setDropAction(Qt::LinkAction);
    //         event->acceptProposedAction();
    //     }
    // }
}

void ApplicationActionController::onDropEvent(QDropEvent*)
{
    //! TODO AU4
    // const QMimeData* mime = event->mimeData();
    // QList<QUrl> urls = mime->urls();
    // if (urls.count() > 0) {
    //     const QUrl& url = urls.front();
    //     LOGD() << url;

    //     bool shouldBeHandled = false;

    //     if (projectFilesController()->isUrlSupported(url)) {
    //         async::Async::call(this, [this, url]() {
    //             Ret ret = projectFilesController()->openProject(url);
    //             if (!ret) {
    //                 LOGE() << ret.toString();
    //             }
    //         });
    //         shouldBeHandled = true;
    //     } else if (url.isLocalFile()) {
    //         io::path_t filePath { url };

    //         if (audio::synth::isSoundFont(filePath)) {
    //             async::Async::call(this, [this, filePath]() {
    //                 Ret ret = soundFontRepository()->addSoundFont(filePath);
    //                 if (!ret) {
    //                     LOGE() << ret.toString();
    //                 }
    //             });
    //             shouldBeHandled = true;
    //         }
    //     }

    //     if (shouldBeHandled) {
    //         event->accept();
    //     } else {
    //         event->ignore();
    //     }
    // }
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
    //! TODO AU4
    if ((event->type() == QEvent::Close && watched == mainWindow()->qWindow())
        || event->type() == QEvent::Quit) {
        const bool accepted = quit();
        event->setAccepted(accepted);
        return true;
    }

    // if (event->type() == QEvent::FileOpen && watched == qApp) {
    //     const QFileOpenEvent* openEvent = static_cast<const QFileOpenEvent*>(event);
    //     const QUrl url = openEvent->url();

    //     if (projectFilesController()->isUrlSupported(url)) {
    //         if (startupScenario()->startupCompleted()) {
    //             dispatcher()->dispatch("file-open", ActionData::make_arg1<QUrl>(url));
    //         } else {
    //             startupScenario()->setStartupScoreFile(project::ProjectFile { url });
    //         }

    //         return true;
    //     }
    // }

    return QObject::eventFilter(watched, event);
}

bool ApplicationActionController::quit()
{
    if (m_quiting) {
        return false;
    }

    m_quiting = true;
    DEFER {
        m_quiting = false;
    };

    constexpr auto quit = true;
    if (!projectFilesController()->closeOpenedProject(quit)) {
        return false;
    }

    QCoreApplication::exit();
    return true;
}

void ApplicationActionController::restart()
{
    if (projectFilesController()->closeOpenedProject()) {
        // if (multiInstancesProvider()->instances().size() == 1) {
        application()->restart();
        // } else {
        // multiInstancesProvider()->quitAllAndRestartLast();

        // QCoreApplication::exit();
        // }
    }
}

void ApplicationActionController::toggleFullScreen()
{
    mainWindow()->toggleFullScreen();
}

void ApplicationActionController::openAboutDialog()
{
    // interactive()->open("audacity://about/audacity");
}

void ApplicationActionController::openAboutQtDialog()
{
    QApplication::aboutQt();
}

void ApplicationActionController::openOnlineHandbookPage()
{
    std::string handbookUrl = configuration()->handbookUrl();
    interactive()->openUrl(handbookUrl);
}

void ApplicationActionController::openAskForHelpPage()
{
    std::string askForHelpUrl = configuration()->askForHelpUrl();
    interactive()->openUrl(askForHelpUrl);
}

void ApplicationActionController::openPreferencesDialog()
{
    //! TODO AU4
    // if (multiInstancesProvider()->isPreferencesAlreadyOpened()) {
    //     multiInstancesProvider()->activateWindowWithOpenedPreferences();
    //     return;
    // }

    interactive()->open("audacity://preferences");
}

void ApplicationActionController::revertToFactorySettings()
{
    std::string title = muse::trc("appshell", "Are you sure you want to revert to factory settings?");
    std::string question = muse::trc("appshell",
                                     "This action will reset all your app preferences and delete all custom palettes and custom shortcuts. "
                                     "The list of recent scores will also be cleared.\n\n"
                                     "This action will not delete any of your scores.");

    int revertBtn = int(muse::IInteractive::Button::Apply);
    muse::IInteractive::Result result = interactive()->warning(title, question,
                                                               { interactive()->buttonData(muse::IInteractive::Button::Cancel),
                                                                 muse::IInteractive::ButtonData(revertBtn, muse::trc("appshell", "Revert"),
                                                                                                true) },
                                                               revertBtn);

    if (result.standardButton() == muse::IInteractive::Button::Cancel) {
        return;
    }

    static constexpr bool KEEP_DEFAULT_SETTINGS = false;
    static constexpr bool NOTIFY_ABOUT_CHANGES = false;
    configuration()->revertToFactorySettings(KEEP_DEFAULT_SETTINGS, NOTIFY_ABOUT_CHANGES);

    title = muse::trc("appshell", "Would you like to restart Audacity now?");
    question = muse::trc("appshell", "Audacity needs to be restarted for these changes to take effect.");

    int restartBtn = int(muse::IInteractive::Button::Apply);
    result = interactive()->question(title, question,
                                     { interactive()->buttonData(muse::IInteractive::Button::Cancel),
                                       muse::IInteractive::ButtonData(restartBtn, muse::trc("appshell", "Restart"), true) },
                                     restartBtn);

    if (result.standardButton() == muse::IInteractive::Button::Cancel) {
        return;
    }

    restart();
}
