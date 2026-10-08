/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
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
#include "mainwindowtitleprovider.h"
#include "translation.h"

#include <QFileInfo>
#include <QLocale>

using namespace au::appshell;
using namespace au::project;

namespace {
//! Debug info shown together with Diagnostics > Project > Show sample blocks
static QString storageUsage(const IAudacityProject& project)
{
    const QLocale locale;
    auto format = [&](qint64 bytes) {
        return locale.formattedDataSize(bytes, 1, QLocale::DataSizeTraditionalFormat);
    };

    const QString path = project.path().toQString();
    const qint64 fileBytes = QFileInfo(path).size() + QFileInfo(path + "-wal").size();

    return QString("[blocks: %1 current, %2 incl. undo history | file: %3]")
           .arg(format(project.sampleBlocksUsage(false)), format(project.sampleBlocksUsage(true)), format(fileBytes));
}

static QString appDisplayName()
{
#ifdef AU4_APP_TITLE_VERSION
    return QString::fromUtf8(AU4_APP_TITLE_VERSION);
#else
    return QStringLiteral("Audacity 4");
#endif
}
}

MainWindowTitleProvider::MainWindowTitleProvider(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void MainWindowTitleProvider::load()
{
    update();
    context()->currentProjectChanged().onNotify(this, [this]() {
        if (auto currentProject = context()->currentProject()) {
            currentProject->displayNameChanged().onNotify(this, [this]() {
                update();
            }, muse::async::Asyncable::Mode::SetReplace);

            currentProject->needSave().notification.onNotify(this, [this]() {
                update();
            }, muse::async::Asyncable::Mode::SetReplace);
        }

        update();
    }, muse::async::Asyncable::Mode::SetReplace);

    projectSceneConfiguration()->isSampleBlocksVisibleChanged().onReceive(this, [this](bool) {
        update();
    });

    projectHistory()->historyChanged().onReceive(this, [this](auto) {
        if (projectSceneConfiguration()->isSampleBlocksVisible()) {
            update();
        }
    });
}

QString MainWindowTitleProvider::title() const
{
    return m_title;
}

QString MainWindowTitleProvider::filePath() const
{
    return m_filePath;
}

bool MainWindowTitleProvider::fileModified() const
{
    return m_fileModified;
}

void MainWindowTitleProvider::setTitle(const QString& title)
{
    if (title == m_title) {
        return;
    }

    m_title = title;
    emit titleChanged(title);
}

void MainWindowTitleProvider::setFilePath(const QString& filePath)
{
    if (filePath == m_filePath) {
        return;
    }

    m_filePath = filePath;
    emit filePathChanged(filePath);
}

void MainWindowTitleProvider::setFileModified(bool fileModified)
{
    if (fileModified == m_fileModified) {
        return;
    }

    m_fileModified = fileModified;
    emit fileModifiedChanged(fileModified);
}

void MainWindowTitleProvider::update()
{
    IAudacityProjectPtr project = context()->currentProject();

    if (!project) {
        setTitle(appDisplayName());
        setFilePath("");
        setFileModified(false);
        return;
    }

    const QString projectTitle = project->title().toQString();
    const bool projectModified = project->hasUnsavedChanges();
    setTitle(projectTitle.isEmpty()
             ? appDisplayName()
             //: %1 is the project title, %2 is the modified marker ("*") shown when there are unsaved changes, %3 is the app name
             : muse::qtrc("appshell", "%1 %2 - %3").arg(projectTitle, projectModified ? muse::qtrc("appshell", "*") : QString(),
                                                        appDisplayName()));

    if (projectSceneConfiguration()->isSampleBlocksVisible()) {
        setTitle(m_title + " " + storageUsage(*project));
    }

    setFilePath(project->path().toQString());
    setFileModified(projectModified);
}
