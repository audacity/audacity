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

using namespace au::appshell;
using namespace au::project;

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
        setTitle(muse::qtrc("appshell", "Audacity 4"));
        setFilePath("");
        setFileModified(false);
        return;
    }

    const QString projectTitle = project->title().toQString();
    const bool projectModified = project->hasUnsavedChanges();
    setTitle(projectTitle.isEmpty()
             ? muse::qtrc("appshell", "Audacity 4")
             : muse::qtrc("appshell", "%1 %2- Audacity 4").arg(projectTitle, projectModified ? QString("* ") : QString()));

    setFilePath(project->path().toQString());
    setFileModified(projectModified);
}
