/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * Music Composition & Notation
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
#ifndef AU_PROJECT_CLOUDSCORESTATUSWATCHER_H
#define AU_PROJECT_CLOUDSCORESTATUSWATCHER_H

#include <QObject>

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "iprojectfilescontroller.h"

namespace au::project {
class CloudScoreStatusWatcher : public QObject, muse::async::Asyncable
{
    Q_OBJECT

    muse::Inject<IProjectFilesController> projectFilesController;

    Q_PROPERTY(bool isDownloadedAndUpToDate READ isDownloadedAndUpToDate CONSTANT)

    Q_PROPERTY(bool isProgress READ isProgress NOTIFY progressChanged)
    Q_PROPERTY(int progressTotal READ progressTotal NOTIFY progressChanged)
    Q_PROPERTY(int progressCurrent READ progressCurrent NOTIFY progressChanged)

public:
    explicit CloudScoreStatusWatcher(QObject* parent = nullptr);

    Q_INVOKABLE void load(int scoreId);

    bool isDownloadedAndUpToDate() const;

    bool isProgress() const;
    int progressTotal() const;
    int progressCurrent() const;

    Q_INVOKABLE void cancel();

signals:
    void progressChanged();

private:
    void onProjectBeingDownloadedChanged();
    void clearProgress();

    int m_scoreId = 0;

    int m_progressTotal = 0;
    int m_progressCurrent = 0;
};
}

#endif // AU_PROJECT_CLOUDSCORESTATUSWATCHER_H
