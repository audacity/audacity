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

#include "cloudprojectstatuswatcher.h"

#include "log.h"

using namespace au::project;

CloudScoreStatusWatcher::CloudScoreStatusWatcher(QObject* parent)
    : QObject(parent)
{
}

void CloudScoreStatusWatcher::load(int scoreId)
{
    m_scoreId = scoreId;

    if (!m_scoreId) {
        return;
    }

    onProjectBeingDownloadedChanged();
    projectFilesController()->projectBeingDownloadedChanged().onNotify(this, [this] {
        onProjectBeingDownloadedChanged();
    });
}

bool CloudScoreStatusWatcher::isDownloadedAndUpToDate() const
{
    NOT_IMPLEMENTED;
    return true;
}

void CloudScoreStatusWatcher::onProjectBeingDownloadedChanged()
{
    ProjectBeingDownloaded download = projectFilesController()->projectBeingDownloaded();
    if (download.scoreId != m_scoreId) {
        clearProgress();
        return;
    }

    download.progress->progressChanged().onReceive(this, [this](int64_t current, int64_t total, const std::string&) {
        m_progressCurrent = current;
        m_progressTotal = total;
        emit progressChanged();
    });

    download.progress->finished().onReceive(this, [this](const muse::ProgressResult&) {
        clearProgress();
    });
}

void CloudScoreStatusWatcher::clearProgress()
{
    m_progressCurrent = 0;
    m_progressTotal = 0;
    emit progressChanged();
}

bool CloudScoreStatusWatcher::isProgress() const
{
    return m_progressTotal > 0;
}

int CloudScoreStatusWatcher::progressTotal() const
{
    return m_progressTotal;
}

int CloudScoreStatusWatcher::progressCurrent() const
{
    return m_progressCurrent;
}

void CloudScoreStatusWatcher::cancel()
{
    if (!m_scoreId) {
        return;
    }

    ProjectBeingDownloaded download = projectFilesController()->projectBeingDownloaded();
    if (download.scoreId != m_scoreId) {
        return;
    }

    download.progress->cancel();
}
