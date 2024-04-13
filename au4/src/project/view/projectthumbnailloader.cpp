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
#include "projectthumbnailloader.h"

using namespace au::project;

ProjectThumbnailLoader::ProjectThumbnailLoader(QObject* parent)
    : QObject(parent)
{
}

QString ProjectThumbnailLoader::projectPath() const
{
    return m_projectPath;
}

void ProjectThumbnailLoader::setProjectPath(const QString& projectPath)
{
    if (m_projectPath == projectPath) {
        return;
    }

    m_projectPath = projectPath;
    emit projectPathChanged();

    loadThumbnail();
}

bool ProjectThumbnailLoader::isThumbnailValid() const
{
    return !m_thumbnail.isNull();
}

QPixmap ProjectThumbnailLoader::thumbnail() const
{
    return m_thumbnail;
}

void ProjectThumbnailLoader::loadThumbnail()
{
    if (m_projectPath.isEmpty()) {
        setThumbnail(QPixmap());
        return;
    }

    // recentFilesController()->thumbnail(m_scorePath)
    // .onResolve(this, [this](const QPixmap& thumbnail) {
    //     setThumbnail(thumbnail);
    // }).onReject(this, [this](int code, const std::string& error) {
    //     LOGE() << "Could not load thumbnail for " << m_scorePath << ": [" << code << "] " << error;
    //     setThumbnail(QPixmap());
    // });
}

void ProjectThumbnailLoader::setThumbnail(const QPixmap& thumbnail)
{
    m_thumbnail = thumbnail;
    emit thumbnailChanged();
}
