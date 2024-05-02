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
#ifndef AU_PROJECT_PROJECTTHUMBNAILLOADER_H
#define AU_PROJECT_PROJECTTHUMBNAILLOADER_H

#include <QObject>

#include "async/asyncable.h"

#include "irecentfilescontroller.h"

namespace au::project {
class ProjectThumbnailLoader : public QObject, public muse::async::Asyncable
{
    Q_OBJECT;

    // INJECT(IRecentFilesController, recentFilesController)

    Q_PROPERTY(QString projectPath READ projectPath WRITE setProjectPath NOTIFY projectPathChanged)

    Q_PROPERTY(bool isThumbnailValid READ isThumbnailValid NOTIFY thumbnailChanged)
    Q_PROPERTY(QPixmap thumbnail READ thumbnail NOTIFY thumbnailChanged)

public:
    ProjectThumbnailLoader(QObject* parent = nullptr);

    bool isThumbnailValid() const;
    QPixmap thumbnail() const;

    QString projectPath() const;
    void setProjectPath(const QString& projectPath);

signals:
    void thumbnailChanged();

    void projectPathChanged();

private:
    void loadThumbnail();
    void setThumbnail(const QPixmap& thumbnail);

    QPixmap m_thumbnail;
    QString m_projectPath;
};
}

#endif // AU_PROJECT_PROJECTTHUMBNAILLOADER_H
