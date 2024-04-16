/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2023 MuseScore BVBA and others
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
#ifndef MU_PROJECT_SCORETHUMBNAILLOADER_H
#define MU_PROJECT_SCORETHUMBNAILLOADER_H

#include <QObject>

#include "async/asyncable.h"

#include "irecentfilescontroller.h"

namespace mu::project {
class ScoreThumbnailLoader : public QObject, public muse::async::Asyncable
{
    Q_OBJECT;

    INJECT(IRecentFilesController, recentFilesController)

    Q_PROPERTY(QString scorePath READ scorePath WRITE setScorePath NOTIFY scorePathChanged)

    Q_PROPERTY(bool isThumbnailValid READ isThumbnailValid NOTIFY thumbnailChanged)
    Q_PROPERTY(QPixmap thumbnail READ thumbnail NOTIFY thumbnailChanged)

public:
    ScoreThumbnailLoader(QObject* parent = nullptr);

    bool isThumbnailValid() const;
    QPixmap thumbnail() const;

    QString scorePath() const;
    void setScorePath(const QString& scorePath);

signals:
    void thumbnailChanged();

    void scorePathChanged();

private:
    void loadThumbnail();
    void setThumbnail(const QPixmap& thumbnail);

    QPixmap m_thumbnail;
    QString m_scorePath;
};
}

#endif // MU_PROJECT_SCORETHUMBNAILLOADER_H
