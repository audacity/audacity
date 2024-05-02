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
#ifndef AU_PROJECT_PROJECTMETA_H
#define AU_PROJECT_PROJECTMETA_H

#include <QDate>
#include <QPixmap>
#include <QSet>
#include <QString>

#include "io/path.h"

namespace au::project {
struct ProjectMeta
{
    muse::io::path_t filePath;

    QString title;
    QString subtitle;
    QString arranger;
    QString copyright;
    QDate creationDate;

    size_t partsCount = 0;
    QPixmap thumbnail;

    QString source;
    QString audioComUrl;
    QString platform;
    QString audacityVersion;
    int audacityRevision = 0;

    QVariantMap additionalTags;

    muse::io::path_t fileName(bool includingExtension = true) const
    {
        return muse::io::filename(filePath, includingExtension);
    }

    bool operator==(const ProjectMeta& other) const
    {
        bool equal = filePath == other.filePath;
        equal &= title == other.title;
        equal &= subtitle == other.subtitle;
        equal &= arranger == other.arranger;
        equal &= copyright == other.copyright;
        equal &= creationDate == other.creationDate;

        equal &= partsCount == other.partsCount;
        equal &= thumbnail.toImage() == other.thumbnail.toImage();

        equal &= source == other.source;
        equal &= audioComUrl == other.audioComUrl;
        equal &= platform == other.platform;
        equal &= audacityVersion == other.audacityVersion;
        equal &= audacityRevision == other.audacityRevision;

        equal &= additionalTags == other.additionalTags;

        return equal;
    }

    bool operator!=(const ProjectMeta& other) const
    {
        return !(*this == other);
    }
};

using ProjectMetaList = QList<ProjectMeta>;

// Tags
inline const QString WORK_TITLE_TAG("workTitle");
inline const QString WORK_NUMBER_TAG("workNumber");
inline const QString SUBTITLE_TAG("subtitle");
inline const QString ARRANGER_TAG("arranger");
inline const QString COPYRIGHT_TAG("copyright");
inline const QString MOVEMENT_TITLE_TAG("movementTitle");
inline const QString MOVEMENT_NUMBER_TAG("movementNumber");
inline const QString CREATION_DATE_TAG("creationDate");
inline const QString PLATFORM_TAG("platform");

// Cloud-related tags
// TODO: don't use the score's meta tags to use this information
// https://github.com/musescore/MuseScore/issues/17560
// https://github.com/musescore/MuseScore/issues/17561
inline const QString SOURCE_TAG("source");
inline const QString SOURCE_REVISION_ID_TAG("sourceRevisionId");
inline const QString AUDIO_COM_URL_TAG("audioComUrl");

inline bool isStandardTag(const QString& tag)
{
    static const QSet<QString> standardTags {
        WORK_TITLE_TAG,
        WORK_NUMBER_TAG,
        SUBTITLE_TAG,
        ARRANGER_TAG,
        COPYRIGHT_TAG,
        MOVEMENT_TITLE_TAG,
        MOVEMENT_NUMBER_TAG,
        CREATION_DATE_TAG,
        PLATFORM_TAG,

        SOURCE_TAG,
        SOURCE_REVISION_ID_TAG,
        AUDIO_COM_URL_TAG
    };

    return standardTags.contains(tag);
}

inline bool isRepresentedInProjectMeta(const QString& tag)
{
    static const QSet<QString> projectMetaTags{
        WORK_TITLE_TAG,
        SUBTITLE_TAG,
        ARRANGER_TAG,
        COPYRIGHT_TAG,
        CREATION_DATE_TAG,
        PLATFORM_TAG,

        SOURCE_TAG,
        AUDIO_COM_URL_TAG
    };

    return projectMetaTags.contains(tag);
}
}

#endif // AU_PROJECT_PROJECTMETA_H
