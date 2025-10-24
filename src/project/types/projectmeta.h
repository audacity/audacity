/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QDate>
#include <QPixmap>
#include <QSet>
#include <QString>

#include "io/path.h"
#include "translation.h"

namespace au::project {
struct ProjectMeta
{
    muse::io::path_t filePath;

    QString title;
    QString artist;
    QString album;
    QString trackNumber;
    QString year;
    QString genre;
    QString comments;
    QString software;
    QString copyright;

    QPixmap thumbnail;

    // QString audioComUrl;
    // QString audacityVersion;
    // int audacityRevision = 0;

    QVariantMap additionalTags;

    muse::io::path_t fileName(bool includingExtension = true) const
    {
        return muse::io::filename(filePath, includingExtension);
    }

    bool operator==(const ProjectMeta& other) const
    {
        bool equal = filePath == other.filePath;
        equal &= title == other.title;
        equal &= artist == other.artist;
        equal &= album == other.album;
        equal &= copyright == other.copyright;
        equal &= trackNumber == other.trackNumber;
        equal &= year == other.year;
        equal &= genre == other.genre;
        equal &= comments == other.comments;
        equal &= software == other.software;
        equal &= copyright == other.copyright;

        equal &= thumbnail.toImage() == other.thumbnail.toImage();

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
inline const QString TITLE_TAG("TITLE");
inline const QString ARTIST_TAG("ARTIST");
inline const QString ALBUM_TAG("ALBUM");
inline const QString TRACK_TAG("TRACKNUMBER");
inline const QString YEAR_TAG("YEAR");
inline const QString GENRE_TAG("GENRE");
inline const QString COMMENTS_TAG("COMMENTS");
inline const QString SOFTWARE_TAG("Software");
inline const QString COPYRIGHT_TAG("Copyright");

static const QList<QString> standardTags {
    TITLE_TAG,
    ARTIST_TAG,
    ALBUM_TAG,
    TRACK_TAG,
    YEAR_TAG,
    GENRE_TAG,
    COMMENTS_TAG,
    SOFTWARE_TAG,
    COPYRIGHT_TAG
};

using MemberPtr = QString ProjectMeta::*;

inline static const std::array<MemberPtr, 9> kStdMembers = {
    &project::ProjectMeta::title,
    &project::ProjectMeta::artist,
    &project::ProjectMeta::album,
    &project::ProjectMeta::trackNumber,
    &project::ProjectMeta::year,
    &project::ProjectMeta::genre,
    &project::ProjectMeta::comments,
    &project::ProjectMeta::software,
    &project::ProjectMeta::copyright
};

static std::map<QString, QString> LABEL_MAP {
    { TITLE_TAG,    muse::qtrc("metadata", "Title") },
    { ARTIST_TAG,   muse::qtrc("metadata", "Artist") },
    { ALBUM_TAG,    muse::qtrc("metadata", "Album") },
    { TRACK_TAG,    muse::qtrc("metadata", "Track number") },
    { YEAR_TAG,     muse::qtrc("metadata", "Year") },
    { GENRE_TAG,    muse::qtrc("metadata", "Genre") },
    { COMMENTS_TAG, muse::qtrc("metadata", "Comments") },
    { SOFTWARE_TAG, muse::qtrc("metadata", "Software") },
    { COPYRIGHT_TAG, muse::qtrc("metadata", "Copyright") },
};

// Cloud-related tags
// TODO: don't use the score's meta tags to use this information
// https://github.com/musescore/MuseScore/issues/17560
// https://github.com/musescore/MuseScore/issues/17561
// inline const QString SOURCE_TAG("source");
// inline const QString SOURCE_REVISION_ID_TAG("sourceRevisionId");
// inline const QString AUDIO_COM_URL_TAG("audioComUrl");

inline bool isStandardTag(const QString& tag)
{
    return standardTags.contains(tag);
}
}
