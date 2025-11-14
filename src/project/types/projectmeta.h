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

    std::string artist;
    std::string trackTitle;
    std::string album;
    std::string trackNumber;
    std::string year;
    std::string comments;

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
        equal &= trackTitle == other.trackTitle;
        equal &= artist == other.artist;
        equal &= album == other.album;
        equal &= trackNumber == other.trackNumber;
        equal &= year == other.year;
        equal &= comments == other.comments;

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
inline const std::string ARTIST_TAG("Artist name");
inline const std::string TRACK_TITLE_TAG("Track title");
inline const std::string ALBUM_TAG("Album title");
inline const std::string TRACK_NUMBER_TAG("Track number");
inline const std::string YEAR_TAG("Year");
inline const std::string COMMENTS_TAG("Comments");

static const QList<std::string> standardTags {
    ARTIST_TAG,
    TRACK_TITLE_TAG,
    ALBUM_TAG,
    TRACK_NUMBER_TAG,
    YEAR_TAG,
    COMMENTS_TAG,
};

using MemberPtr = std::string ProjectMeta::*;

inline static const std::array<MemberPtr, 6> kStdMembers = {
    &project::ProjectMeta::artist,
    &project::ProjectMeta::trackTitle,
    &project::ProjectMeta::album,
    &project::ProjectMeta::trackNumber,
    &project::ProjectMeta::year,
    &project::ProjectMeta::comments,
};

static std::map<std::string, std::string> LABEL_MAP {
    { ARTIST_TAG,       muse::trc("metadata", "Artist name") },
    { TRACK_TITLE_TAG,  muse::trc("metadata", "Track title") },
    { ALBUM_TAG,        muse::trc("metadata", "Album title") },
    { TRACK_NUMBER_TAG, muse::trc("metadata", "Track number") },
    { YEAR_TAG,         muse::trc("metadata", "Year") },
    { COMMENTS_TAG,     muse::trc("metadata", "Comments") },
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
