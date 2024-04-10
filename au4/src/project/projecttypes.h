#ifndef AU_PROJECT_PROJECTTYPES_H
#define AU_PROJECT_PROJECTTYPES_H

#include <QUrl>
#include <QString>
#include "global/io/path.h"

namespace au::project {
struct ProjectFile {
    QUrl url;
    QString displayNameOverride = {};

    ProjectFile() = default;

    ProjectFile(const QUrl& url, const QString& displayNameOverride = {})
        : url(url), displayNameOverride(displayNameOverride) {}

    ProjectFile(const muse::io::path_t& path, const QString& displayNameOverride = {})
        : url(path.toQUrl()), displayNameOverride(displayNameOverride) {}

    bool isNull() const
    {
        return url.isEmpty();
    }

    bool isValid() const
    {
        return url.isValid();
    }

    bool hasDisplayName() const
    {
        if (!displayNameOverride.isEmpty()) {
            return true;
        }

        return url.isLocalFile();
    }

    QString displayName(bool includingExtension) const
    {
        if (!displayNameOverride.isEmpty()) {
            return displayNameOverride;
        }

        return muse::io::filename(path(), includingExtension).toQString();
    }

    muse::io::path_t path() const
    {
        assert(url.isEmpty() || url.isLocalFile());
        return muse::io::path_t(url);
    }

    bool operator ==(const ProjectFile& other) const
    {
        return url == other.url && displayNameOverride == other.displayNameOverride;
    }

    bool operator !=(const ProjectFile& other) const
    {
        return !(*this == other);
    }
};
}

#endif // AU_PROJECT_PROJECTTYPES_H
