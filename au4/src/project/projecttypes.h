#ifndef MU_PROJECT_PROJECTTYPES_H
#define MU_PROJECT_PROJECTTYPES_H

#include <QUrl>
#include <QString>
#include "global/io/path.h"

namespace mu::project {
struct ProjectFile {
    QUrl url;
    QString displayNameOverride = {};

    ProjectFile() = default;

    ProjectFile(const QUrl& url, const QString& displayNameOverride = {})
        : url(url), displayNameOverride(displayNameOverride) {}

    ProjectFile(const io::path_t& path, const QString& displayNameOverride = {})
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

        return io::filename(path(), includingExtension).toQString();
    }

    io::path_t path() const
    {
        assert(url.isEmpty() || url.isLocalFile());

        return io::path_t(url);
    }

    bool operator ==(const ProjectFile& other) const
    {
        return url == other.url
               && displayNameOverride == other.displayNameOverride;
    }

    bool operator !=(const ProjectFile& other) const
    {
        return !(*this == other);
    }
};
}

#endif // MU_PROJECT_PROJECTTYPES_H
