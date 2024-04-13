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
#ifndef AU_PROJECT_PROJECTTYPES_H
#define AU_PROJECT_PROJECTTYPES_H

#include <variant>

#include <QString>
#include <QUrl>

#include "io/path.h"
#include "progress.h"
#include "log.h"

#include "projectmeta.h"

#include "cloud/cloudtypes.h"

namespace au::project {
struct ProjectCreateOptions
{
    QString title;
    QString subtitle;
    QString composer;
    QString lyricist;
    QString copyright;

    muse::io::path_t templatePath;
};

enum class SaveMode
{
    Save,
    SaveAs,
    SaveCopy,
    SaveSelection,
    AutoSave
};

enum class SaveLocationType
{
    Undefined,
    Local,
    Cloud
};

struct CloudProjectInfo {
    QUrl sourceUrl;
    int revisionId = 0;
    QString name;

    muse::cloud::Visibility visibility = muse::cloud::Visibility::Private;

    bool isValid() const
    {
        return !sourceUrl.isEmpty();
    }
};

struct CloudAudioInfo {
    QString name;
    QUrl url;
    muse::cloud::Visibility visibility = muse::cloud::Visibility::Private;
    bool replaceExisting = false;

    bool isValid() const
    {
        return !name.isEmpty();
    }
};

struct SaveLocation
{
    SaveLocationType type = SaveLocationType::Undefined;

    std::variant<muse::io::path_t, CloudProjectInfo> data;

    bool isLocal() const
    {
        return type == SaveLocationType::Local
               && std::holds_alternative<muse::io::path_t>(data);
    }

    bool isCloud() const
    {
        return type == SaveLocationType::Cloud
               && std::holds_alternative<CloudProjectInfo>(data);
    }

    bool isValid() const
    {
        return isLocal() || isCloud();
    }

    const muse::io::path_t& localPath() const
    {
        IF_ASSERT_FAILED(isLocal()) {
            static muse::io::path_t null;
            return null;
        }

        return std::get<muse::io::path_t>(data);
    }

    const CloudProjectInfo& cloudInfo() const
    {
        IF_ASSERT_FAILED(isCloud()) {
            static CloudProjectInfo null;
            return null;
        }

        return std::get<CloudProjectInfo>(data);
    }

    SaveLocation() = default;

    SaveLocation(SaveLocationType type, const std::variant<muse::io::path_t, CloudProjectInfo>& data = {})
        : type(type), data(data) {}

    SaveLocation(const muse::io::path_t& localPath)
        : type(SaveLocationType::Local), data(localPath) {}

    SaveLocation(const CloudProjectInfo& cloudInfo)
        : type(SaveLocationType::Cloud), data(cloudInfo) {}
};

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
        return url == other.url
               && displayNameOverride == other.displayNameOverride;
    }

    bool operator !=(const ProjectFile& other) const
    {
        return !(*this == other);
    }
};

using ProjectFilesList = std::vector<ProjectFile>;

struct RecentFile {
    muse::io::path_t path;
    QString displayNameOverride = {};

    RecentFile() = default;

    RecentFile(const muse::io::path_t& path, const QString& displayNameOverride = {})
        : path(path), displayNameOverride(displayNameOverride) {}

    static RecentFile fromProjectFile(const ProjectFile& projectFile)
    {
        return RecentFile(projectFile.path(), projectFile.displayNameOverride);
    }

    ProjectFile toProjectFile() const
    {
        return ProjectFile(path, displayNameOverride);
    }

    bool isValid() const
    {
        return !path.empty();
    }

    QString displayName(bool includingExtension) const
    {
        if (!displayNameOverride.isEmpty()) {
            return displayNameOverride;
        }

        return muse::io::filename(path, includingExtension).toQString();
    }

    bool operator ==(const RecentFile& other) const
    {
        return path == other.path
               && displayNameOverride == other.displayNameOverride;
    }

    bool operator !=(const RecentFile& other) const
    {
        return !(*this == other);
    }
};

using RecentFilesList = std::vector<RecentFile>;

struct Template
{
    QString categoryTitle;
    ProjectMeta meta;
    bool isCustom = false;
};

using Templates = QList<Template>;

struct ProjectBeingDownloaded {
    int scoreId = 0;
    muse::ProgressPtr progress;
};

class GenerateAudioTimePeriod
{
    Q_GADGET

public:
    enum class Type {
        Never = 0,
        Always,
        AfterCertainNumberOfSaves
    };
    Q_ENUM(Type)
};

using GenerateAudioTimePeriodType = GenerateAudioTimePeriod::Type;
}

#endif // AU_PROJECT_PROJECTTYPES_H
