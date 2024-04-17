/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-Studio-CLA-applies
 *
 * MuseScore Studio
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore Limited
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
#ifndef MU_PROJECT_RECENTFILESCONTROLLER_H
#define MU_PROJECT_RECENTFILESCONTROLLER_H

#include "irecentfilescontroller.h"

#include <mutex>
#include <map>

#include "async/asyncable.h"
#include "async/promise.h"

#include "modularity/ioc.h"
#include "iprojectconfiguration.h"
#include "imscmetareader.h"
#include "io/ifilesystem.h"
#include "multiinstances/imultiinstancesprovider.h"

namespace mu::project {
class RecentFilesController : public IRecentFilesController, public muse::async::Asyncable
{
    INJECT(IProjectConfiguration, configuration)
    INJECT(IMscMetaReader, mscMetaReader)
    INJECT(muse::io::IFileSystem, fileSystem)
    INJECT(muse::mi::IMultiInstancesProvider, multiInstancesProvider)

public:
    void init();

    const RecentFilesList& recentFilesList() const override;
    muse::async::Notification recentFilesListChanged() const override;

    void prependRecentFile(const RecentFile& file) override;
    void moveRecentFile(const muse::io::path_t& before, const RecentFile& after) override;
    void clearRecentFiles() override;

    muse::async::Promise<QPixmap> thumbnail(const muse::io::path_t& file) const override;

protected:
    virtual void prependPlatformRecentFile(const muse::io::path_t& path);
    virtual void clearPlatformRecentFiles();

private:
    void loadRecentFilesList();
    void removeNonexistentFiles();
    void setRecentFilesList(const RecentFilesList& list, bool saveAndNotify);
    void saveRecentFilesList();

    void cleanUpThumbnailCache(const RecentFilesList& files);

    mutable bool m_dirty = true;
    mutable RecentFilesList m_recentFilesList;
    muse::async::Notification m_recentFilesListChanged;
    mutable bool m_isSaving = false;

    struct CachedThumbnail {
        QPixmap thumbnail;
        muse::DateTime lastModified;
    };

    mutable std::mutex m_thumbnailCacheMutex;
    mutable std::map<muse::io::path_t, CachedThumbnail> m_thumbnailCache;
};
}

#endif // MU_PROJECT_RECENTFILESCONTROLLER_H
