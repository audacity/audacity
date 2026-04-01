/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <map>
#include <mutex>
#include <set>
#include <string>

#include "framework/global/async/asyncable.h"
#include "framework/global/async/channel.h"
#include "framework/global/io/ifilesystem.h"
#include "framework/global/io/path.h"
#include "framework/global/modularity/ioc.h"
#include "framework/network/inetworkmanagercreator.h"

#include "au3cloud/cloudtypes.h"

namespace au::au3cloud {
class DownloadManager : public muse::async::Asyncable
{
    muse::GlobalInject<muse::network::INetworkManagerCreator> networkManagerCreator;
    muse::GlobalInject<muse::io::IFileSystem> filesystem;

public:
    void scheduleDownloads(const std::vector<DownloadRequest>& requests);

    muse::async::Channel<std::string, muse::io::path_t> downloadCompleted() const;

private:
    void startDownload(const std::string& id, const std::string& url, const muse::io::path_t& destPath);

    muse::async::Channel<std::string, muse::io::path_t> m_downloadCompleted;

    std::map<std::string, muse::network::INetworkManagerPtr> m_activeManagers;

    std::set<std::string> m_inProgressIds;
    std::mutex m_mutex;
};
}
