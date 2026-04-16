/*
* Audacity: A Digital Audio Editor
*/
#include "downloadmanager.h"

#include <QBuffer>
#include <QString>
#include <QUrl>

#include "framework/global/async/async.h"
#include "framework/global/runtime.h"
#include "framework/global/types/bytearray.h"

#include "au3cloud/cloudtypes.h"
#include "thirdparty/kors_logger/src/log_base.h"

using namespace au::au3cloud;

void DownloadManager::scheduleDownloads(const std::vector<DownloadRequest>& requests)
{
    for (const auto& req : requests) {
        if (req.url.empty()) {
            continue;
        }

        if (filesystem()->exists(req.localPath)) {
            continue;
        }

        {
            std::lock_guard lock(m_mutex);
            if (!m_inProgressIds.insert(req.id).second) {
                //Already in progress
                continue;
            }
        }

        muse::async::Async::call(this, [this, id = req.id, url = req.url, path = req.localPath]() {
            startDownload(id, url, path);
        }, muse::runtime::mainThreadId());
    }
}

muse::async::Channel<std::string, muse::io::path_t> DownloadManager::downloadCompleted() const
{
    return m_downloadCompleted;
}

void DownloadManager::startDownload(const std::string& id, const std::string& url, const muse::io::path_t& destPath)
{
    auto manager = networkManagerCreator()->makeNetworkManager();
    auto buffer = std::make_shared<QBuffer>();

    muse::network::RequestHeaders headers;
    headers.rawHeaders["User-Agent"] = "Audacity";

    auto retVal = manager->get(QUrl(QString::fromStdString(url)),
                               std::static_pointer_cast<QIODevice>(buffer),
                               headers);

    {
        std::lock_guard lock(m_mutex);

        if (!retVal.ret) {
            m_inProgressIds.erase(id);
            return;
        }

        m_activeManagers[id] = std::move(manager);
    }

    retVal.val.finished().onReceive(this,
                                    [this, id, destPath, buffer](const muse::ProgressResult& res) {
        {
            std::lock_guard lock(m_mutex);
            m_activeManagers.erase(id);
            m_inProgressIds.erase(id);
        }

        if (!res.ret) {
            LOGE() << "Download failed for id " << id;
            return;
        }

        const muse::ByteArray data = muse::ByteArray::fromQByteArray(buffer->data());
        const muse::Ret writeRet = filesystem()->writeFile(destPath, data);
        if (writeRet) {
            m_downloadCompleted.send(id, destPath);
        } else {
            LOGE() << "Failed to write downloaded file for id " << id;
        }
    });
}
