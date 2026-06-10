/*
* Audacity: A Digital Audio Editor
*/
#include "usageinfoservice.h"

#include <QBuffer>
#include <QUuid>

#include "framework/global/settings.h"
#include "framework/global/log.h"

using namespace muse;
using namespace au::usageinfo;

static const Settings::Key SEND_ANONYMOUS_USAGE_INFO_KEY("usageinfo", "application/sendAnonymousUsageInfo");
static const Settings::Key INSTANCE_ID_KEY("usageinfo", "application/instanceId");

static const QUrl UUID_OPT_OUT_URL("https://api.audio.com/analytics/audacity-uuid/opt-out");

void UsageInfoService::init()
{
    settings()->setDefaultValue(SEND_ANONYMOUS_USAGE_INFO_KEY, Val(false));
    settings()->setDefaultValue(INSTANCE_ID_KEY, Val(std::string()));
}

void UsageInfoService::setSendAnonymousUsageInfo(bool allow)
{
    if (allow == getSendAnonymousUsageInfo()) {
        return;
    }

    settings()->setSharedValue(SEND_ANONYMOUS_USAGE_INFO_KEY, Val(allow));

    if (allow) {
        ensureInstanceIdCreated();
    } else {
        sendOptOutRequest();
    }

    m_usageInfoChanged.notify();
}

bool UsageInfoService::getSendAnonymousUsageInfo() const
{
    return settings()->value(SEND_ANONYMOUS_USAGE_INFO_KEY).toBool();
}

std::string UsageInfoService::instanceId() const
{
    return settings()->value(INSTANCE_ID_KEY).toString();
}

muse::async::Notification UsageInfoService::usageInfoChanged() const
{
    return m_usageInfoChanged;
}

std::vector<std::pair<std::string, std::string> > UsageInfoService::updateRequestParams() const
{
    std::vector<std::pair<std::string, std::string> > params;

    if (!getSendAnonymousUsageInfo()) {
        return params;
    }

    params.emplace_back("audacity-instance-id", instanceId());

    if (authorization()) {
        const std::string userId = authorization()->accountInfo().id;
        if (!userId.empty()) {
            params.emplace_back("user_id", userId);
        }
    }

    return params;
}

void UsageInfoService::ensureInstanceIdCreated()
{
    if (!instanceId().empty()) {
        return;
    }

    const QByteArray uuid = QUuid::createUuid().toByteArray(QUuid::Id128);
    settings()->setSharedValue(INSTANCE_ID_KEY, Val(uuid.toStdString()));
}

void UsageInfoService::sendOptOutRequest()
{
    const std::string uuid = instanceId();
    if (uuid.empty()) {
        return;
    }

    if (!m_networkManager) {
        m_networkManager = networkManagerCreator()->makeNetworkManager();
    }

    auto outgoingData = std::make_shared<QBuffer>();
    outgoingData->setData(QByteArray::fromStdString("{\"uuid\":\"" + uuid + "\"}"));

    network::RequestHeaders headers;
    headers.knownHeaders[QNetworkRequest::ContentTypeHeader] = "application/json";

    RetVal<Progress> progress = m_networkManager->post(UUID_OPT_OUT_URL, outgoingData, nullptr, headers);
    if (!progress.ret) {
        LOGW() << "UUID opt-out request failed: " << progress.ret.toString();
        return;
    }

    progress.val.finished().onReceive(this, [outgoingData](const ProgressResult& res) {
        if (!res.ret) {
            LOGW() << "UUID opt-out request failed: " << res.ret.toString();
        }
    });
}
