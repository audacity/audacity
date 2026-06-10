/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "async/asyncable.h"
#include "modularity/ioc.h"
#include "network/inetworkmanagercreator.h"
#include "update/iupdaterequestparamsprovider.h"

#include "iusageinfo.h"

namespace au::usageinfo {
class UsageInfoService : public IUsageInfo, public muse::update::IUpdateRequestParamsProvider, public muse::async::Asyncable
{
    muse::GlobalInject<muse::network::INetworkManagerCreator> networkManagerCreator;

public:
    void init();

    void setSendAnonymousUsageInfo(bool allow) override;
    bool getSendAnonymousUsageInfo() const override;

    std::string instanceId() const override;

    void setUserId(const std::string& userId) override;

    muse::async::Notification usageInfoChanged() const override;

    std::vector<std::pair<std::string, std::string> > updateRequestParams() const override;

private:
    void ensureInstanceIdCreated();
    void sendOptOutRequest();

    muse::async::Notification m_usageInfoChanged;
    muse::network::INetworkManagerPtr m_networkManager;
    std::string m_userId;
};
}
