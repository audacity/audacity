/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/async/asyncable.h"
#include "framework/update/iupdaterequestparamsprovider.h"

#include "framework/global/modularity/ioc.h"
#include "framework/network/inetworkmanagercreator.h"
#include "framework/network/inetworkconfiguration.h"

#include "iusageinfo.h"

namespace au::usageinfo {
class UsageInfoService : public IUsageInfo, public muse::update::IUpdateRequestParamsProvider, public muse::async::Asyncable
{
    muse::GlobalInject<muse::network::INetworkManagerCreator> networkManagerCreator;
    muse::GlobalInject<muse::network::INetworkConfiguration> networkConfiguration;

public:
    void init();

    bool isUsageInfoAvailable() const override;

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
