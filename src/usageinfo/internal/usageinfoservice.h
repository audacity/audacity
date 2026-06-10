/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "async/asyncable.h"
#include "modularity/ioc.h"
#include "network/inetworkmanagercreator.h"
#include "update/iupdaterequestparamsprovider.h"

#include "au3cloud/iauthorization.h"

#include "iusageinfo.h"

namespace au::usageinfo {
class UsageInfoService : public IUsageInfo, public muse::update::IUpdateRequestParamsProvider, public muse::async::Asyncable
{
    muse::GlobalInject<muse::network::INetworkManagerCreator> networkManagerCreator;
    muse::GlobalInject<au3cloud::IAuthorization> authorization;

public:
    void init();

    void setSendAnonymousUsageInfo(bool allow) override;
    bool getSendAnonymousUsageInfo() const override;

    std::string instanceId() const override;

    muse::async::Notification usageInfoChanged() const override;

    std::vector<std::pair<std::string, std::string> > updateRequestParams() const override;

private:
    void ensureInstanceIdCreated();
    void sendOptOutRequest();

    muse::async::Notification m_usageInfoChanged;
    muse::network::INetworkManagerPtr m_networkManager;
};
}
