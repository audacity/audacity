/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/modularity/ioc.h"
#include "framework/ui/iuiconfiguration.h"

#include "../irecordconfiguration.h"

namespace au::record {
class RecordConfiguration : public IRecordConfiguration
{
    muse::GlobalInject<muse::ui::IUiConfiguration> uiConfiguration;

public:
    void init();

    bool isMicMeteringOn() const override;
    void setIsMicMeteringOn(bool enable) override;
    muse::async::Notification isMicMeteringOnChanged() const override;

    bool isInputMonitoringOn() const override;
    void setIsInputMonitoringOn(bool enable) override;
    muse::async::Notification isInputMonitoringOnChanged() const override;

private:
    muse::async::Notification m_isMicMeteringOnChanged;
    muse::async::Notification m_isInputMonitoringOnChanged;
};
}
