/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../irecordconfiguration.h"

namespace au::record {
class RecordConfiguration : public IRecordConfiguration
{
public:
    void init();

    muse::draw::Color recordColor() const override;

    bool isMicMeteringOn() const override;
    void setIsMicMeteringOn(bool enable) override;
    muse::async::Notification isMicMeteringOnChanged() const override;

private:
    muse::async::Notification m_isMicMeteringOnChanged;
};
}
