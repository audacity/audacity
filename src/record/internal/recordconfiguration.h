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

    bool micMetering() const override;
    void setMicMetering(bool enable) override;
    muse::async::Notification micMeteringChanged() const override;

private:
    muse::async::Notification m_micMeteringChanged;
};
}
