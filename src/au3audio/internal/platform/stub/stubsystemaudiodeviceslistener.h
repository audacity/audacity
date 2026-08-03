/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "au3audio/isystemaudiodeviceslistener.h"

namespace au::au3audio {
class StubSystemAudioDevicesListener : public ISystemAudioDevicesListener
{
public:
    void startListening() override {}
    void stopListening() override {}

    muse::async::Notification systemDevicesChanged() const override { return m_systemDevicesChanged; }

private:
    muse::async::Notification m_systemDevicesChanged;
};
}
