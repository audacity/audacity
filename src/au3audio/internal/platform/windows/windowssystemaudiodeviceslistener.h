/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>

#include <QObject>
#include <QTimer>

#include "au3audio/isystemaudiodeviceslistener.h"

namespace au::au3audio {
class WindowsSystemAudioDevicesListener : public QObject, public ISystemAudioDevicesListener
{
public:
    WindowsSystemAudioDevicesListener();
    ~WindowsSystemAudioDevicesListener() override;

    void startListening() override;
    void stopListening() override;

    muse::async::Notification systemDevicesChanged() const override;

    //! NOTE Called from a WASAPI notification thread; hops to this object's thread
    void scheduleNotification();

private:
    struct Impl;
    std::unique_ptr<Impl> m_impl;

    QTimer m_debounceTimer;

    muse::async::Notification m_systemDevicesChanged;
};
}
