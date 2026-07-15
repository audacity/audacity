/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>

#include <QObject>
#include <QSocketNotifier>
#include <QTimer>

#include "au3audio/isystemaudiodeviceslistener.h"

struct udev;
struct udev_monitor;

namespace au::au3audio {
//! NOTE Detects sound devices being plugged/unplugged via udev; a change
//! of the default device inside the sound server (PulseAudio/PipeWire)
//! is not visible to udev and therefore not detected
class LinuxSystemAudioDevicesListener : public QObject, public ISystemAudioDevicesListener
{
public:
    LinuxSystemAudioDevicesListener();
    ~LinuxSystemAudioDevicesListener() override;

    void startListening() override;
    void stopListening() override;

    muse::async::Notification systemDevicesChanged() const override;

private:
    struct udev* m_udev = nullptr;
    struct udev_monitor* m_monitor = nullptr;
    std::unique_ptr<QSocketNotifier> m_notifier;

    QTimer m_debounceTimer;

    muse::async::Notification m_systemDevicesChanged;
};
}
