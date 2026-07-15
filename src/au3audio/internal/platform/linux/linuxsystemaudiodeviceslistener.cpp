/*
* Audacity: A Digital Audio Editor
*/

#include "linuxsystemaudiodeviceslistener.h"

#include <libudev.h>

#include "log.h"

using namespace au::au3audio;

namespace {
constexpr int DEBOUNCE_INTERVAL_MS = 500;
}

LinuxSystemAudioDevicesListener::LinuxSystemAudioDevicesListener()
{
    m_debounceTimer.setSingleShot(true);
    m_debounceTimer.setInterval(DEBOUNCE_INTERVAL_MS);
    QObject::connect(&m_debounceTimer, &QTimer::timeout, this, [this]() {
        m_systemDevicesChanged.notify();
    });
}

LinuxSystemAudioDevicesListener::~LinuxSystemAudioDevicesListener()
{
    stopListening();
}

void LinuxSystemAudioDevicesListener::startListening()
{
    if (m_monitor) {
        return;
    }

    m_udev = udev_new();
    if (!m_udev) {
        LOGE() << "failed to create the udev context";
        return;
    }

    m_monitor = udev_monitor_new_from_netlink(m_udev, "udev");
    if (!m_monitor) {
        LOGE() << "failed to create the udev monitor";
        udev_unref(m_udev);
        m_udev = nullptr;
        return;
    }

    udev_monitor_filter_add_match_subsystem_devtype(m_monitor, "sound", nullptr);
    udev_monitor_enable_receiving(m_monitor);

    m_notifier = std::make_unique<QSocketNotifier>(udev_monitor_get_fd(m_monitor), QSocketNotifier::Read);
    QObject::connect(m_notifier.get(), &QSocketNotifier::activated, this, [this]() {
        // drain the queued events; what changed does not matter, and bursts
        // (a card exposes several sound devices) collapse into one notification
        while (struct udev_device* device = udev_monitor_receive_device(m_monitor)) {
            udev_device_unref(device);
        }
        m_debounceTimer.start();
    });
}

void LinuxSystemAudioDevicesListener::stopListening()
{
    m_notifier.reset();

    if (m_monitor) {
        udev_monitor_unref(m_monitor);
        m_monitor = nullptr;
    }

    if (m_udev) {
        udev_unref(m_udev);
        m_udev = nullptr;
    }
}

muse::async::Notification LinuxSystemAudioDevicesListener::systemDevicesChanged() const
{
    return m_systemDevicesChanged;
}
