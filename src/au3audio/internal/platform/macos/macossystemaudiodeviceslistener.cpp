/*
* Audacity: A Digital Audio Editor
*/

#include "macossystemaudiodeviceslistener.h"

#include <CoreAudio/CoreAudio.h>

#include <algorithm>

using namespace au::au3audio;

namespace {
constexpr int DEBOUNCE_INTERVAL_MS = 500;

//! NOTE Device plugged/unplugged and system default input/output changed
const AudioObjectPropertyAddress LISTENED_PROPERTIES[] = {
    { kAudioHardwarePropertyDevices, kAudioObjectPropertyScopeGlobal, kAudioObjectPropertyElementMaster },
    { kAudioHardwarePropertyDefaultInputDevice, kAudioObjectPropertyScopeGlobal, kAudioObjectPropertyElementMaster },
    { kAudioHardwarePropertyDefaultOutputDevice, kAudioObjectPropertyScopeGlobal, kAudioObjectPropertyElementMaster },
};

OSStatus devicesChangedCallback(AudioObjectID, UInt32 count, const AudioObjectPropertyAddress* addresses, void* clientData)
{
    auto* self = reinterpret_cast<MacosSystemAudioDevicesListener*>(clientData);
    const bool onlyDeviceList = std::all_of(addresses, addresses + count, [](const AudioObjectPropertyAddress& address) {
        return address.mSelector == kAudioHardwarePropertyDevices;
    });
    QMetaObject::invokeMethod(self, [self, onlyDeviceList]() {
        self->onSystemDevicesChanged(onlyDeviceList);
    }, Qt::QueuedConnection);

    return noErr;
}

bool isPrivateAggregate(AudioObjectID device)
{
    const AudioObjectPropertyAddress address {
        kAudioAggregateDevicePropertyComposition, kAudioObjectPropertyScopeGlobal, kAudioObjectPropertyElementMaster
    };
    CFDictionaryRef composition = nullptr;
    UInt32 size = sizeof(composition);
    if (AudioObjectGetPropertyData(device, &address, 0, nullptr, &size, &composition) != noErr || !composition) {
        return false;
    }

    int isPrivate = 0;
    const auto value = static_cast<CFNumberRef>(CFDictionaryGetValue(composition, CFSTR(kAudioAggregateDeviceIsPrivateKey)));
    if (value) {
        CFNumberGetValue(value, kCFNumberIntType, &isPrivate);
    }
    CFRelease(composition);
    return isPrivate != 0;
}

//! A private aggregate is visible only to the process that made it, and PortAudio
//! makes one for each stream on two devices: it is not a system change
std::vector<uint32_t> systemDevices()
{
    const AudioObjectPropertyAddress address {
        kAudioHardwarePropertyDevices, kAudioObjectPropertyScopeGlobal, kAudioObjectPropertyElementMaster
    };
    UInt32 size = 0;
    if (AudioObjectGetPropertyDataSize(kAudioObjectSystemObject, &address, 0, nullptr, &size) != noErr) {
        return {};
    }
    std::vector<AudioObjectID> devices(size / sizeof(AudioObjectID));
    if (AudioObjectGetPropertyData(kAudioObjectSystemObject, &address, 0, nullptr, &size, devices.data()) != noErr) {
        return {};
    }
    devices.resize(size / sizeof(AudioObjectID));
    devices.erase(std::remove_if(devices.begin(), devices.end(), isPrivateAggregate), devices.end());
    std::sort(devices.begin(), devices.end());
    return devices;
}
}

MacosSystemAudioDevicesListener::MacosSystemAudioDevicesListener()
{
    m_debounceTimer.setSingleShot(true);
    m_debounceTimer.setInterval(DEBOUNCE_INTERVAL_MS);
    QObject::connect(&m_debounceTimer, &QTimer::timeout, this, [this]() {
        m_systemDevicesChanged.notify();
    });
}

MacosSystemAudioDevicesListener::~MacosSystemAudioDevicesListener()
{
    stopListening();
}

void MacosSystemAudioDevicesListener::startListening()
{
    if (m_listening) {
        return;
    }

    m_systemDevices = systemDevices();
    for (const AudioObjectPropertyAddress& property : LISTENED_PROPERTIES) {
        AudioObjectAddPropertyListener(kAudioObjectSystemObject, &property, devicesChangedCallback, this);
    }
    m_listening = true;
}

void MacosSystemAudioDevicesListener::stopListening()
{
    if (!m_listening) {
        return;
    }

    for (const AudioObjectPropertyAddress& property : LISTENED_PROPERTIES) {
        AudioObjectRemovePropertyListener(kAudioObjectSystemObject, &property, devicesChangedCallback, this);
    }
    m_listening = false;
}

muse::async::Notification MacosSystemAudioDevicesListener::systemDevicesChanged() const
{
    return m_systemDevicesChanged;
}

void MacosSystemAudioDevicesListener::onSystemDevicesChanged(bool onlyDeviceList)
{
    auto devices = systemDevices();
    const bool devicesChanged = devices != m_systemDevices;
    m_systemDevices = std::move(devices);
    if (onlyDeviceList && !devicesChanged) {
        return;
    }

    // bursts of CoreAudio notifications (e.g. plugging a headset changes
    // the device list and both defaults) collapse into one notification;
    // restarting the timer would let a continuous stream starve it forever
    if (!m_debounceTimer.isActive()) {
        m_debounceTimer.start();
    }
}
