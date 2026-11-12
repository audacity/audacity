/*
* Audacity: A Digital Audio Editor
*/

#include "macossystemaudiodeviceslistener.h"

#include <CoreAudio/CoreAudio.h>

#include "framework/global/log.h"

using namespace au::au3audio;

namespace {
constexpr int DEBOUNCE_INTERVAL_MS = 500;

//! NOTE Device plugged/unplugged and system default input/output changed
const AudioObjectPropertyAddress LISTENED_PROPERTIES[] = {
    { kAudioHardwarePropertyDevices, kAudioObjectPropertyScopeGlobal, kAudioObjectPropertyElementMaster },
    { kAudioHardwarePropertyDefaultInputDevice, kAudioObjectPropertyScopeGlobal, kAudioObjectPropertyElementMaster },
    { kAudioHardwarePropertyDefaultOutputDevice, kAudioObjectPropertyScopeGlobal, kAudioObjectPropertyElementMaster },
};

const char* propertyName(AudioObjectPropertySelector selector)
{
    switch (selector) {
    case kAudioHardwarePropertyDevices: return "devices list";
    case kAudioHardwarePropertyDefaultInputDevice: return "default input device";
    case kAudioHardwarePropertyDefaultOutputDevice: return "default output device";
    default: return "unknown";
    }
}

OSStatus devicesChangedCallback(AudioObjectID, UInt32 nAddresses, const AudioObjectPropertyAddress* addresses, void* clientData)
{
    for (UInt32 i = 0; i < nAddresses; ++i) {
        LOGW() << "CoreAudio event: " << propertyName(addresses[i].mSelector) << " changed";
    }

    auto* self = reinterpret_cast<MacosSystemAudioDevicesListener*>(clientData);
    QMetaObject::invokeMethod(self, [self]() {
        self->onSystemDevicesChanged();
    }, Qt::QueuedConnection);

    return noErr;
}
}

MacosSystemAudioDevicesListener::MacosSystemAudioDevicesListener()
{
    m_debounceTimer.setSingleShot(true);
    m_debounceTimer.setInterval(DEBOUNCE_INTERVAL_MS);
    QObject::connect(&m_debounceTimer, &QTimer::timeout, this, [this]() {
        LOGW() << "debounce settled, notifying system devices changed";
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

void MacosSystemAudioDevicesListener::onSystemDevicesChanged()
{
    LOGW() << (m_debounceTimer.isActive() ? "debounce timer restarted" : "debounce timer started");
    m_debounceTimer.start();
}
