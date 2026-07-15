/*
* Audacity: A Digital Audio Editor
*/

#include "windowssystemaudiodeviceslistener.h"

#include <mmdeviceapi.h>
#include <objbase.h>

#include "log.h"

using namespace au::au3audio;

namespace {
constexpr int DEBOUNCE_INTERVAL_MS = 500;

class DeviceNotificationClient : public IMMNotificationClient
{
public:
    explicit DeviceNotificationClient(WindowsSystemAudioDevicesListener* listener)
        : m_listener(listener) {}

    // IUnknown
    ULONG STDMETHODCALLTYPE AddRef() override
    {
        return InterlockedIncrement(&m_refCount);
    }

    ULONG STDMETHODCALLTYPE Release() override
    {
        const ULONG count = InterlockedDecrement(&m_refCount);
        if (count == 0) {
            delete this;
        }
        return count;
    }

    HRESULT STDMETHODCALLTYPE QueryInterface(REFIID riid, void** ppv) override
    {
        if (riid == IID_IUnknown || riid == __uuidof(IMMNotificationClient)) {
            *ppv = static_cast<IMMNotificationClient*>(this);
            AddRef();
            return S_OK;
        }
        *ppv = nullptr;
        return E_NOINTERFACE;
    }

    // IMMNotificationClient
    HRESULT STDMETHODCALLTYPE OnDefaultDeviceChanged(EDataFlow, ERole role, LPCWSTR) override
    {
        // fired once per role; reacting to a single role avoids duplicates
        if (role == eConsole) {
            m_listener->scheduleNotification();
        }
        return S_OK;
    }

    HRESULT STDMETHODCALLTYPE OnDeviceAdded(LPCWSTR) override
    {
        m_listener->scheduleNotification();
        return S_OK;
    }

    HRESULT STDMETHODCALLTYPE OnDeviceRemoved(LPCWSTR) override
    {
        m_listener->scheduleNotification();
        return S_OK;
    }

    HRESULT STDMETHODCALLTYPE OnDeviceStateChanged(LPCWSTR, DWORD) override
    {
        m_listener->scheduleNotification();
        return S_OK;
    }

    HRESULT STDMETHODCALLTYPE OnPropertyValueChanged(LPCWSTR, const PROPERTYKEY) override
    {
        return S_OK;
    }

private:
    virtual ~DeviceNotificationClient() = default;

    WindowsSystemAudioDevicesListener* m_listener = nullptr;
    LONG m_refCount = 1;
};
}

struct WindowsSystemAudioDevicesListener::Impl
{
    IMMDeviceEnumerator* enumerator = nullptr;
    DeviceNotificationClient* client = nullptr;
    bool ownsComInit = false;
};

WindowsSystemAudioDevicesListener::WindowsSystemAudioDevicesListener()
    : m_impl(std::make_unique<Impl>())
{
    m_debounceTimer.setSingleShot(true);
    m_debounceTimer.setInterval(DEBOUNCE_INTERVAL_MS);
    QObject::connect(&m_debounceTimer, &QTimer::timeout, this, [this]() {
        m_systemDevicesChanged.notify();
    });
}

WindowsSystemAudioDevicesListener::~WindowsSystemAudioDevicesListener()
{
    stopListening();
}

void WindowsSystemAudioDevicesListener::startListening()
{
    if (m_impl->enumerator) {
        return;
    }

    // S_OK/S_FALSE must be balanced with CoUninitialize;
    // RPC_E_CHANGED_MODE means COM is already initialized with another model and is usable as is
    const HRESULT initResult = CoInitializeEx(nullptr, COINIT_APARTMENTTHREADED);
    m_impl->ownsComInit = SUCCEEDED(initResult);

    HRESULT hr = CoCreateInstance(__uuidof(MMDeviceEnumerator), nullptr, CLSCTX_ALL,
                                  __uuidof(IMMDeviceEnumerator), reinterpret_cast<void**>(&m_impl->enumerator));
    if (FAILED(hr)) {
        LOGE() << "failed to create the audio device enumerator, hr: " << hr;
        return;
    }

    m_impl->client = new DeviceNotificationClient(this);
    hr = m_impl->enumerator->RegisterEndpointNotificationCallback(m_impl->client);
    if (FAILED(hr)) {
        LOGE() << "failed to register the audio device notification callback, hr: " << hr;
        m_impl->client->Release();
        m_impl->client = nullptr;
        m_impl->enumerator->Release();
        m_impl->enumerator = nullptr;
    }
}

void WindowsSystemAudioDevicesListener::stopListening()
{
    if (!m_impl->enumerator) {
        return;
    }

    if (m_impl->client) {
        m_impl->enumerator->UnregisterEndpointNotificationCallback(m_impl->client);
        m_impl->client->Release();
        m_impl->client = nullptr;
    }

    m_impl->enumerator->Release();
    m_impl->enumerator = nullptr;

    if (m_impl->ownsComInit) {
        CoUninitialize();
        m_impl->ownsComInit = false;
    }
}

muse::async::Notification WindowsSystemAudioDevicesListener::systemDevicesChanged() const
{
    return m_systemDevicesChanged;
}

void WindowsSystemAudioDevicesListener::scheduleNotification()
{
    QMetaObject::invokeMethod(this, [this]() {
        // bursts of notifications (e.g. plugging a headset changes the
        // device list and the defaults) collapse into one notification
        m_debounceTimer.start();
    }, Qt::QueuedConnection);
}
