/**********************************************************************

  Audacity: A Digital Audio Editor

  DeviceChange.cpp

  Leland Lucius

*******************************************************************//*!

\file DeviceChange.cpp
\brief

*//*******************************************************************/

#include "DeviceChange.h"

#include "BasicUI.h"

#if defined(EXPERIMENTAL_DEVICE_CHANGE_HANDLER)

#if defined(HAVE_DEVICE_CHANGE)

#include <wx/module.h>
#include <wx/thread.h>

#if defined(__WXMSW__)

#include <Windows.h>
#include <mmsystem.h>
#include <mmdeviceapi.h>
#include <audioclient.h>

class DeviceChangeListener final : public IMMNotificationClient, public DeviceChangeInterface
{
public:
    DeviceChangeListener()
    {
        mRefCnt = 1;
        mEnumerator = NULL;
        mEnabled = false;
        mHandler = NULL;
    }

    virtual ~DeviceChangeListener()
    {
        if (mEnumerator) {
            mEnumerator->UnregisterEndpointNotificationCallback(this);
            mEnumerator = NULL;
        }

        if (mHandler) {
            CoInitialize(NULL);
        }
    }

    // IUnknown implementation

    ULONG STDMETHODCALLTYPE AddRef()
    {
        return InterlockedIncrement(&mRefCnt);
    }

    ULONG STDMETHODCALLTYPE Release()
    {
        ULONG cnt = InterlockedDecrement(&mRefCnt);
        if (cnt == 0) {
            delete this;
        }
        return cnt;
    }

    HRESULT STDMETHODCALLTYPE QueryInterface(REFIID riid, VOID** ppvInterface)
    {
        if (riid == IID_IUnknown) {
            AddRef();
            *ppvInterface = (IUnknown*)this;
        } else if (riid == __uuidof(IMMNotificationClient)) {
            AddRef();
            *ppvInterface = (IMMNotificationClient*)this;
        } else {
            *ppvInterface = NULL;
            return E_NOINTERFACE;
        }

        return S_OK;
    }

    // IMMDeviceChangeListener implementation

    HRESULT STDMETHODCALLTYPE OnDefaultDeviceChanged(EDataFlow flow, ERole role, LPCWSTR pwstrDeviceId)
    {
        return S_OK;
    }

    HRESULT STDMETHODCALLTYPE OnDeviceAdded(LPCWSTR pwstrDeviceId)
    {
        return S_OK;
    }

    HRESULT STDMETHODCALLTYPE OnDeviceRemoved(LPCWSTR pwstrDeviceId)
    {
        return S_OK;
    }

    HRESULT STDMETHODCALLTYPE OnDeviceStateChanged(LPCWSTR pwstrDeviceId, DWORD dwNewState)
    {
        if (mEnabled) {
            mEnabled = false;
            BasicUI::CallAfter([This]{
                This->mHandler->Publish(DeviceChangeMessage::Change);
            });
        }

        return S_OK;
    }

    HRESULT STDMETHODCALLTYPE OnPropertyValueChanged(LPCWSTR pwstrDeviceId, const PROPERTYKEY key)
    {
        return S_OK;
    }

    bool SetHandler(DeviceChangeMessagePublisher* handler)
    {
        mHandler = handler;

        CoInitialize(NULL);

        HRESULT hr = CoCreateInstance(__uuidof(MMDeviceEnumerator),
                                      NULL,
                                      CLSCTX_INPROC_SERVER,
                                      __uuidof(IMMDeviceEnumerator),
                                      (void**)&mEnumerator);
        if (hr == S_OK && mEnumerator) {
            mEnumerator->RegisterEndpointNotificationCallback(this);
        }

        return hr == S_OK && mEnumerator;
    }

    void Enable(bool enable)
    {
        mEnabled = enable;
    }

private:
    DeviceChangeMessagePublisher* mHandler;
    bool mEnabled;
    ULONG mRefCnt;
    IMMDeviceEnumerator* mEnumerator;
};

#elif defined(__WXGTK__)

#include <libudev.h>
#include <stdlib.h>
#include <locale.h>
#include <unistd.h>

class DeviceChangeListener final : public DeviceChangeInterface
{
public:
    DeviceChangeListener()
    {
        mEnabled = false;
        mHandler = NULL;
        mThread = 0;
    }

    virtual ~DeviceChangeListener()
    {
        if (mThread) {
            pthread_cancel(mThread);
            mThread = 0;
        }
    }

    // IUnknown implementation
    bool SetHandler(DeviceChangeMessagePublisher* handler)
    {
        mHandler = handler;

        return pthread_create(&mThread, NULL, DeviceChangeListener::Listener, this) == 0;
    }

    void Enable(bool enable)
    {
        mEnabled = enable;
    }

    static void* Listener(void* parm)
    {
        DeviceChangeListener* This = (DeviceChangeListener*)parm;

        // Instantiate the udev object
        struct udev* udev = udev_new();
        if (!udev) {
            pthread_exit(NULL);
        }

        // Instantiate the monitor object
        struct udev_monitor* mon = udev_monitor_new_from_netlink(udev, "udev");

        // Start receiving notifications
        udev_monitor_enable_receiving(mon);

        // Get the file descriptor we'll wait on
        int fd = udev_monitor_get_fd(mon);

        while (true)
        {
            fd_set set;

            FD_ZERO(&set);
            FD_SET(fd, &set);

            if (select(fd + 1, &set, NULL, NULL, NULL) < 0) {
                break;
            }

            if (FD_ISSET(fd, &set)) {
                struct udev_device* dev = udev_monitor_receive_device(mon);
                if (dev) {
#if 0
                    wxPrintf("Got Device\n");
                    wxPrintf("   Node: %s\n", udev_device_get_devnode(dev));
                    wxPrintf("   Subsystem: %s\n", udev_device_get_subsystem(dev));
                    wxPrintf("   Devtype: %s\n", udev_device_get_devtype(dev));
                    wxPrintf("   Action: %s\n", udev_device_get_action(dev));
#endif
                    if (This->mEnabled) {
                        This->mEnabled = false;
                        BasicUI::CallAfter([This]{
                            This->mHandler->Publish(DeviceChangeMessage::Change);
                        });
                    }

                    udev_device_unref(dev);
                }
            }
        }

        udev_unref(udev);

        pthread_exit(NULL);
    }

private:
    DeviceChangeMessagePublisher* mHandler;
    bool mEnabled;
    pthread_t mThread;
};

#elif defined(__WXMAC__)

#include <CoreAudio/CoreAudio.h>

class DeviceChangeListener final : public DeviceChangeInterface
{
public:
    DeviceChangeListener()
    {
        mEnabled = false;
        mHandler = NULL;
        mListening = false;
    }

    virtual ~DeviceChangeListener()
    {
        if (mListening) {
            AudioObjectPropertyAddress property_address;

            property_address.mSelector = kAudioHardwarePropertyDevices;
            property_address.mScope = kAudioObjectPropertyScopeGlobal;
            property_address.mElement = kAudioObjectPropertyElementMaster;

            AudioObjectRemovePropertyListener(kAudioObjectSystemObject,
                                              &property_address,
                                              DeviceChangeListener::Listener,
                                              this);
            mListening = false;
        }
    }

    // IUnknown implementation
    bool SetHandler(DeviceChangeMessagePublisher* handler)
    {
        mHandler = handler;

        AudioObjectPropertyAddress property_address;

        property_address.mSelector = kAudioHardwarePropertyDevices;
        property_address.mScope = kAudioObjectPropertyScopeGlobal;
        property_address.mElement = kAudioObjectPropertyElementMaster;

        mListening = AudioObjectAddPropertyListener(kAudioObjectSystemObject,
                                                    &property_address,
                                                    DeviceChangeListener::Listener,
                                                    this) == 0;
        return true;
    }

    void Enable(bool enable)
    {
        mEnabled = enable;
    }

    static OSStatus Listener(AudioObjectID objectID,
                             UInt32 numberAddresses,
                             const AudioObjectPropertyAddress inAddresses[],
                             void* clientData)
    {
        DeviceChangeListener* This = (DeviceChangeListener*)clientData;

        for (int i = 0; i < numberAddresses; i++) {
#if 0
            wxPrintf("address %d\n", i);
            wxPrintf("selector %08x\n", inAddresses[i].mSelector);
            wxPrintf("scope %08x\n", inAddresses[i].mScope);
            wxPrintf("element %08x\n", inAddresses[i].mElement);
#endif
            if (This->mEnabled) {
                This->mEnabled = false;
                BasicUI::CallAfter([This]{
                    This->mHandler->Publish(DeviceChangeMessage::Change);
                });
            }
        }

        return 0;
    }

private:
    DeviceChangeMessagePublisher* mHandler;
    bool mEnabled;
    bool mListening;
};
#endif

BEGIN_EVENT_TABLE(DeviceChangeHandler, wxEvtHandler)
EVT_TIMER(wxID_ANY, DeviceChangeHandler::OnTimer)
END_EVENT_TABLE()

DeviceChangeHandler::DeviceChangeHandler()
    :  wxEvtHandler()
{
    mTimer.SetOwner(this);
    mListener = std::make_unique<DeviceChangeListener>();
    mListener->SetHandler(this);
    mListener->Enable(true);

    // Subscribe to self!
    mSubscription = this->Subscribe(*this, &DeviceChangeHandler::OnChange);
}

DeviceChangeHandler::~DeviceChangeHandler()
{
    if (mListener) {
        mListener->Enable(false);
    }
}

void DeviceChangeHandler::Enable(bool enable)
{
    mListener->Enable(enable);
}

void DeviceChangeHandler::OnChange(DeviceChangeMessage m)
{
    if (m == DeviceChangeMessage::Change) {
        mTimer.Start(500, true);
    }
}

void DeviceChangeHandler::OnTimer(wxTimerEvent& evt)
{
    DeviceChangeNotification();
    mListener->Enable(true);
}

#endif

#endif
