/**********************************************************************

  Audacity: A Digital Audio Editor

  DeviceChange.h

  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_DEVICECHANGE_H__
#define __AUDACITY_DEVICECHANGE_H__

#include "Observer.h"

enum class DeviceChangeMessage : char {
    Rescan, Change
};
using DeviceChangeMessagePublisher = Observer::Publisher<DeviceChangeMessage>;

#if defined(EXPERIMENTAL_DEVICE_CHANGE_HANDLER)

#include <memory>

#if defined(__WXMSW__) || defined(__WXMAC__) || defined(HAVE_LIBUDEV_H)
#define HAVE_DEVICE_CHANGE
#endif

#if defined(HAVE_DEVICE_CHANGE)

#include "Observer.h"

class DeviceChangeInterface /* not final */
{
public:
    virtual ~DeviceChangeInterface() {}

    virtual bool SetHandler(DeviceChangeMessagePublisher* handler) = 0;
    virtual void Enable(bool enable = true) = 0;
};

class DeviceChangeHandler : public wxEvtHandler, // for wxTimerEvent
    public DeviceChangeMessagePublisher
{
public:
    DeviceChangeHandler();
    virtual ~DeviceChangeHandler();

    void Enable(bool enable = true);

    virtual void DeviceChangeNotification() = 0;

private:
    void OnChange(DeviceChangeMessage);
    void OnTimer(wxTimerEvent& evt);

    std::unique_ptr<DeviceChangeInterface> mListener;
    wxTimer mTimer;

    Observer::Subscription mSubscription;

    DECLARE_EVENT_TABLE()
};

#endif

#endif

#endif
