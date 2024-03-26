/**********************************************************************

  Audacity: A Digital Audio Editor

  DeviceChange.h

  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_DEVICECHANGE_H__
#define __AUDACITY_DEVICECHANGE_H__

#include "Observer.h"
#include <wx/event.h>
#include <wx/timer.h>

enum class DeviceChangeMessage : char { Rescan, Change };
struct DeviceChangeMessagePublisher : Observer::Publisher<DeviceChangeMessage>
{
   using Observer::Publisher<DeviceChangeMessage>::Publish;
};

#include <memory>

namespace Experimental {
constexpr bool DeviceChangeHandler = false &&
#if defined(__WXMSW__) || defined(__WXMAC__) || defined(HAVE_LIBUDEV_H)
   true
#else
   false
#endif
;
}

#include "Observer.h"

class DeviceChangeInterface /* not final */
{
public:
   virtual ~DeviceChangeInterface() {};

   virtual bool SetHandler(DeviceChangeMessagePublisher *handler) = 0;
   void Enable(bool enable = true) { mEnabled = enable; }
protected:
   bool mEnabled{ false };
};

class DeviceChangeHandler
   : public wxEvtHandler // for wxTimerEvent
   , public DeviceChangeMessagePublisher
{
public:
   DeviceChangeHandler();
   virtual ~DeviceChangeHandler();

   void Enable(bool enable = true);

   virtual void DeviceChangeNotification() = 0;

private:
   void OnChange(DeviceChangeMessage);
   void OnTimer(wxTimerEvent & evt);

   std::unique_ptr<DeviceChangeInterface> mListener;
   wxTimer mTimer;

   Observer::Subscription mSubscription;

   DECLARE_EVENT_TABLE()
};

#endif
