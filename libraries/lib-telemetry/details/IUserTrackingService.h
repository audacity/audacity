/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file IUserTrackingService.h
 @brief Declare an interface for reporting session events.

 Dmitry Vedenko
 **********************************************************************/

#pragma once

namespace audacity
{
namespace telemetry
{
namespace details
{
class IUserTrackingService
{
public:
    virtual ~IUserTrackingService () = default;

    virtual void reportAppStarted () = 0;
    virtual void reportFinished () = 0;
};
}
}
}