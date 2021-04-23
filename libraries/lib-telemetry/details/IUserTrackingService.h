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