#pragma once

#include <deque>
#include <functional>

namespace audacity
{
namespace telemetry
{
namespace details
{

struct Event;

enum class SubmissionResult
{
    Success,
    Retry,
    Failed
};

class ITelemetryService
{
public:
    using Callback = std::function<void (SubmissionResult)>;

    ~ITelemetryService () = default;

    virtual size_t submitEvents (const std::string& clientId, const std::deque<Event>& list, Callback callback) = 0;
};

}
}
}
