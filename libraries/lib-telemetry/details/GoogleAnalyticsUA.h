#pragma once

#include <string>
#include <unordered_map>

#include "ITelemetryService.h"

namespace audacity
{
namespace telemetry
{
namespace details
{

class GoogleAnalyticsUA final : public ITelemetryService
{
public:
	explicit GoogleAnalyticsUA (const std::string& appName, const std::string& appVersion, std::string trackingID) noexcept;

	size_t submitEvents (const std::string& clientId, const std::deque<Event>& list, Callback callback) override;
private:
    std::string getDefaultParams (const Event& evt) const;
    std::string getAdditionalParams (const Event& evt) const;

	void postAnalytics (const std::string& body, Callback callback) const;

	std::string mTrackingID;
	std::string mUserAgent;
};

}
}
}