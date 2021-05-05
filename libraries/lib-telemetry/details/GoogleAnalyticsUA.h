/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file GoogleAnalyticsUA.h
 @brief Declare a class for reporting telemetry events to Google Universal Analytics.

 Dmitry Vedenko
 **********************************************************************/

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
class CommonHeaders;

class GoogleAnalyticsUA final : public ITelemetryService
{
public:
	explicit GoogleAnalyticsUA (std::string trackingID, const CommonHeaders* commonHeaders) noexcept;

	size_t submitEvents (const std::string& clientId, const std::deque<Event>& list, Callback callback) override;
private:
    std::string getDefaultParams (const Event& evt) const;
    std::string getAdditionalParams (const Event& evt) const;

	void postAnalytics (const std::string& body, Callback callback) const;

	std::string mTrackingID;

	const CommonHeaders* mCommonHeaders;
};

}
}
}