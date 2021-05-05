/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file YandexMetricaUserTracking.h
 @brief Declare a class for reporting session events to Yandex Metrica.

 Dmitry Vedenko
 **********************************************************************/

#pragma once

#include "IUserTrackingService.h"

#include <string>
#include <mutex>

#include "lib-network-manager/CookiesList.h"

namespace audacity
{
namespace telemetry
{
namespace details
{
class CommonHeaders;

class YandexMetricaUserTracking final : public IUserTrackingService
{
public:
	YandexMetricaUserTracking (std::string cookiesPath, const std::string& trackingID, const CommonHeaders* commonHeaders);

	void reportAppStarted () override;
	void reportFinished () override;
private:
    void loadCookies ();
    void storeCookies () const;

    std::string mCookiesPath;
	std::string mUrl;

    mutable std::mutex mCookiesMutex;
	network_manager::CookiesList mCookieList;

	const CommonHeaders* mCommonHeaders;
};
}
}
}