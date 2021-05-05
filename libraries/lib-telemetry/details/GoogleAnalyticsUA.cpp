/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file GoogleAnalyticsUA.cpp
 @brief Define a class for reporting telemetry events to Google Universal Analytics.

 Dmitry Vedenko
 **********************************************************************/

#include "GoogleAnalyticsUA.h"

#include <algorithm>
#include <wx/platinfo.h>
#include <wx/xlocale.h>

#include "Event.h"
#include "CommonHeaders.h"

#include "lib-network-manager/NetworkManager.h"
#include "lib-network-manager/Request.h"
#include "lib-network-manager/IResponse.h"

#include "lib-string-utils/UrlEncode.h"
#include "lib-string-utils/CodeConversions.h"

namespace audacity
{
namespace telemetry
{
namespace details
{
static const char* EventTypeNames[] = {
    "screenview",
    "exception",
    "exception",
    "event"
};

constexpr size_t MaxEventsCount = 20;

GoogleAnalyticsUA::GoogleAnalyticsUA (std::string trackingID, const CommonHeaders* commonHeaders) noexcept
    : mTrackingID (std::move (trackingID)),
    mCommonHeaders(commonHeaders)
{
}

size_t GoogleAnalyticsUA::submitEvents (const std::string& clientId, const std::deque<Event>& list, Callback callback)
{
    auto eventIt = std::find_if (list.begin (), list.end (), [](const Event& evt) {
        return !evt.Submitted;
    });

    size_t eventsCount = 0;

    std::string postBody;

    const std::string preambule = 
        "v=1&tid=" + mTrackingID + 
        "&cid=" + clientId + "&t=";

    for (;
        eventsCount < MaxEventsCount && eventIt != list.end ();
        ++eventIt)
    {
        const Event& evt = *eventIt;

        if (evt.Submitted)
            continue;

        ++eventsCount;

        if (!postBody.empty ())
            postBody += "\n";

        postBody += preambule +
            EventTypeNames[size_t (evt.Type)] +
            getDefaultParams (evt) +
            getAdditionalParams (evt);

        if (postBody.size() >= 6 * 1024)
            break;
    }

    if (!postBody.empty ())
        postAnalytics (postBody, std::move (callback));

    return eventsCount;
}

std::string GoogleAnalyticsUA::getDefaultParams (const Event& evt) const
{
    switch (evt.Type)
    {
    case EventType::ScreenView:
        return "&cd=" + UrlEncode (evt.Value);
    case EventType::FatalException:
        return "&exf=1&exd=" + UrlEncode (evt.Value);
    case EventType::Exception:
        return "&exf=0&exd=" + UrlEncode (evt.Value);
    case EventType::Event:
    {
        std::string param = "&ec=" + UrlEncode (evt.Category);

        if (!evt.Value.empty ())
            param += "&el=" + UrlEncode (evt.Value);

        if (!evt.Action.empty ())
            param += "&ea=" + UrlEncode (evt.Action);

        return param;
    }
    default:
        break;
    }

    return {};
}

std::string GoogleAnalyticsUA::getAdditionalParams (const Event& evt) const
{
    if (evt.Params.empty ())
        return {};

    std::string paramsString;

    int customIndex = 2;

    for (const auto& param : evt.Params)
    {
        if (param.first == "ts")
        {
            paramsString += "&cm1=" + UrlEncode (param.second);
        }
        else if (param.first == "sid")
        {
            paramsString += "&cd1=" + UrlEncode (param.second);
        }
        else
        {
            paramsString += 
                "&cd" + std::to_string (customIndex++) + 
                "=" + UrlEncode (param.second);
        }
    }

    return paramsString;
}

void GoogleAnalyticsUA::postAnalytics (const std::string& body, Callback callback) const
{
    network_manager::Request request ("https://www.google-analytics.com/batch");

    mCommonHeaders->setupHeaders(&request);

    request.setHeader ("Content-Type", "application/x-www-form-urlencoded");

    auto response = network_manager::NetworkManager::GetInstance ().doPost (request, body.data (), body.size ());

    response->setRequestFinishedCallback ([response, cb = std::move (callback)](network_manager::IResponse*) {
        const network_manager::NetworkError errorCode = response->getError ();

        if (errorCode == network_manager::NetworkError::NoError)
            cb (SubmissionResult::Success);
        else if (errorCode == network_manager::NetworkError::HTTPError)
            cb (SubmissionResult::Failed);
        else
            cb (SubmissionResult::Retry);
    });
}

}
}
}