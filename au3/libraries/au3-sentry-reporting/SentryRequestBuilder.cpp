/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file SentryRequestBuilder.h
 @brief Define a class to generate the requests to Sentry.

 Dmitry Vedenko
 **********************************************************************/

#include "SentryRequestBuilder.h"

#include <chrono>

namespace audacity {
namespace sentry {
const SentryRequestBuilder& audacity::sentry::SentryRequestBuilder::Get()
{
    static SentryRequestBuilder builder;

    return builder;
}

network_manager::Request SentryRequestBuilder::CreateRequest() const
{
    using namespace std::chrono;

    const std::string sentryAuth
        =std::string("Sentry sentry_version=7,sentry_timestamp=")
          + std::to_string(
              duration_cast<seconds>(system_clock::now().time_since_epoch())
              .count())
          + ",sentry_client=sentry-audacity/1.0,sentry_key=" + SENTRY_DSN_KEY;

    network_manager::Request request(mUrl);

    request.setHeader("Content-Type", "application/json");
    request.setHeader("X-Sentry-Auth", sentryAuth);

    return request;
}

SentryRequestBuilder::SentryRequestBuilder()
{
    mUrl = std::string("https://") + SENTRY_DSN_KEY + "@" + SENTRY_HOST
           + "/api/" + SENTRY_PROJECT + "/store/";
}
} // namespace sentry
} // namespace audacity
