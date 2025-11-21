/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file SentryRequestBuilder.cpp
 @brief Declare a class to generate the requests to Sentry.

 Dmitry Vedenko
 **********************************************************************/

#pragma once

#include <string>

#include "Request.h"

namespace audacity {
namespace sentry {
// This is a private class, so it is not exported
//! A helper, that creates a correct Request to Sentry
class SentryRequestBuilder final
{
public:
    static const SentryRequestBuilder& Get();

    network_manager::Request CreateRequest() const;

private:
    SentryRequestBuilder();

    std::string mUrl;
};
} // namespace sentry
} // namespace audacity
