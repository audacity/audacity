#pragma once

#include <string>

namespace audacity
{
namespace network_manager
{
class Request;
}

namespace telemetry
{
namespace details
{
class CommonHeaders final
{
public:
    CommonHeaders (const std::string& appName, const std::string& appVersion);

    void setupHeaders (network_manager::Request* request) const;

private:
    std::string mUserAgent;
    std::string mAcceptLanguage;
    std::string mReferer;
};
}
}
}