/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file IResponse.h
 @brief Declare an interface for HTTP response factory.

 Dmitry Vedenko
 **********************************************************************/

#pragma once

#include <memory>
#include <string>
#include <memory>

namespace audacity {
namespace network_manager {
class IResponse;
class Request;
class MultipartData;

using ResponsePtr = std::shared_ptr<IResponse>;

class RequestPayloadStream;
using RequestPayloadStreamPtr = std::shared_ptr<RequestPayloadStream>;

enum class RequestVerb
{
    Head,
    Get,
    Post,
    Put,
    Delete,
    Patch
};

class IResponseFactory
{
public:
    virtual ~IResponseFactory () = default;

    virtual void setProxy(const std::string& proxy) = 0;

    virtual ResponsePtr performRequest(RequestVerb verb, const Request& request) = 0;
    virtual ResponsePtr performRequest(
        RequestVerb verb, const Request& request, RequestPayloadStreamPtr payloadStream) = 0;
    virtual ResponsePtr performRequest(RequestVerb verb, const Request& request, std::unique_ptr<MultipartData> form) = 0;

    virtual void terminate() = 0;
};
}
}
