/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file IResponse.h
 @brief Declare an interface for HTTP response factory.

 Dmitry Vedenko
 **********************************************************************/

#pragma once

#include <memory>
#include <string>

namespace audacity
{
namespace network_manager
{

class IResponse;
class Request;

using ResponsePtr = std::shared_ptr<IResponse>;

enum class RequestVerb
{
    Head,
    Get,
    Post,
    Put,
    Delete
};

class IResponseFactory
{
public:
    virtual ~IResponseFactory () = default;

    virtual void setProxy (const std::string& proxy) = 0;

    virtual ResponsePtr performRequest (RequestVerb verb, const Request& request) = 0;
    virtual ResponsePtr performRequest (RequestVerb verb, const Request& request, const void* data, size_t size) = 0;

    virtual void terminate () = 0;
};

}
}
