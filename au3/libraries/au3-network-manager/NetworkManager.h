/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file NetworkManager.h
 @brief Declare a class for performing HTTP requests.

 Dmitry Vedenko
 **********************************************************************/

#pragma once

#include <memory>
#include <string>

#include "NetworkManagerApi.h"

namespace audacity {
namespace network_manager {
class Request;
class IResponse;
class IResponseFactory;
class MultipartData;

using ResponsePtr = std::shared_ptr<IResponse>;

class RequestPayloadStream;
using RequestPayloadStreamPtr = std::shared_ptr<RequestPayloadStream>;

class NETWORK_MANAGER_API NetworkManager final
{
    NetworkManager ();
    ~NetworkManager ();
public:
    static NetworkManager& GetInstance();
    static void Terminate();

    ResponsePtr doGet(const Request& request);
    ResponsePtr doHead(const Request& request);

    ResponsePtr doDelete(const Request& request);

    ResponsePtr doPost(const Request& request, const void* data, size_t size);
    ResponsePtr doPost(const Request& request, RequestPayloadStreamPtr payloadStream);
    ResponsePtr doPost(const Request& request, std::unique_ptr<MultipartData> form);

    ResponsePtr doPut(const Request& request, const void* data, size_t size);
    ResponsePtr doPut(const Request& request, RequestPayloadStreamPtr payloadStream);
    ResponsePtr doPut(const Request& request, std::unique_ptr<MultipartData> form);

    ResponsePtr doPatch(const Request& request, const void* data, size_t size);
    ResponsePtr doPatch(const Request& request, RequestPayloadStreamPtr payloadStream);

    void setProxy(const std::string& proxy);
private:
    std::unique_ptr<IResponseFactory> mResponseFactory;
};
}
}
