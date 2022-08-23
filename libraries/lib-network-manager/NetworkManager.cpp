/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file NetworkManager.cpp
 @brief Define a class for performing HTTP requests.

 Dmitry Vedenko
 **********************************************************************/

/*!********************************************************************

 @class NetworkManager
 @brief Class for performing HTTP requests.

 **********************************************************************/

#include "NetworkManager.h"

#include "IResponseFactory.h"
#include "curl/CurlResponseFactory.h"

#include "MultipartData.h"

namespace audacity
{
namespace network_manager
{

NetworkManager::NetworkManager ()
{
    mResponseFactory = std::make_unique<CurlResponseFactory> ();
}

NetworkManager::~NetworkManager ()
{}

NetworkManager& NetworkManager::GetInstance ()
{
    static NetworkManager instance;

    return instance;
}

void NetworkManager::Terminate ()
{
    GetInstance ().mResponseFactory->terminate ();
}

ResponsePtr NetworkManager::doGet (const Request& request)
{
    return mResponseFactory->performRequest (RequestVerb::Get, request);
}

ResponsePtr NetworkManager::doHead (const Request& request)
{
    return mResponseFactory->performRequest (RequestVerb::Head, request);
}

ResponsePtr NetworkManager::doDelete (const Request& request)
{
    return mResponseFactory->performRequest (RequestVerb::Delete, request);
}

ResponsePtr NetworkManager::doPost (const Request& request, const void* data, size_t size)
{
    return mResponseFactory->performRequest (RequestVerb::Post, request, data, size);
}

ResponsePtr NetworkManager::doPost(
   const Request& request, std::unique_ptr<MultipartData> form)
{
   return mResponseFactory->performRequest(RequestVerb::Post, request, std::move(form));
}

ResponsePtr NetworkManager::doPut (const Request& request, const void* data, size_t size)
{
    return mResponseFactory->performRequest (RequestVerb::Put, request, data, size);
}

ResponsePtr NetworkManager::doPut(
   const Request& request, std::unique_ptr<MultipartData> form)
{
   return mResponseFactory->performRequest(RequestVerb::Put, request, std::move(form));
}

ResponsePtr
NetworkManager::doPatch(const Request& request, const void* data, size_t size)
{
   return mResponseFactory->performRequest(
      RequestVerb::Patch, request, data, size);
}

void NetworkManager::setProxy (const std::string& proxy)
{
    mResponseFactory->setProxy (proxy);
}

}
}
