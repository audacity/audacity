/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file NetworkManager.cpp
 @brief Define a class for preforming HTTP requests.

 Dmitry Vedenko
 **********************************************************************/

/*!********************************************************************

 @class NetworkManager
 @brief Class for preforming HTTP requests.

 **********************************************************************/

#include "NetworkManager.h"

#include "IResponseFactory.h"
#include "curl/CurlResponseFactory.h"

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

ResponsePtr NetworkManager::doPut (const Request& request, const void* data, size_t size)
{
    return mResponseFactory->performRequest (RequestVerb::Put, request, data, size);
}

void NetworkManager::setProxy (const std::string& proxy)
{
    mResponseFactory->setProxy (proxy);
}

}
}
