/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file CurlResponseFactory.cpp
 @brief Define an implementation of IResponseFactory using libcurl.

 Dmitry Vedenko
 **********************************************************************/
#include "CurlResponseFactory.h"

#include "CurlResponse.h"
#include "MultipartData.h"
#include "RequestPayload.h"

#include "ThreadPool/ThreadPool.h"

namespace audacity
{
namespace network_manager
{

constexpr decltype(std::thread::hardware_concurrency ()) MIN_CURL_THREADS = 6;

CurlResponseFactory::CurlResponseFactory ()
    : mThreadPool (std::make_unique<ThreadPool>(
        std::max (
            MIN_CURL_THREADS, 
            std::thread::hardware_concurrency ()
        )))
{

}

CurlResponseFactory::~CurlResponseFactory ()
{
}

void CurlResponseFactory::setProxy (const std::string& proxy)
{
    mHandleManager->setProxy (proxy);
}

ResponsePtr CurlResponseFactory::performRequest (RequestVerb verb, const Request& request)
{
    return performRequest(verb, request, RequestPayloadStreamPtr {});
}

ResponsePtr CurlResponseFactory::performRequest(
   RequestVerb verb, const Request& request,
   RequestPayloadStreamPtr payloadStream)
{
    if (!mThreadPool)
       return {};

    auto response =
       std::make_shared<CurlResponse>(verb, request, mHandleManager.get());

    mThreadPool->enqueue(
       [response, payloadStream = std::move(payloadStream)]()
       {
          if (payloadStream)
             response->setPayload(payloadStream);

          response->perform();
       });

    return response;
}

ResponsePtr CurlResponseFactory::performRequest(
   RequestVerb verb, const Request& request,
   std::unique_ptr<MultipartData> form)
{
   if (!mThreadPool)
      return {};

   std::shared_ptr<CurlResponse> response =
      std::make_shared<CurlResponse>(verb, request, mHandleManager.get());


   mThreadPool->enqueue(
      [response, rawForm = form.release()]() mutable
      {
         if (rawForm != nullptr && !rawForm->IsEmpty())
            response->setForm(std::unique_ptr<MultipartData>(rawForm));

         response->perform();
      });

   return response;
}

void CurlResponseFactory::terminate ()
{
    mThreadPool.reset ();
    mHandleManager.reset ();
}

}
}
