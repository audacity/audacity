/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file CurlResponseFactory.h
 @brief Declare an implementation of IResponseFactory using libcurl.

 Dmitry Vedenko
 **********************************************************************/

#pragma once

#include <thread>
#include <vector>
#include <memory>
#include <mutex>

#include "../IResponseFactory.h"

#include "CurlHandleManager.h"
#include "ThreadPool/ThreadPool.h"

namespace audacity
{
namespace network_manager
{

class CurlResponseFactory final : public IResponseFactory
{
public:
	CurlResponseFactory ();

	void setProxy (const std::string& proxy) override;

	ResponsePtr performRequest (RequestVerb verb, const Request& request) override;
	ResponsePtr performRequest (RequestVerb verb, const Request& request, const void* data, size_t size) override;

	void terminate () override;

private:
	std::unique_ptr<CurlHandleManager> mHandleManager { std::make_unique<CurlHandleManager> () };
	std::unique_ptr<ThreadPool> mThreadPool;
};

}
}
