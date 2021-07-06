/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file CurlResponse.h
 @brief Declare an implementation of IResponse using libcurl.

 Dmitry Vedenko
 **********************************************************************/

#pragma once

#include <deque>
#include <mutex>
#include <cstdint>

#include "../IResponse.h"

#include "../HeadersList.h"
#include "../CookiesList.h"
#include "../Request.h"

#include "CurlHandleManager.h"

namespace audacity
{
namespace network_manager
{

class CurlResponse final : public IResponse
{
public:
	CurlResponse (RequestVerb verb, const Request& request, CurlHandleManager* handleManager) noexcept;

	bool isFinished () const noexcept override;
	unsigned getHTTPCode () const noexcept override;

	NetworkError getError () const noexcept override;
	std::string getErrorString () const override;

	bool headersReceived () const noexcept override;

	bool hasHeader (const std::string& headerName) const noexcept override;
	std::string getHeader (const std::string& headerName) const override;

	const HeadersList& getHeaders () const noexcept override;
	const CookiesList& getCookies () const noexcept override;

	const Request& getRequest () const noexcept override;

	std::string getURL () const override;

	void abort () noexcept override;

	void setOnDataReceivedCallback (RequestCallback callback) override;

	void setRequestFinishedCallback (RequestCallback callback) override;

	uint64_t getBytesAvailable () const noexcept override;
	uint64_t readData (void* buffer, uint64_t maxBytesCount) override;

	void perform (const void* ptr, size_t size);
private:
	RequestVerb mVerb;

	Request mRequest;
	CurlHandleManager* mHandleManager;

	CurlHandleManager::Handle* mCurrentHandle { nullptr };

	mutable std::mutex mCallbackMutex;
    RequestCallback mOnDataReceivedCallback;
    RequestCallback mRequestFinishedCallback;

	mutable std::mutex mHeadersMutex;
	HeadersList mResponseHeaders;
	CookiesList mResponseCookies;

	mutable std::mutex mDataBufferMutex;
	std::deque<uint8_t> mDataBuffer;

	mutable std::mutex mStatusMutex;

    NetworkError mNetworkError { NetworkError::NoError };
    std::string mErrorString;

	unsigned mHttpCode { 0 };

    bool mHeadersReceived { false };
    bool mRequestFinished { false };
    bool mAbortRequested { false };

    static size_t WriteCallback (const uint8_t* ptr, size_t size, size_t nmemb, CurlResponse* userdata) noexcept;
    static size_t HeaderCallback (const char* buffer, size_t size, size_t nitems, CurlResponse* userdata) noexcept;
};

}
}
