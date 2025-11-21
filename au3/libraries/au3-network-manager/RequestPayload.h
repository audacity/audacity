/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file RequestPayload.h
 @brief Declare a class for constructing HTTP requests.

 Dmitry Vedenko
 **********************************************************************/

#pragma once

#include <cstdint>
#include <memory>
#include <string>

namespace audacity::network_manager {
class NETWORK_MANAGER_API RequestPayloadStream /* not final */
{
public:
    virtual ~RequestPayloadStream();

    //! return true on if stream is not empty
    virtual bool HasData() const = 0;

    //! May return 0 even if HasData() returns true.
    virtual int64_t GetDataSize() const = 0;

    enum class SeekDirection
    {
        Start,
        Current,
        End
    };

    //! returns true on success
    virtual bool Seek(int64_t offset, SeekDirection direction) = 0;

    //! returns number of bytes read
    virtual int64_t Read(void* buffer, int64_t size) = 0;
}; // class RequestPayload

using RequestPayloadStreamPtr = std::shared_ptr<RequestPayloadStream>;

NETWORK_MANAGER_API RequestPayloadStreamPtr
CreateRequestPayloadStream(const void* data, int64_t size, bool copyData);

NETWORK_MANAGER_API RequestPayloadStreamPtr CreateRequestPayloadStream();

NETWORK_MANAGER_API RequestPayloadStreamPtr
CreateRequestPayloadStream(const std::string& filePath);
} // namespace audacity::network_manager
