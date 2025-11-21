/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file Uuid.cpp
 @brief Define a class to generate and parse UUIDs

 Dmitry Vedenko
 **********************************************************************/

/*!********************************************************************

 @class Uuid
 @brief Platform independent class for generating and parsing UUIDs.

 **********************************************************************/

#include "Uuid.h"

#include <algorithm>
#include <cstring>
#include <cassert>
#include "CFResources.h"

#if defined(USE_UUID_CREATE)
#   include <rpc.h>
#elif defined(USE_CFUUID)
#   include <CoreFoundation/CFUUID.h>
#elif defined(USE_LIBUUID)
#   include <uuid/uuid.h>
#endif

#include "HexHelpers.h"

namespace audacity {
constexpr int BRACED_UUID_LENGTH = 38;
constexpr int UUID_LENGTH = 36;
constexpr int HEX_UUID_LENGTH = 32;

namespace {
bool readByte(Uuid::Bytes::iterator& outputIt,
              std::string::const_iterator& inputIt,
              const std::string::const_iterator& inputEnd)
{
    if (inputIt == inputEnd) {
        return false;
    }

    const char c1 = *inputIt++;

    if (!std::isxdigit(c1)) {
        return false;
    }

    if (inputIt == inputEnd) {
        return false;
    }

    const char c2 = *inputIt++;

    if (!std::isxdigit(c2)) {
        return false;
    }

    *outputIt = static_cast<uint8_t>((HexCharToNum(c1) << 4) | HexCharToNum(c2));

    ++outputIt;

    return true;
}
} // namespace

Uuid::Uuid()
    : Uuid(Bytes {})
{
}

Uuid::Uuid(const Bytes& data) noexcept
    : mData(data)
{
}

Uuid Uuid::Generate()
{
#if defined(USE_UUID_CREATE)
    UUID winUid;

    if (RPC_S_OK != ::UuidCreate(&winUid)) {
        return {}
    }

    Uuid uuid;

    std::memcpy(uuid.mData.data(), &winUid, sizeof(winUid));

    return uuid;
#elif defined(USE_CFUUID)
    CFUUIDBytes bytes = CFUUIDGetUUIDBytes(
        CF_ptr<CFUUIDRef> { CFUUIDCreate(NULL) }.get());

    Uuid uuid;

    std::memcpy(uuid.mData.data(), &bytes, sizeof(bytes));

    return uuid;
#elif defined(USE_LIBUUID)
    uuid_t newId;

    uuid_generate(newId);

    Uuid uuid;

    std::memcpy(uuid.mData.data(), newId, sizeof(newId));

    return uuid;
#else
#   error "UUID generator is not defined"
#endif
}

Uuid Uuid::FromString(const std::string& str)
{
    const size_t length = str.length();

    if (length == 0) {
        return {}
    }

    const bool hasBraces = str[0] == '{';

    if (hasBraces && (length != BRACED_UUID_LENGTH || str.back() != '}')) {
        return {}
    } else if (!hasBraces && length != UUID_LENGTH) {
        return {}
    }

    const unsigned int iteratorOffset = hasBraces ? 1 : 0;

    std::string::const_iterator currentSymbol = str.begin() + iteratorOffset;
    std::string::const_iterator inputEnd = str.end() - iteratorOffset;

    Uuid uuid;
    Bytes::iterator currentByte = uuid.mData.begin();

    for (int i = 0; i < 16; ++i) {
        if (!readByte(currentByte, currentSymbol, inputEnd)) {
            return {}
        }

        if (currentSymbol != inputEnd && *currentSymbol == '-') {
            ++currentSymbol;
        }
    }

    return uuid;
}

bool Uuid::IsNil() const noexcept
{
    return std::all_of(mData.begin(), mData.end(),
                       [](uint8_t c) { return c == 0; });
}

Uuid::operator bool() const noexcept
{
    return !IsNil();
}

bool Uuid::operator==(const Uuid& rhs) const noexcept
{
    return mData == rhs.mData;
}

bool Uuid::operator!=(const Uuid& rhs) const noexcept
{
    return mData != rhs.mData;
}

bool Uuid::operator>(const Uuid& rhs) const noexcept
{
    return mData > rhs.mData;
}

bool Uuid::operator>=(const Uuid& rhs) const noexcept
{
    return mData >= rhs.mData;
}

bool Uuid::operator<(const Uuid& rhs) const noexcept
{
    return mData < rhs.mData;
}

bool Uuid::operator<=(const Uuid& rhs) const noexcept
{
    return mData <= rhs.mData;
}

const Uuid::Bytes& Uuid::ToBytes() const noexcept
{
    return mData;
}

std::string Uuid::ToString() const
{
    char buffer[UUID_LENGTH + 1] = {};

    const int bytesWritten = snprintf(
        buffer, sizeof(buffer),
        "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x",
        mData[0], mData[1], mData[2], mData[3], mData[4], mData[5], mData[6],
        mData[7], mData[8], mData[9], mData[10], mData[11], mData[12], mData[13],
        mData[14], mData[15]);

    assert(bytesWritten == UUID_LENGTH);

    return buffer;
}

std::string Uuid::ToHexString() const
{
    char buffer[HEX_UUID_LENGTH + 1] = {};

    const int bytesWritten = snprintf(
        buffer, sizeof(buffer),
        "%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x",
        mData[0], mData[1], mData[2], mData[3], mData[4], mData[5], mData[6],
        mData[7], mData[8], mData[9], mData[10], mData[11], mData[12], mData[13],
        mData[14], mData[15]);

    assert(bytesWritten == HEX_UUID_LENGTH);

    return buffer;
}

// GoldenRatio is a constant, that has 0 and ones uniformly distributed.
// (It is a binary representation of golden ration number)
// We need to have different constants for 32 and 64 bit architectures.
template<int S> struct GoldenRatio;

template<> struct GoldenRatio<4>
{
    enum : unsigned
    {
        Value = 0x9e3779b9U
    };
};

template<> struct GoldenRatio<8>
{
    enum : unsigned long long
    {
        Value = 0x9e3779b97f4a7c15ULL
    };
};

std::size_t Uuid::GetHash() const noexcept
{
    const std::hash<uint8_t> hasher;

    constexpr std::size_t goldenRatio = GoldenRatio<sizeof(std::size_t)>::Value;

    std::size_t seed = ~0;

    for (uint8_t byte : mData) {
        seed ^= hasher(seed) + goldenRatio + (seed << 6) + (seed >> 2);
    }

    return seed;
}
} // namespace audacity
