#include "Uuid.h"

#include <algorithm>

#ifdef USE_UUID_CREATE
#   include <rpc.h>
#endif

#ifdef USE_CFUUID
#   include <CoreFoundation/CFUUID.h>
#endif

#ifdef USE_LIBUUID
#   include <uuid/uuid.h>
#endif

#include "lib-string-utils/HexHelpers.h"

namespace audacity
{

namespace
{
bool readByte (
    Uuid::Bytes::iterator& outputIt, 
    std::string::const_iterator& inputIt, 
    const std::string::const_iterator& inputEnd)
{
    if (inputIt == inputEnd)
        return false;

    const char c1 = *inputIt++;

    if (!IsHexChar (c1))
        return false;

    if (inputIt == inputEnd)
        return false;

    const char c2 = *inputIt++;

    if (!IsHexChar (c2))
        return false;

    *outputIt = uint8_t (
        (HexCharToNum (c1) << 4) | 
        HexCharToNum (c2)
    );

    ++outputIt;

    return true;
}
}

Uuid::Uuid ()
    : Uuid (Bytes {})
{
}

Uuid::Uuid (const Bytes& data) noexcept
    : mData (data)
{
}

Uuid Uuid::Generate ()
{
#ifdef USE_UUID_CREATE
    UUID winUid;

    if (RPC_S_OK != ::UuidCreate (&winUid))
        return {};

    Uuid uuid;

    std::memcpy (uuid.mData.data (), &winUid, sizeof (winUid));

    return uuid;
#endif

#ifdef USE_CFUUID
    CFUUIDRef newId = CFUUIDCreate (NULL);
    CFUUIDBytes bytes = CFUUIDGetUUIDBytes (newId);
    CFRelease (newId);

    Uuid uuid;

    std::memcpy (uuid.mData.data (), &bytes, sizeof (bytes));

    return uuid;
#endif

#ifdef USE_LIBUUID
    uuid_t newId;

    uuid_generate (newId);

    Uuid uuid;

    std::memcpy (uuid.mData.data (), newId, sizeof (bytes));

    return uuid;
#endif
}

Uuid Uuid::FromString (const std::string& str)
{
    const size_t length = str.length ();

    if (length == 0)
        return {};

    const bool hasBraces = str[0] == '{';

    if (hasBraces && (length != 38 || str.back() != '}'))
        return {};
    else if (!hasBraces && length != 36)
        return {};

    std::string::const_iterator currentSymbol = hasBraces ? str.begin () + 1 : str.begin ();
    std::string::const_iterator inputEnd = str.end ();

    Uuid uuid;
    Bytes::iterator currentByte = uuid.mData.begin ();

    for (int i = 0; i < 16; ++i)
    {
        if (!readByte (currentByte, currentSymbol, inputEnd))
            return {};

        if (currentSymbol != inputEnd && *currentSymbol == '-')
            ++currentSymbol;
    }

    return uuid;
}

bool Uuid::isValid () const noexcept
{
    return std::any_of (mData.begin (), mData.end (), [](uint8_t c) { return c != 0; });
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

Uuid::Bytes Uuid::toBytes () const
{
    return mData;
}

std::string Uuid::toString () const
{
    char buffer[36 + 1] = {};

    snprintf (buffer, sizeof (buffer),
        "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x",
        mData[ 0], mData[ 1], mData[ 2], mData[ 3],
        mData[ 4], mData[ 5], mData[ 6], mData[ 7],
        mData[ 8], mData[ 9], mData[10], mData[11],
        mData[12], mData[13], mData[14], mData[15]
    );

    return buffer;
}

}