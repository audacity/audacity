#pragma once

#include <array>
#include <string>
#include <cstdint>

#include "UuidApi.h"

namespace audacity
{

class UUID_API Uuid final
{
public:
    using Bytes = std::array<uint8_t, 16>;

    Uuid ();
    explicit Uuid (const Bytes& data) noexcept;

    static Uuid Generate ();
    static Uuid FromString (const std::string& str);
    
    bool isValid () const noexcept;

    bool operator==(const Uuid& other) const noexcept;
    bool operator!=(const Uuid& other) const noexcept;

    bool operator>(const Uuid& other) const noexcept;
    bool operator>=(const Uuid& other) const noexcept;

    bool operator<(const Uuid& other) const noexcept;
    bool operator<=(const Uuid& other) const noexcept;

    Bytes toBytes () const;
    std::string toString () const;
private:
    Bytes mData;
};

}