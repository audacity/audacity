/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file Uuid.h
 @brief Declare a class to generate and parse UUIDs

 Dmitry Vedenko
 **********************************************************************/

#pragma once

#include <array>
#include <cstdint>
#include <string>

namespace audacity {
/*! @brief Utility class that generates and parses UUIDs.

UUIDs are generated using:
* UuidCreate on Windows,
* CFUUIDCreate on macOS,
* libuuid on other systems.
*/
class UUID_API Uuid final
{
public:
    using Bytes = std::array<uint8_t, 16>;

    //! Creates a nil UUID
    Uuid();

    //! Creates UUID from the 16 bytes long array.
    explicit Uuid(const Bytes& data) noexcept;

    //! Generate a new UUID
    static Uuid Generate();

    //! Parses an UUID from the string.
    //! This method expects the string to be in
    //! xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx or
    //! {xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx}
    //! formats.
    //! This will return nil UUID if parsing fails
    static Uuid FromString(const std::string& str);

    //! Checks, if the UUID is nil
    bool IsNil() const noexcept;

    //! Synonymous to !isNil()
    explicit operator bool() const noexcept;

    // Comparison is performed lexicographically.

    bool operator==(const Uuid& other) const noexcept;
    bool operator!=(const Uuid& other) const noexcept;

    bool operator>(const Uuid& other) const noexcept;
    bool operator>=(const Uuid& other) const noexcept;

    bool operator<(const Uuid& other) const noexcept;
    bool operator<=(const Uuid& other) const noexcept;

    //! Get the 16 bytes long array with the raw UUID data
    const Bytes& ToBytes() const noexcept;

    //! Get a string in the xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx format
    std::string ToString() const;

    //! Get a string in the xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx format
    std::string ToHexString() const;

    std::size_t GetHash() const noexcept;

private:
    Bytes mData;
};
} // namespace audacity

namespace std {
template<> struct UUID_API hash<audacity::Uuid>
{
    std::size_t operator()(const audacity::Uuid& uuid) const noexcept
    {
        return uuid.GetHash();
    }
};
} // namespace std
